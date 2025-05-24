import { createClient } from '@libsql/client'
import { config } from './config'
import { logger } from './logger'
import { TursoService } from './services/turso'
import { Database as BunDatabase } from 'bun:sqlite'
import fs from 'fs'
import path from 'path'
import { parse } from 'csv-parse'
import { pipeline } from 'stream/promises'
import fetch, { Response } from 'node-fetch'
import type { RequestInit, RequestInfo, BodyInit } from 'node-fetch'
import type { ContactCreate } from './types'

import fsPromises from 'fs/promises'
import Bun from 'bun'
import { ZIP_DATA } from './index' // Import ZIP_DATA for state lookup

// Connection pool to reuse database connections
interface ConnectionInfo {
  client: any;
  url: string;
  lastUsed: number;
}

class ConnectionPool {
  private static instance: ConnectionPool;
  private connections: Map<string, ConnectionInfo> = new Map();
  private readonly MAX_IDLE_TIME = 60000; // 60 seconds max idle time
  private readonly MAX_POOL_SIZE = 20; // Maximum connections to keep in the pool
  private cleanupInterval: any;

  private constructor() {
    // Start the cleanup interval to remove idle connections
    this.cleanupInterval = setInterval(() => this.cleanupIdleConnections(), 30000);
  }

  public static getInstance(): ConnectionPool {
    if (!ConnectionPool.instance) {
      ConnectionPool.instance = new ConnectionPool();
    }
    return ConnectionPool.instance;
  }

  public getConnection(url: string, authToken: string): any {
    // Check if we have a connection for this URL
    if (this.connections.has(url)) {
      const conn = this.connections.get(url)!;
      conn.lastUsed = Date.now();
      return conn.client;
    }

    // If we've reached max pool size, remove the oldest connection
    if (this.connections.size >= this.MAX_POOL_SIZE) {
      let oldestTime = Infinity;
      let oldestUrl = '';
      
      for (const [connUrl, conn] of this.connections.entries()) {
        if (conn.lastUsed < oldestTime) {
          oldestTime = conn.lastUsed;
          oldestUrl = connUrl;
        }
      }
      
      if (oldestUrl) {
        logger.info(`Connection pool: removing oldest connection ${oldestUrl}`);
        this.connections.delete(oldestUrl);
      }
    }

    // Create a new connection
    logger.info(`Creating new Turso connection for ${url}`);
    const client = createClient({
      url,
      authToken,
      concurrency: 25, // Lower concurrency to prevent rate limits
      fetch: async (fetchUrl: RequestInfo, options: RequestInit) => {
        // Add custom fetch with retry for 429 errors
        const maxRetries = 3;
        for (let attempt = 0; attempt < maxRetries; attempt++) {
          try {
            const response = await fetch(fetchUrl, options);
            if (response.status === 429) {
              // Rate limited, wait with exponential backoff
              const delay = Math.pow(2, attempt) * 1000;
              logger.warn(`Rate limit hit in Turso API call, retry ${attempt+1}/${maxRetries} after ${delay}ms`);
              await new Promise(resolve => setTimeout(resolve, delay));
              continue;
            }
            return response;
          } catch (error) {
            if (attempt === maxRetries - 1) throw error;
            const delay = Math.pow(2, attempt) * 1000;
            logger.warn(`Error in Turso API call, retry ${attempt+1}/${maxRetries} after ${delay}ms: ${error}`);
            await new Promise(resolve => setTimeout(resolve, delay));
          }
        }
        throw new Error('Max retries reached for Turso API call');
      }
    });

    // Store in the pool
    this.connections.set(url, {
      client,
      url,
      lastUsed: Date.now()
    });

    return client;
  }

  private cleanupIdleConnections() {
    const now = Date.now();
    let cleanedCount = 0;
    
    for (const [url, conn] of this.connections.entries()) {
      if (now - conn.lastUsed > this.MAX_IDLE_TIME) {
        this.connections.delete(url);
        cleanedCount++;
      }
    }
    
    if (cleanedCount > 0) {
      logger.info(`Connection pool: cleaned up ${cleanedCount} idle connections, remaining: ${this.connections.size}`);
    }
  }

  public shutdown() {
    clearInterval(this.cleanupInterval);
    this.connections.clear();
  }
}

type ColumnMapping = {
  firstName: string;
  lastName: string;
  email: string;
  phoneNumber: string;
  state?: string; // Make state optional since we'll infer it from zip code
  currentCarrier: string;
  effectiveDate: string;
  birthDate: string;
  tobaccoUser: string;
  gender: string;
  zipCode: string;
  planType: string;
};

type CarrierMapping = {
  detectedCarriers: string[];
  mappings: Record<string, string>;
};

interface FetchOptions extends RequestInit {
  method?: string;
  headers?: Record<string, string>;
  body?: BodyInit;
}

export class Database {
  private client: any
  private url: string
  private isLocal: boolean
  private bunDb: BunDatabase | null = null

  public static normalizeDbUrl(url: string): { hostname: string, apiUrl: string, dbUrl: string, dbName: string } {
    // Strip any protocol prefix
    const hostname = url.replace(/(^https?:\/\/)|(^libsql:\/\/)/, '');
    const dbName = hostname.split('/').pop()?.split('.')[0] || '';
    return {
      hostname,  // Raw hostname without protocol
      apiUrl: `https://${hostname}`,  // For API calls
      dbUrl: `libsql://${hostname}`,   // For database connections
      dbName // For local SQLite files
    };
  }

  constructor(dbUrl?: string, authToken?: string) {
    const url = dbUrl || config.TURSO_DATABASE_URL
    const token = authToken || config.TURSO_AUTH_TOKEN

    if (!url) {
      logger.error('Missing database URL')
      throw new Error('Missing database URL')
    }

    const { dbUrl: normalizedUrl, dbName } = Database.normalizeDbUrl(url)
    this.url = normalizedUrl
    this.isLocal = config.USE_LOCAL_SQLITE

    if (this.isLocal) {
      const dbPath = path.join(process.cwd(), config.LOCAL_DB_PATH, `${dbName}.sqlite`)
      logger.info(`Using local SQLite database at: ${dbPath}`)
      
      // Create directory if it doesn't exist
      const dbDir = path.dirname(dbPath)
      if (!fs.existsSync(dbDir)) {
        fs.mkdirSync(dbDir, { recursive: true })
      }
      
      this.bunDb = new BunDatabase(dbPath)
      this.client = this.bunDb
      
      // Enable foreign keys
      this.bunDb.exec('PRAGMA foreign_keys = ON;')
    } else {
      if (!token) {
        logger.error('Missing database token')
        throw new Error('Missing database token')
      }
      this.client = createClient({
        url: normalizedUrl,
        authToken: token,
        concurrency: 25, // Reduced concurrency to prevent rate limits
        fetch: async (fetchUrl: RequestInfo, options: RequestInit) => {
          // Add custom fetch with retry for 429 errors
          const maxRetries = 3;
          for (let attempt = 0; attempt < maxRetries; attempt++) {
            try {
              const response = await fetch(fetchUrl, options);
              if (response.status === 429) {
                // Rate limited, wait with exponential backoff
                const delay = Math.pow(2, attempt) * 1000;
                logger.warn(`Rate limit hit in Turso API call, retry ${attempt+1}/${maxRetries} after ${delay}ms`);
                await new Promise(resolve => setTimeout(resolve, delay));
                continue;
              }
              return response;
            } catch (error) {
              if (attempt === maxRetries - 1) throw error;
              const delay = Math.pow(2, attempt) * 1000;
              logger.warn(`Error in Turso API call, retry ${attempt+1}/${maxRetries} after ${delay}ms: ${error}`);
              await new Promise(resolve => setTimeout(resolve, delay));
            }
          }
          throw new Error('Max retries reached for Turso API call');
        }
      })
    }
    
    logger.info(`Database connected to: ${this.isLocal ? dbName : this.url}`)
  }

  static async getOrgDb(orgId: string): Promise<Database> {
    logger.info(`Getting org database for org ${orgId}`);
    const mainDb = new Database();
    
    try {
      const org = await mainDb.fetchOne<{ turso_db_url: string; turso_auth_token: string }>(
        'SELECT turso_db_url, turso_auth_token FROM organizations WHERE id = ?',
        [orgId]
      );
      logger.info(`[OrgDB] Organization record: ${JSON.stringify(org)}`);

      if (!org) {
        logger.warn(`[OrgDB] Organization record not found for orgId: ${orgId}`);
        throw new Error('Organization database not configured');
      }
      if (!org.turso_db_url) {
        logger.warn(`[OrgDB] No turso_db_url found in organization record for orgId: ${orgId}. Record: ${JSON.stringify(org)}`);
        throw new Error('Organization database not configured');
      }
      if (!org.turso_auth_token) {
        logger.warn(`[OrgDB] No turso_auth_token found in organization record for orgId: ${orgId}. Record: ${JSON.stringify(org)}`);
        // Depending on policy, you might still proceed or throw an error.
        // For now, let's assume a URL without a token is also a configuration issue.
        throw new Error('Organization database not configured (missing token)');
      }

      logger.info(`[OrgDB] Found credentials for org ${orgId}. URL: ${org.turso_db_url.substring(0, 20)}... Token: ${org.turso_auth_token ? 'present' : 'MISSING'}`);
      const db = new Database(org.turso_db_url, org.turso_auth_token);

      // Validate connection by running a simple query with timeout
      logger.info(`[OrgDB] Validating database connection for org ${orgId}...`);
      try {
        const timeoutPromise = new Promise((_, reject) => {
          setTimeout(() => reject(new Error('Database validation timed out after 5 seconds')), 5000);
        });
        
        const queryPromise = db.execute('SELECT 1');
        
        const result = await Promise.race([queryPromise, timeoutPromise]);
        logger.info(`Database connection validation successful for org ${orgId}. Result: ${JSON.stringify(result)}`);
        return db;
      } catch (error) {
        const errorMessage = error instanceof Error ? error.message : String(error);
        logger.error(`Database connection validation failed for org ${orgId}. Error: ${errorMessage}`);
        if (error instanceof Error && error.stack) {
          logger.error(`Stack trace: ${error.stack}`);
        }
        throw new Error(`Failed to establish database connection: ${errorMessage}`);
      }
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : String(error);
      logger.error(`Error getting org database for org ${orgId}: ${errorMessage}`);
      if (error instanceof Error && error.stack) {
        logger.error(`Stack trace: ${error.stack}`);
      }
      throw error;
    }
  }

  /**
   * Get organization's database or initialize it if it doesn't exist
   * This method is used as a fallback when the database needs to be created on the fly
   */
  static async getOrInitOrgDb(orgId: string): Promise<Database> {
    const mainDb = new Database();
    const MAX_RETRIES = 10;
    const RETRY_DELAY_MS = 500;
    let retries = 0;

    logger.info(`[GetOrInitOrgDb] Entered for orgId: ${orgId}`);

    while (true) {
      try {
        // Try to get the org DB as usual
        logger.info(`[GetOrInitOrgDb] Attempting to get OrgDB (attempt ${retries + 1}) for orgId: ${orgId}`);
        const db = await Database.getOrgDb(orgId)
        logger.info(`[GetOrInitOrgDb] Successfully got OrgDB for orgId: ${orgId}. Ensuring schema.`);
        await Database.ensureOrgSchema(db)
        logger.info(`[GetOrInitOrgDb] Schema ensured for orgId: ${orgId}. Returning DB.`);
        return db
      } catch (error) {
        logger.warn(`[GetOrInitOrgDb] Failed to get OrgDB for orgId: ${orgId}. Error: ${error instanceof Error ? error.message : String(error)}`);
        if (error instanceof Error && (error.message === 'Organization database not configured' || error.message === 'Organization database not configured (missing token)')) {
          logger.info(`[GetOrInitOrgDb] Database not configured for orgId: ${orgId}. Attempting to provision.`);
          // Try to atomically claim the right to provision the DB
          logger.info(`[GetOrInitOrgDb] Attempting to claim provisioning lock for orgId: ${orgId}`);
          const claimResult = await mainDb.execute(
            `UPDATE organizations
             SET is_db_provisioning = 1
             WHERE id = ? AND is_db_provisioning = 0 AND (turso_db_url IS NULL OR turso_db_url = '' OR turso_auth_token IS NULL OR turso_auth_token = '')`,
            [orgId]
          );

          logger.info(`[GetOrInitOrgDb] Claim lock result for orgId: ${orgId}: rowsAffected = ${claimResult.rowsAffected}`);
          if (claimResult.rowsAffected === 0) {
            // Someone else is provisioning, wait and retry
            logger.warn(`[GetOrInitOrgDb] Failed to claim lock for orgId: ${orgId} (possibly locked or already provisioned). Retrying...`);
            if (retries++ >= MAX_RETRIES) {
              logger.error(`[GetOrInitOrgDb] Timed out waiting for organization DB provisioning for orgId: ${orgId} after ${MAX_RETRIES} retries.`);
              throw new Error('Timed out waiting for organization DB provisioning');
            }
            await new Promise(res => setTimeout(res, RETRY_DELAY_MS));
            continue;
          }

          // We have the lock, proceed to provision
          logger.info(`[GetOrInitOrgDb] Successfully claimed provisioning lock for orgId: ${orgId}. Proceeding with provisioning.`);
          try {
            const orgExists = await mainDb.fetchOne<{ id: number }>(
              'SELECT id FROM organizations WHERE id = ?',
              [orgId]
            )
            if (!orgExists) {
              logger.error(`[GetOrInitOrgDb] Organization ${orgId} not found during provisioning.`);
              throw new Error('Organization not found')
            }

            const turso = new TursoService()
            logger.info(`[GetOrInitOrgDb] Creating new Turso database for org ${orgId} via TursoService.`);
            const { url, token } = await turso.createOrganizationDatabase(orgId)
            logger.info(`[GetOrInitOrgDb] TursoService returned new database URL: ${url} and token (length: ${token.length}) for org ${orgId}.`);

            // Verify we can connect with the new credentials
            try {
              logger.info(`[GetOrInitOrgDb] Verifying connection with new credentials for org ${orgId}...`)
              const testDb = new Database(url, token)
              await testDb.execute('SELECT 1')
              logger.info(`[GetOrInitOrgDb] Successfully verified connection with new credentials for org ${orgId}.`)
            } catch (connError) {
              logger.error(`[GetOrInitOrgDb] Failed to verify connection with new credentials for org ${orgId}: ${connError instanceof Error ? connError.message : String(connError)}`)
              throw new Error('Failed to verify connection with new database credentials')
            }

            // Update organization with new credentials and clear provisioning flag
            logger.info(`[GetOrInitOrgDb] Updating organization ${orgId} in main DB with new Turso credentials.`);
            await mainDb.execute(
              'UPDATE organizations SET turso_db_url = ?, turso_auth_token = ?, is_db_provisioning = 0 WHERE id = ?',
              [url, token, orgId]
            )
            logger.info(`[GetOrInitOrgDb] Successfully updated organization ${orgId} with new credentials. Clearing provisioning flag.`);

            // Verify the update
            const updatedOrg = await mainDb.fetchOne<{ turso_db_url: string, turso_auth_token: string }>(
              'SELECT turso_db_url, turso_auth_token FROM organizations WHERE id = ?',
              [orgId]
            )
            if (!updatedOrg) {
              logger.error('[GetOrInitOrgDb] Failed to fetch updated organization after credential update for org ${orgId}.')
              throw new Error('Failed to update organization credentials')
            }
            if (updatedOrg.turso_db_url !== url || updatedOrg.turso_auth_token !== token) {
              logger.error(`[GetOrInitOrgDb] Organization credentials mismatch after update for org ${orgId}.`);
              logger.error(`Expected URL: ${url}, got: ${updatedOrg.turso_db_url}`);
              logger.error(`Expected token length: ${token.length}, got: ${updatedOrg.turso_auth_token.length}`);
              throw new Error('Organization credentials mismatch after update')
            }

            logger.info(`[GetOrInitOrgDb] Successfully initialized and verified database for organization ${orgId}. URL: ${url}`);
            const newDb = new Database(url, token)
            logger.info(`[GetOrInitOrgDb] Ensuring schema for newly provisioned DB for org ${orgId}.`);
            await Database.ensureOrgSchema(newDb)
            logger.info(`[GetOrInitOrgDb] Schema ensured for newly provisioned DB for org ${orgId}. Returning DB.`);
            return newDb
          } catch (provisionError) {
            logger.error(`[GetOrInitOrgDb] Error during provisioning for orgId: ${orgId}. Error: ${provisionError instanceof Error ? provisionError.message : String(provisionError)}`);
            // On error, clear the provisioning flag so future attempts can retry
            logger.info(`[GetOrInitOrgDb] Clearing provisioning lock for orgId: ${orgId} due to provisioning error.`);
            await mainDb.execute(
              'UPDATE organizations SET is_db_provisioning = 0 WHERE id = ?',
              [orgId]
            )
            throw provisionError
          }
        }
        logger.error(`[GetOrInitOrgDb] Unhandled error in getOrInitOrgDb for orgId ${orgId}: ${error instanceof Error ? error.message : String(error)}`)
        if (error instanceof Error && error.stack) {
          logger.error(`Stack trace: ${error.stack}`)
        }
        throw error
      }
    }
  }

  /**
   * Ensure that the organization database has all required tables
   * This can be used to add new tables to existing databases when the schema changes
   */
  static async ensureDatabaseSchema(orgId: string): Promise<void> {
    const orgDb = await Database.getOrgDb(orgId);
    
    // Define tables and their schema
    const tables = [
      {
        name: 'contacts',
        createStatement: `
          CREATE TABLE IF NOT EXISTS contacts (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            first_name TEXT NOT NULL,
            last_name TEXT NOT NULL,
            email TEXT NOT NULL,
            current_carrier TEXT NOT NULL,
            plan_type TEXT NOT NULL,
            effective_date TEXT NOT NULL,
            birth_date TEXT NOT NULL,
            tobacco_user INTEGER NOT NULL,
            gender TEXT NOT NULL,
            state TEXT NOT NULL,
            zip_code TEXT NOT NULL,
            agent_id INTEGER,
            last_emailed DATETIME,
            phone_number TEXT NOT NULL DEFAULT '',
            status TEXT NOT NULL DEFAULT '',
            aep_request BOOLEAN DEFAULT FALSE,
            aep_request_date DATETIME,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
        )`,
        indexStatements: [
          `CREATE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email)`,
          `CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email_unique ON contacts(LOWER(TRIM(email)))`,
        ],
      },
      {
        name: 'eligibility_answers',
        createStatement: `CREATE TABLE IF NOT EXISTS eligibility_answers (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          contact_id INTEGER NOT NULL,
          quote_id TEXT NOT NULL,
          answers TEXT NOT NULL,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
          FOREIGN KEY (contact_id) REFERENCES contacts(id)
        )`,
        indexStatements: [
          `CREATE INDEX IF NOT EXISTS idx_eligibility_answers_contact_id ON eligibility_answers(contact_id)`
        ]
      },
      {
        name: 'contact_events',
        createStatement: `CREATE TABLE IF NOT EXISTS contact_events (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          contact_id INTEGER,
          lead_id INTEGER,
          event_type TEXT NOT NULL,
          metadata TEXT,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
          FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE,
          FOREIGN KEY (lead_id) REFERENCES leads(id) ON DELETE CASCADE
        )`,
        indexStatements: [
          `CREATE INDEX IF NOT EXISTS idx_contact_events_contact_id ON contact_events(contact_id)`,
          `CREATE INDEX IF NOT EXISTS idx_contact_events_lead_id ON contact_events(lead_id)`,
          `CREATE INDEX IF NOT EXISTS idx_contact_events_type ON contact_events(event_type)`
        ]
      },
      {
        name: 'tracking_clicks',
        createStatement: `CREATE TABLE IF NOT EXISTS tracking_clicks (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          tracking_id TEXT NOT NULL,
          path TEXT NOT NULL,
          query TEXT,
          contact_id INTEGER,
          ip_address TEXT,
          user_agent TEXT,
          referrer TEXT,
          clicked_at DATETIME DEFAULT CURRENT_TIMESTAMP,
          FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
        )`,
        indexStatements: [
          `CREATE INDEX IF NOT EXISTS idx_tracking_clicks_tracking_id ON tracking_clicks(tracking_id)`,
          `CREATE INDEX IF NOT EXISTS idx_tracking_clicks_contact_id ON tracking_clicks(contact_id)`,
          `CREATE INDEX IF NOT EXISTS idx_tracking_clicks_clicked_at ON tracking_clicks(clicked_at)`
        ]
      },
      {
        name: 'leads',
        createStatement: `CREATE TABLE IF NOT EXISTS leads (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          name TEXT NOT NULL,
          email TEXT NOT NULL,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
          UNIQUE(email)
        )`,
        indexStatements: [
          `CREATE INDEX IF NOT EXISTS idx_leads_email ON leads(email)`
        ]
      },
      {
        name: 'email_send_tracking',
        createStatement: `CREATE TABLE IF NOT EXISTS email_send_tracking (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          org_id INTEGER NOT NULL,
          contact_id INTEGER NOT NULL,
          email_type TEXT NOT NULL,
          scheduled_date TEXT NOT NULL,
          send_status TEXT NOT NULL CHECK(send_status IN ('pending', 'processing', 'accepted', 'delivered', 'sent', 'deferred', 'bounced', 'dropped', 'failed', 'skipped')) DEFAULT 'pending',
          send_mode TEXT NOT NULL CHECK(send_mode IN ('test', 'production')) DEFAULT 'test',
          test_email TEXT,
          send_attempt_count INTEGER NOT NULL DEFAULT 0,
          last_attempt_date TEXT,
          last_error TEXT,
          batch_id TEXT NOT NULL,
          message_id TEXT,
          delivery_status TEXT,
          status_checked_at TEXT,
          status_details TEXT,
          created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
          updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
          FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
        )`,
        indexStatements: [
          `CREATE INDEX IF NOT EXISTS idx_email_tracking_batch_id ON email_send_tracking(batch_id)`,
          `CREATE INDEX IF NOT EXISTS idx_email_tracking_send_status ON email_send_tracking(send_status)`,
          `CREATE INDEX IF NOT EXISTS idx_email_tracking_send_mode ON email_send_tracking(send_mode)`,
          `CREATE INDEX IF NOT EXISTS idx_email_tracking_contact_id ON email_send_tracking(contact_id)`,
          `CREATE INDEX IF NOT EXISTS idx_email_tracking_contact_type ON email_send_tracking(contact_id, email_type)`,
          `CREATE INDEX IF NOT EXISTS idx_email_tracking_status_date ON email_send_tracking(send_status, scheduled_date)`,
          `CREATE INDEX IF NOT EXISTS idx_email_tracking_message_id ON email_send_tracking(message_id)`,
          `CREATE INDEX IF NOT EXISTS idx_email_tracking_delivery_status ON email_send_tracking(delivery_status)`,
          `CREATE TRIGGER IF NOT EXISTS update_email_tracking_timestamp AFTER UPDATE ON email_send_tracking BEGIN UPDATE email_send_tracking SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id; END`
        ]
      },
      {
        name: 'email_schedules',
        createStatement: `
          CREATE TABLE IF NOT EXISTS email_schedules (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            org_id INTEGER NOT NULL,
            contact_id INTEGER NOT NULL,
            email_type TEXT NOT NULL,
            scheduled_send_date TEXT NOT NULL,
            scheduled_send_time TEXT NOT NULL DEFAULT '08:30:00',
            batch_id TEXT,
            status TEXT NOT NULL DEFAULT 'pre-scheduled',
            skip_reason TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
          )
        `,
        indexStatements: [
          `CREATE INDEX IF NOT EXISTS idx_email_schedules_org_contact ON email_schedules (contact_id)`,
          `CREATE INDEX IF NOT EXISTS idx_email_schedules_org_send_date ON email_schedules (scheduled_send_date)`,
          `CREATE INDEX IF NOT EXISTS idx_email_schedules_status ON email_schedules (status)`,
          `CREATE TRIGGER IF NOT EXISTS update_email_schedules_updated_at
AFTER UPDATE ON email_schedules
FOR EACH ROW
BEGIN
    UPDATE email_schedules
    SET updated_at = CURRENT_TIMESTAMP
    WHERE id = OLD.id;
END;`
        ]
      },
      {
        name: 'deleted_contacts',
        createSql: `
          CREATE TABLE IF NOT EXISTS deleted_contacts (
              original_contact_id INTEGER NOT NULL, -- The ID from the original 'contacts' table
              first_name TEXT,
              last_name TEXT,
              email TEXT NOT NULL,        -- Must be normalized: lowercase, trimmed
              phone_number TEXT,
              current_carrier TEXT,       -- Optional: Include other relevant fields
              plan_type TEXT,             -- Optional
              effective_date TEXT,        -- Optional
              birth_date TEXT,            -- Optional
              tobacco_user INTEGER,       -- Optional
              gender TEXT,                -- Optional
              state TEXT,                 -- Optional
              zip_code TEXT,              -- Optional
              agent_id INTEGER,           -- Optional
              status TEXT,                -- Optional: Original status before deletion
              deleted_at DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL
          )
        `,
        indexSqls: [
          `CREATE INDEX IF NOT EXISTS idx_deleted_contacts_email_org ON deleted_contacts (LOWER(TRIM(email)))`,
          `CREATE INDEX IF NOT EXISTS idx_deleted_contacts_deleted_at ON deleted_contacts (deleted_at)`,
          `CREATE INDEX IF NOT EXISTS idx_deleted_contacts_original_id ON deleted_contacts (original_contact_id)`
        ]
      }
    ];
    
    // Get all existing tables in one query for efficiency
    const existingTables = await orgDb.fetchAll(
      `SELECT name FROM sqlite_master WHERE type='table' AND name IN (${tables.map(t => `'${t.name}'`).join(', ')})`
    );
    
    // Create a set of existing table names for faster lookup
    const tableSet = new Set(existingTables.map((row: any) => row.name || row[0]));
    
    // Prepare batch operations
    const batchOperations = [];
    
    for (const table of tables) {
      if (!tableSet.has(table.name)) {
        logger.info(`Adding create table operation for ${table.name}`);
        if (table.createStatement) {
          batchOperations.push({
            sql: table.createStatement,
            args: []
          });
        }
      }
      
      // Always add index creation statements, even for existing tables
      // This ensures all necessary indexes exist in all databases
      if (table.indexStatements) {
        for (const indexStatement of table.indexStatements) {
          logger.info(`Ensuring index for ${table.name}: ${indexStatement}`);
          batchOperations.push({
            sql: indexStatement,
            args: []
          });
        }
      }
    }
    
    // Execute all schema operations in a single batch if there are any
    if (batchOperations.length > 0) {
      logger.info(`Executing ${batchOperations.length} schema operations in batch for org ${orgId}`);
      await orgDb.batch(batchOperations, 'write');
      logger.info(`Schema setup completed successfully for org ${orgId}`);
    } else {
      logger.info(`No schema changes needed for org ${orgId}`);
    }
  }

  getClient() {
    return this.client
  }

  async execute(sql: string, args: any[] = []) {
    try {
      if (this.isLocal && this.bunDb) {
        // For local SQLite
        const stmt = this.bunDb.prepare(sql)
        const result = stmt.run(...args)
        return {
          rows: Array.isArray(result) ? result : result.changes > 0 ? [result] : [],
          rowsAffected: result.changes
        }
      } else {
        // For Turso
        const result = await this.client.execute({
          sql,
          args
        })
        return result
      }
    } catch (error) {
      logger.error(`Database execute error: ${error}`)
      throw error
    }
  }
  
  /**
   * Execute a batch of SQL statements in an implicit transaction
   * @param statements Array of SQL statements with args
   * @param mode Transaction mode (read or write)
   * @returns Result of the batch operation
   */
  async batch(statements: { sql: string, args: any[] }[], mode: 'read' | 'write' = 'write') {
    try {
      if (this.isLocal && this.bunDb) {
        // For local SQLite, implement batch manually with transaction
        this.bunDb.exec('BEGIN TRANSACTION');
        const results = [];
        
        try {
          for (const { sql, args } of statements) {
            const stmt = this.bunDb.prepare(sql);
            const result = stmt.run(...args);
            results.push({
              rows: Array.isArray(result) ? result : result.changes > 0 ? [result] : [],
              rowsAffected: result.changes
            });
          }
          
          this.bunDb.exec('COMMIT');
          return results;
        } catch (error) {
          this.bunDb.exec('ROLLBACK');
          throw error;
        }
      } else {
        // For Turso, use native batch support
        const batchStatements = statements.map(({ sql, args }) => ({
          sql,
          args: args || []
        }));
        
        return await this.client.batch(batchStatements, mode);
      }
    } catch (error) {
      logger.error(`Database batch error: ${error}`);
      throw error;
    }
  }

  async fetchAll(sql: string, args: any[] = []) {
    try {
      if (this.isLocal && this.bunDb) {
        // For local SQLite
        const stmt = this.bunDb.prepare(sql)
        const rows = stmt.all(...args)
        return rows || []
      } else {
        // For Turso
        const result = await this.client.execute({
          sql,
          args
        })
        return result.rows || []
      }
    } catch (error) {
      logger.error(`Database fetchAll error: ${error}`)
      throw error
    }
  }

  async fetchOne<T>(sql: string, args: any[] = []): Promise<T | null> {
    if (this.isLocal && this.bunDb) {
      // For local SQLite
      const stmt = this.bunDb.prepare(sql)
      const row = stmt.get(...args)
      return row as T || null
    } else {
      // For Turso
      const result = await this.execute(sql, args)
      if (!result.rows || result.rows.length === 0) return null
      const row = result.rows[0]
      const columns = result.columns || []
      const obj: any = {}
      columns.forEach((col: string, i: number) => (obj[col] = row[i]))
      return obj as T
    }
  }

  // Compatibility method for old query interface
  async query<T = any>(sql: string, args: any[] = []): Promise<T[]> {
    return this.fetchAll(sql, args)
  }

  // Add transaction support for local SQLite
  async transaction<T>(callback: (tx: Database) => Promise<T>): Promise<T>;
  async transaction<T>(mode: 'read' | 'write', callback: (tx: Database) => Promise<T>): Promise<T>;
  async transaction<T>(
    callbackOrMode: ((tx: Database) => Promise<T>) | 'read' | 'write',
    callback?: (tx: Database) => Promise<T>
  ): Promise<T> {
    let mode: 'read' | 'write' = 'write'
    let fn: ((tx: Database) => Promise<T>) | null = null

    if (typeof callbackOrMode === 'string') {
      mode = callbackOrMode
      fn = callback || null
    } else {
      fn = callbackOrMode
    }
    
    if (!fn) {
      throw new Error('Transaction callback is required')
    }

    if (this.isLocal && this.bunDb) {
      // For local SQLite
      try {
        this.bunDb.exec('BEGIN TRANSACTION')
        const result = await fn(this)
        this.bunDb.exec('COMMIT')
        return result
      } catch (error) {
        this.bunDb.exec('ROLLBACK')
        throw error
      }
    } else {
      // For Turso
      const tx = await this.client.transaction(mode)
      try {
        const txWrapper = new Database()
        txWrapper.client = tx
        const result = await fn(txWrapper)
        await tx.commit()
        return result
      } catch (error) {
        await tx.rollback()
        throw error
      }
    }
  }

  /**
   * Bulk import contacts from CSV directly into the database
   */
  static async bulkImportContacts(
    orgId: string,
    csvFilePath: string,
    overwriteExisting: boolean = false,
    columnMapping?: ColumnMapping,
    carrierMapping?: CarrierMapping,
    agentId?: number | null
  ): Promise<string> {
    logger.info(`Starting bulk import for organization ${orgId} from ${csvFilePath}`);
    
    try {
      // get the main db
      const mainDb = new Database();
      
      // get the org db url / auth token
      const orgData = await mainDb.fetchOne<{ turso_db_url: string, turso_auth_token: string }>(
        'SELECT turso_db_url, turso_auth_token FROM organizations WHERE id = ?',
        [orgId]
      );

      if (!orgData) {
        throw new Error(`Organization ${orgId} not found`);
      }

      const { turso_db_url, turso_auth_token } = orgData;

      if (!turso_db_url || !turso_auth_token) {
        throw new Error(`Could not get database configuration for organization ${orgId}`);
      }

      // First, download the existing database from Turso
      const tursoService = new TursoService();
      logger.info(`Downloading existing database from Turso for org ${orgId}`);
      logger.info(`Turso DB URL: ${turso_db_url}`);
      logger.info(`Turso Auth Token: ${turso_auth_token}`);
      const dumpContent = await tursoService.downloadDatabaseDump(turso_db_url, turso_auth_token);
      logger.info(`Downloaded ${dumpContent.length} bytes of database dump`);

      // Create a temporary file for the dump
      const tempDumpFile = `dump-${Date.now()}.sql`;
      const tempDbFile = `temp-${Date.now()}.db`;
      let localDb: BunDatabase | null = null;
      
      try {
        // Write dump to temporary file
        await fsPromises.writeFile(tempDumpFile, dumpContent);

        // Use sqlite3 CLI to create and populate the database
        logger.info('Creating temporary database from dump...');
        await new Promise((resolve, reject) => {
          const sqlite = Bun.spawn(['sqlite3', tempDbFile], {
            stdin: Bun.file(tempDumpFile),
            onExit(proc, exitCode, signalCode, error) {
              if (exitCode === 0) {
                resolve(null);
              } else {
                reject(new Error(`SQLite process exited with code ${exitCode}: ${error}`));
              }
            }
          });
        });

        logger.info('Successfully created temporary database from dump');

        // Now connect to the temporary database using BunSQLite
        localDb = new BunDatabase(tempDbFile);

        // Use DELETE journal mode instead of WAL for direct file writes
        localDb.exec('PRAGMA journal_mode = DELETE');
        localDb.exec('PRAGMA foreign_keys = OFF');

        // Drop the unique index on email temporarily to allow the import
        logger.info('Dropping unique email index for import...');
        localDb.exec('DROP INDEX IF EXISTS idx_contacts_email_unique');

        // Verify the database state before CSV import
        const tables = ['contacts', 'contact_events', 'leads', 'eligibility_answers', 'email_send_tracking'];
        for (const table of tables) {
          try {
            const count = localDb.prepare(`SELECT COUNT(*) as count FROM ${table}`).get() as { count: number };
            logger.info(`Table ${table} before CSV import: ${count.count} rows`);
          } catch (error) {
            logger.error(`Error counting rows in ${table}: ${error}`);
          }
        }

        // Now process the new contacts from CSV
        logger.info(`Processing new contacts from CSV file`);
        
        // Read the CSV file
        const fileContents = await fsPromises.readFile(csvFilePath, 'utf8');
        const rows = await new Promise<any[]>((resolve, reject) => {
          const results: any[] = [];
          const parser = parse(fileContents, { columns: true });
          
          parser.on('readable', function() {
            let record;
            while ((record = parser.read()) !== null) {
              results.push(record);
            }
          });
          
          parser.on('error', function(err) {
            reject(err);
          });
          
          parser.on('end', function() {
            resolve(results);
          });
        });
        
        logger.info(`Processing ${rows.length} rows from CSV`);
        
        // Find the email column (required for deduplication)
        const emailColumn = columnMapping ? columnMapping.email : 'email';
        if (!rows[0]?.[emailColumn]) {
          throw new Error(`Email column "${emailColumn}" not found in CSV`);
        }
        
        // Map columns based on provided mapping or use default field names
        const processRow = (row: any) => {
          const mappedRow: any = {};
          
          // Apply column mappings if provided
          if (columnMapping) {
            // Map each field using the provided column mapping
            mappedRow.first_name = row[columnMapping.firstName] || '';
            mappedRow.last_name = row[columnMapping.lastName] || '';
            mappedRow.email = row[columnMapping.email] || '';
            mappedRow.phone_number = row[columnMapping.phoneNumber] || '';
            mappedRow.zip_code = row[columnMapping.zipCode] || '';
            
            // Infer state from zip code
            if (mappedRow.zip_code && ZIP_DATA[mappedRow.zip_code]) {
              mappedRow.state = ZIP_DATA[mappedRow.zip_code].state;
              logger.info(`Inferred state ${mappedRow.state} from zip code ${mappedRow.zip_code}`);
            } else if (columnMapping.state && row[columnMapping.state]) {
              // Fallback to provided state if zip code lookup fails
              mappedRow.state = row[columnMapping.state] || '';
              logger.info(`Using provided state ${mappedRow.state} (zip code lookup failed)`);
            } else {
              logger.warn(`No state found for zip code ${mappedRow.zip_code}, defaulting to empty string`);
              mappedRow.state = '';
            }
            
            // Apply carrier mapping if provided
            let carrierValue = '';
            if (columnMapping.currentCarrier && row[columnMapping.currentCarrier]) {
              const originalCarrier = row[columnMapping.currentCarrier];
              
              // Use carrier mapping if available
              if (carrierMapping && carrierMapping.mappings[originalCarrier]) {
                carrierValue = carrierMapping.mappings[originalCarrier];
                
                // If mapped to "Other", preserve original value
                if (carrierValue === 'Other') {
                  carrierValue = originalCarrier;
                }
              } else {
                carrierValue = originalCarrier;
              }
            }
            
            mappedRow.current_carrier = carrierValue;
            mappedRow.effective_date = row[columnMapping.effectiveDate] || '';
            mappedRow.birth_date = row[columnMapping.birthDate] || '';
            mappedRow.tobacco_user = row[columnMapping.tobaccoUser] === 'true' || row[columnMapping.tobaccoUser] === 'yes' || row[columnMapping.tobaccoUser] === '1';
            mappedRow.gender = row[columnMapping.gender] || '';
            mappedRow.plan_type = row[columnMapping.planType] || '';
          } else {
            // Use default field names if no mapping provided
            mappedRow.first_name = row.first_name || row.firstName || '';
            mappedRow.last_name = row.last_name || row.lastName || '';
            mappedRow.email = row.email || '';
            mappedRow.phone_number = row.phone_number || row.phoneNumber || '';
            mappedRow.zip_code = row.zip_code || row.zipCode || '';
            
            // Infer state from zip code
            if (mappedRow.zip_code && ZIP_DATA[mappedRow.zip_code]) {
              mappedRow.state = ZIP_DATA[mappedRow.zip_code].state;
              logger.info(`Inferred state ${mappedRow.state} from zip code ${mappedRow.zip_code}`);
            } else if (row.state) {
              // Fallback to provided state if zip code lookup fails
              mappedRow.state = row.state || '';
              logger.info(`Using provided state ${mappedRow.state} (zip code lookup failed)`);
            } else {
              logger.warn(`No state found for zip code ${mappedRow.zip_code}, defaulting to empty string`);
              mappedRow.state = '';
            }
            
            mappedRow.current_carrier = row.current_carrier || row.currentCarrier || '';
            mappedRow.effective_date = row.effective_date || row.effectiveDate || '';
            mappedRow.birth_date = row.birth_date || row.birthDate || '';
            mappedRow.tobacco_user = row.tobacco_user === 'true' || row.tobacco_user === 'yes' || row.tobacco_user === '1' ||
                                   row.tobaccoUser === 'true' || row.tobaccoUser === 'yes' || row.tobaccoUser === '1';
            mappedRow.gender = row.gender || '';
            mappedRow.plan_type = row.plan_type || row.planType || '';
          }
          
          // Add standard fields
          mappedRow.created_at = new Date().toISOString();
          mappedRow.updated_at = new Date().toISOString();
          
          // Add agent_id if provided
          if (agentId !== undefined && agentId !== null) {
            mappedRow.agent_id = agentId;
          }
          
          return mappedRow;
        };
        
        // Begin transaction for CSV import
        localDb.exec('BEGIN TRANSACTION');
        
        try {
          // First, get all existing emails (for manual deduplication)
          const existingEmails = new Set(
            localDb.prepare('SELECT LOWER(TRIM(email)) as email FROM contacts')
              .all()
              .map((row: any) => row.email || row[0])
          );

          logger.info(`Found ${existingEmails.size} existing emails in database`);

          // Prepare insert statement (without ON CONFLICT since we're handling it manually)
          const stmt = localDb.prepare(`
            INSERT INTO contacts (
              first_name, last_name, email, phone_number, state,
              current_carrier, effective_date, birth_date, tobacco_user,
              gender, zip_code, plan_type, agent_id, created_at, updated_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
          `);

          let totalProcessed = 0;
          let totalAdded = 0;
          let totalSkipped = 0;

          // Process each row
          for (const row of rows) {
            totalProcessed++;
            const processedRow = processRow(row);
            const email = processedRow.email.toLowerCase().trim();
            
            if (!email) {
              logger.warn('Skipping row with no email address');
              totalSkipped++;
              continue;
            }

            try {
              if (existingEmails.has(email)) {
                if (overwriteExisting) {
                  // Update existing record
                  localDb.prepare(`
                    UPDATE contacts SET
                      first_name = ?, last_name = ?, phone_number = ?, state = ?,
                      current_carrier = ?, effective_date = ?, birth_date = ?,
                      tobacco_user = ?, gender = ?, zip_code = ?, plan_type = ?,
                      agent_id = ?, updated_at = ?
                    WHERE LOWER(TRIM(email)) = LOWER(TRIM(?))
                  `).run(
                    processedRow.first_name,
                    processedRow.last_name,
                    processedRow.phone_number,
                    processedRow.state,
                    processedRow.current_carrier,
                    processedRow.effective_date,
                    processedRow.birth_date,
                    processedRow.tobacco_user ? 1 : 0,
                    processedRow.gender,
                    processedRow.zip_code,
                    processedRow.plan_type,
                    processedRow.agent_id || null,
                    processedRow.updated_at,
                    email
                  );
                  totalAdded++;
                } else {
                  totalSkipped++;
                }
              } else {
                stmt.run(
                  processedRow.first_name,
                  processedRow.last_name,
                  email,
                  processedRow.phone_number,
                  processedRow.state,
                  processedRow.current_carrier,
                  processedRow.effective_date,
                  processedRow.birth_date,
                  processedRow.tobacco_user ? 1 : 0,
                  processedRow.gender,
                  processedRow.zip_code,
                  processedRow.plan_type,
                  processedRow.agent_id || null,
                  processedRow.created_at,
                  processedRow.updated_at
                );
                existingEmails.add(email);
                totalAdded++;
              }
            } catch (err) {
              logger.error(`Error processing row with email ${email}: ${err}`);
              totalSkipped++;
            }
          }

          // Recreate the unique index after import
          logger.info('Recreating unique email index...');
          localDb.exec('CREATE UNIQUE INDEX idx_contacts_email_unique ON contacts(LOWER(TRIM(email)))');

          // Commit CSV import transaction
          localDb.exec('COMMIT');
          
          // Force a checkpoint to ensure all changes are written to disk
          localDb.exec('PRAGMA wal_checkpoint(TRUNCATE)');
          
          logger.info(`Successfully processed all rows from CSV`);
          logger.info(`Total processed: ${totalProcessed}, added: ${totalAdded}, skipped: ${totalSkipped}`);

          // Verify final counts
          const finalCount = localDb.prepare('SELECT COUNT(*) as count FROM contacts').get() as { count: number };
          logger.info(`Final contact count in database: ${finalCount.count} (should be ${existingEmails.size} + new additions)`);

          // Close the database to ensure all changes are flushed
          localDb.close();
          localDb = null;

          try {
            // Create new database and upload data
            logger.info(`Creating new Turso database for import`);
            const { dbName: newOrgDbName, url: newOrgDbUrl, token: newOrgDbToken } = await tursoService.createDatabaseForImport(orgId);
            
            // Upload the local db to the new org db
            logger.info(`Uploading data to new Turso database at ${newOrgDbUrl}`);
            await tursoService.uploadDatabase(newOrgDbName, newOrgDbToken, `file:${tempDbFile}`);
            
            // Update main db with new org db url / auth token 
            logger.info(`Updating organization ${orgId} with new database credentials`);
            await mainDb.execute(`
              UPDATE organizations 
              SET turso_db_url = ?, turso_auth_token = ?
              WHERE id = ?
            `, [newOrgDbUrl, newOrgDbToken, orgId]);
            
            logger.info(`Successfully completed import for organization ${orgId}`);
            
            // Return success message with import stats
            return `Successfully imported ${totalAdded} contacts (${totalSkipped} skipped) to new database: ${newOrgDbUrl}`;
          } catch (importError) {
            logger.error(`Error during Turso database creation or upload: ${importError}`);
            throw importError;
          }
        } catch (error) {
          // Rollback transaction on error
          try {
            if (localDb) {
              localDb.exec('ROLLBACK');
            }
          } catch (rollbackError) {
            logger.error(`Error during transaction rollback: ${rollbackError}`);
          }
          throw error;
        }
      } finally {
        // Clean up temporary files
        try {
          if (localDb) {
            localDb.close();
          }
          await fsPromises.unlink(tempDumpFile);
          await fsPromises.unlink(tempDbFile);
          logger.info('Cleaned up temporary files');
        } catch (cleanupError) {
          logger.error(`Error cleaning up temporary files: ${cleanupError}`);
        }
      }
    } catch (error) {
      logger.error(`Error in bulk import for organization ${orgId}: ${error}`);
      throw error;
    }
  }

  static getUserFromSession = getUserFromSession;
  static getOrganizationById = getOrganizationById;

  static async ensureOrgSchema(orgDb: Database): Promise<void> {
    try {
      // Get all existing tables at once
      const existingTables = await orgDb.fetchAll(
        "SELECT name FROM sqlite_master WHERE type='table' AND name IN ('contacts', 'contact_events', 'tracking_clicks', 'leads', 'eligibility_answers', 'email_send_tracking', 'email_schedules', 'deleted_contacts')"
      );

      // Create a set of existing table names for faster lookup
      const tableSet = new Set(existingTables.map((row: any) => row.name || row[0]));
      
      // Define tables with their schema and indexes
      const tables = [
        {
          name: 'contacts',
          createSql: `
            CREATE TABLE IF NOT EXISTS contacts (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              first_name TEXT NOT NULL,
              last_name TEXT NOT NULL,
              email TEXT NOT NULL UNIQUE,
              current_carrier TEXT,
              plan_type TEXT,
              effective_date TEXT,
              birth_date TEXT,
              tobacco_user INTEGER NOT NULL,
              gender TEXT,
              state TEXT,
              zip_code TEXT,
              agent_id INTEGER,
              last_emailed DATETIME,
              phone_number TEXT,
              status TEXT NOT NULL DEFAULT '',
              aep_request BOOLEAN DEFAULT FALSE,
              aep_request_date DATETIME,
              created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
              updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
          `,
          indexSqls: [
            `CREATE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email)`,
            `CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email_unique ON contacts(LOWER(TRIM(email)))`,
            `CREATE INDEX IF NOT EXISTS idx_contacts_agent_id ON contacts(agent_id)`,
            `CREATE INDEX IF NOT EXISTS idx_contacts_status ON contacts(status)`,
            `CREATE TRIGGER IF NOT EXISTS update_contacts_timestamp AFTER UPDATE ON contacts BEGIN UPDATE contacts SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id; END`
          ]
        },
        {
          name: 'contact_events',
          createSql: `
            CREATE TABLE IF NOT EXISTS contact_events (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              contact_id INTEGER,
              lead_id INTEGER,
              event_type TEXT NOT NULL,
              metadata TEXT,
              created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
              FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE,
              FOREIGN KEY (lead_id) REFERENCES leads(id) ON DELETE CASCADE
            )
          `,
          indexSqls: [
            `CREATE INDEX IF NOT EXISTS idx_contact_events_contact_id ON contact_events(contact_id)`,
            `CREATE INDEX IF NOT EXISTS idx_contact_events_lead_id ON contact_events(lead_id)`,
            `CREATE INDEX IF NOT EXISTS idx_contact_events_type ON contact_events(event_type)`
          ]
        },
        {
          name: 'tracking_clicks',
          createSql: `
            CREATE TABLE IF NOT EXISTS tracking_clicks (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              tracking_id TEXT NOT NULL,
              contact_id INTEGER,
              quote_id TEXT,
              clicked_at DATETIME DEFAULT CURRENT_TIMESTAMP,
              FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
            )
          `,
          indexSqls: [
            `CREATE INDEX IF NOT EXISTS idx_tracking_clicks_tracking_id ON tracking_clicks(tracking_id)`,
            `CREATE INDEX IF NOT EXISTS idx_tracking_clicks_contact_id ON tracking_clicks(contact_id)`,
            `CREATE INDEX IF NOT EXISTS idx_tracking_clicks_clicked_at ON tracking_clicks(clicked_at)`,
          ]
        },
        {
          name: 'leads',
          createSql: `
            CREATE TABLE IF NOT EXISTS leads (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              name TEXT NOT NULL,
              email TEXT NOT NULL,
              created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
              UNIQUE(email)
            )
          `,
          indexSqls: [
            `CREATE INDEX IF NOT EXISTS idx_leads_email ON leads(email)`
          ]
        },
        {
          name: 'eligibility_answers',
          createSql: `
            CREATE TABLE IF NOT EXISTS eligibility_answers (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              contact_id INTEGER NOT NULL,
              quote_id TEXT NOT NULL,
              answers TEXT NOT NULL,
              created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
              FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
            )
          `,
          indexSqls: [
            `CREATE INDEX IF NOT EXISTS idx_eligibility_answers_contact_id ON eligibility_answers(contact_id)`
          ]
        },
        {
          name: 'email_send_tracking',
          createSql: `
            CREATE TABLE IF NOT EXISTS email_send_tracking (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              org_id INTEGER NOT NULL,
              contact_id INTEGER NOT NULL,
              email_type TEXT NOT NULL,
              scheduled_date TEXT NOT NULL,
              send_status TEXT NOT NULL CHECK(send_status IN ('pending', 'processing', 'accepted', 'delivered', 'sent', 'deferred', 'bounced', 'dropped', 'failed', 'skipped')) DEFAULT 'pending',
              send_mode TEXT NOT NULL CHECK(send_mode IN ('test', 'production')) DEFAULT 'test',
              test_email TEXT,
              send_attempt_count INTEGER NOT NULL DEFAULT 0,
              last_attempt_date TEXT,
              last_error TEXT,
              batch_id TEXT NOT NULL,
              message_id TEXT,
              delivery_status TEXT,
              status_checked_at TEXT,
              status_details TEXT,
              created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
              updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
              FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
            )
          `,
          indexSqls: [
            `CREATE INDEX IF NOT EXISTS idx_email_tracking_batch_id ON email_send_tracking(batch_id)`,
            `CREATE INDEX IF NOT EXISTS idx_email_tracking_send_status ON email_send_tracking(send_status)`,
            `CREATE INDEX IF NOT EXISTS idx_email_tracking_send_mode ON email_send_tracking(send_mode)`,
            `CREATE INDEX IF NOT EXISTS idx_email_tracking_contact_id ON email_send_tracking(contact_id)`,
            `CREATE INDEX IF NOT EXISTS idx_email_tracking_contact_type ON email_send_tracking(contact_id, email_type)`,
            `CREATE INDEX IF NOT EXISTS idx_email_tracking_status_date ON email_send_tracking(send_status, scheduled_date)`,
            `CREATE INDEX IF NOT EXISTS idx_email_tracking_message_id ON email_send_tracking(message_id)`,
            `CREATE INDEX IF NOT EXISTS idx_email_tracking_delivery_status ON email_send_tracking(delivery_status)`,
            `CREATE TRIGGER IF NOT EXISTS update_email_tracking_timestamp AFTER UPDATE ON email_send_tracking BEGIN UPDATE email_send_tracking SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id; END`
          ]
        },
        {
          name: 'email_schedules',
          createSql: `
            CREATE TABLE IF NOT EXISTS email_schedules (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              contact_id INTEGER NOT NULL,
              email_type TEXT NOT NULL,
              scheduled_send_date TEXT NOT NULL,
              scheduled_send_time TEXT NOT NULL DEFAULT '08:30:00',
              batch_id TEXT,
              status TEXT NOT NULL DEFAULT 'pre-scheduled',
              skip_reason TEXT,
              created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
              updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
          `,
          indexSqls: [
            `CREATE INDEX IF NOT EXISTS idx_email_schedules_org_contact ON email_schedules (contact_id)`,
            `CREATE INDEX IF NOT EXISTS idx_email_schedules_org_send_date ON email_schedules (scheduled_send_date)`,
            `CREATE INDEX IF NOT EXISTS idx_email_schedules_status ON email_schedules (status)`,
            `CREATE TRIGGER IF NOT EXISTS update_email_schedules_updated_at
AFTER UPDATE ON email_schedules
FOR EACH ROW
BEGIN
    UPDATE email_schedules
    SET updated_at = CURRENT_TIMESTAMP
    WHERE id = OLD.id;
END;`
          ]
        },
        {
          name: 'deleted_contacts',
          createSql: `
            CREATE TABLE IF NOT EXISTS deleted_contacts (
                original_contact_id INTEGER NOT NULL, -- The ID from the original 'contacts' table
                first_name TEXT,
                last_name TEXT,
                email TEXT NOT NULL,        -- Must be normalized: lowercase, trimmed
                phone_number TEXT,
                current_carrier TEXT,       -- Optional: Include other relevant fields
                plan_type TEXT,             -- Optional
                effective_date TEXT,        -- Optional
                birth_date TEXT,            -- Optional
                tobacco_user INTEGER,       -- Optional
                gender TEXT,                -- Optional
                state TEXT,                 -- Optional
                zip_code TEXT,              -- Optional
                agent_id INTEGER,           -- Optional
                status TEXT,                -- Optional: Original status before deletion
                deleted_at DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL
            )
          `,
          indexSqls: [
            `CREATE INDEX IF NOT EXISTS idx_deleted_contacts_email_org ON deleted_contacts (LOWER(TRIM(email)))`,
            `CREATE INDEX IF NOT EXISTS idx_deleted_contacts_deleted_at ON deleted_contacts (deleted_at)`,
            `CREATE INDEX IF NOT EXISTS idx_deleted_contacts_original_id ON deleted_contacts (original_contact_id)`
          ]
        }
      ];
      
      // Prepare batch operations
      const schemaOperations = [];
      
      // Process each table
      for (const table of tables) {
        // Create table if it doesn't exist
        if (!tableSet.has(table.name)) {
          logger.info(`Creating ${table.name} table schema`);
          schemaOperations.push({
            sql: table.createSql,
            args: []
          });
        }
        
        // Always add indexes regardless of whether table existed before
        // This ensures all necessary indexes exist in all databases
        for (const indexSql of table.indexSqls) {
          logger.info(`Ensuring index for ${table.name}: ${indexSql}`);
          schemaOperations.push({
            sql: indexSql,
            args: []
          });
        }
      }
      
      // Execute all schema operations in a single batch if there are any
      if (schemaOperations.length > 0) {
        logger.info(`Executing ${schemaOperations.length} schema operations in batch`);
        await orgDb.batch(schemaOperations, 'write');
        logger.info('Schema setup completed successfully');
      } else {
        logger.info('All required tables exist, checking indexes...');
        
        // Even if all tables exist, we still need to ensure all indexes exist
        const indexOperations = [];
        for (const table of tables) {
          for (const indexSql of table.indexSqls) {
            indexOperations.push({
              sql: indexSql,
              args: []
            });
          }
        }
        
        if (indexOperations.length > 0) {
          logger.info(`Ensuring ${indexOperations.length} indexes exist`);
          await orgDb.batch(indexOperations, 'write');
          logger.info('Index verification completed');
        }
      }
    } catch (error) {
      logger.error(`Error ensuring org schema: ${error}`);
      throw error;
    }
  }

  close() {
    if (this.isLocal && this.bunDb) {
      this.bunDb.close()
    }
  }

  private async fetch(fetchUrl: RequestInfo, options: RequestInit | undefined = undefined): Promise<Response> {
    const response = await fetch(fetchUrl, options || {});
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    return response;
  }

  private async fetchWithRetry(fetchUrl: RequestInfo, options: RequestInit | undefined = undefined): Promise<Response> {
    const maxRetries = 3;
    let lastError: Error | null = null;

    for (let attempt = 0; attempt < maxRetries; attempt++) {
      try {
        const response = await this.fetch(fetchUrl, options);
        if (response.status === 429) {
          // Rate limited, wait with exponential backoff
          const delay = Math.pow(2, attempt) * 1000;
          logger.warn(`Rate limit hit, retry ${attempt + 1}/${maxRetries} after ${delay}ms`);
          await new Promise(resolve => setTimeout(resolve, delay));
          continue;
        }
        return response;
      } catch (error) {
        lastError = error as Error;
        if (attempt < maxRetries - 1) {
          const delay = Math.pow(2, attempt) * 1000;
          logger.warn(`Request failed, retry ${attempt + 1}/${maxRetries} after ${delay}ms: ${error}`);
          await new Promise(resolve => setTimeout(resolve, delay));
        }
      }
    }

    throw lastError || new Error('Max retries reached');
  }
}

export const db = new Database() 

/**
 * Get user from session cookie
 */
export async function getUserFromSession(request: any): Promise<any> {
  try {
    const db = new Database();
    let sessionCookie: string | undefined;

    // Handle different request header formats
    if (request.headers) {
      if (typeof request.headers.get === 'function') {
        // Standard Request object
        sessionCookie = request.headers.get('cookie')?.split(';')
          .find((c: string) => c.trim().startsWith('session='))
          ?.split('=')[1];
      } else if (typeof request.headers === 'object') {
        // Raw headers object or Express request
        const cookieHeader = request.headers.cookie || request.headers['cookie'] || request.headers['Cookie'];
        if (typeof cookieHeader === 'string') {
          sessionCookie = cookieHeader.split(';')
            .find((c: string) => c.trim().startsWith('session='))
            ?.split('=')[1];
        }
      }
    }

    if (!sessionCookie) {
      logger.info('No session cookie found');
      return null;
    }

    // Log the session ID for debugging
    logger.info(`Session lookup result: ${JSON.stringify(await db.fetchOne('SELECT * FROM sessions WHERE id = ?', [sessionCookie]))}`);
    
    // Check if the session exists and hasn't expired
    const session = await db.fetchOne<{ id: string, user_id: number, expires_at: string, created_at: string }>(
      'SELECT * FROM sessions WHERE id = ?',
      [sessionCookie]
    );

    if (!session) {
      logger.info('No valid session found');
      return null;
    }

    // Check if session has expired
    const expiresAt = new Date(session.expires_at);
    const now = new Date();
    
    logger.info(`Session expires: ${expiresAt}, current time: ${now}`);
    
    if (expiresAt < now) {
      logger.info('Session has expired');
      return null;
    }

    // Get the user associated with the session
    const user = await db.fetchOne(
      'SELECT u.*, o.name as organization_name FROM users u JOIN organizations o ON u.organization_id = o.id WHERE u.id = ?',
      [session.user_id]
    );

    logger.info(`User lookup result: ${JSON.stringify(user)}`);

    if (!user) {
      logger.info('No user found for session');
      return null;
    }
    
    return user;
  } catch (error) {
    logger.error(`Error getting user from session: ${error}`);
    return null;
  }
}

/**
 * Get organization by ID
 */
export async function getOrganizationById(orgId: number): Promise<any> {
  try {
    const db = new Database();
    const org = await db.query('SELECT * FROM organizations WHERE id = ?', [orgId]);
    if (!org || org.length === 0) return null;
    return org[0];
  } catch (error) {
    logger.error(`Error getting organization: ${error}`);
    return null;
  }
}

// Function to split SQL dump into individual statements
function splitSqlStatements(sql: string): string[] {
  const statements: string[] = [];
  let currentStatement = "";
  let inString = false;
  let inComment = false;
  let inBlockComment = false;
  let stringQuote: string | null = null;

  for (let i = 0; i < sql.length; i++) {
    const char = sql[i];
    const nextChar = i + 1 < sql.length ? sql[i + 1] : null;

    // Handle comments
    if (!inString) {
      if (char === "-" && nextChar === "-" && !inBlockComment) {
        inComment = true;
        i++; // Skip next char
        continue;
      }
      if (char === "/" && nextChar === "*" && !inComment) {
        inBlockComment = true;
        i++;
        continue;
      }
      if (inComment && char === "\n") {
        inComment = false;
        continue;
      }
      if (inBlockComment && char === "*" && nextChar === "/") {
        inBlockComment = false;
        i++;
        continue;
      }
      if (inComment || inBlockComment) {
        continue;
      }
    }

    // Handle string literals
    if ((char === "'" || char === '"') && !inComment && !inBlockComment) {
      if (!inString) {
        inString = true;
        stringQuote = char;
      } else if (char === stringQuote) {
        // Check for escaped quotes
        if (sql[i - 1] !== "\\") {
          inString = false;
          stringQuote = null;
        }
      }
    }

    // Handle statement termination
    if (char === ";" && !inString && !inComment && !inBlockComment) {
      statements.push(currentStatement.trim());
      currentStatement = "";
      continue;
    }

    currentStatement += char;
  }

  // Add the last statement if it exists
  if (currentStatement.trim()) {
    statements.push(currentStatement.trim());
  }

  return statements.filter(stmt => stmt.length > 0);
}