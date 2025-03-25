import { createClient } from '@libsql/client'
import { config } from './config'
import { logger } from './logger'
import { TursoService } from './services/turso'
import { Database as BunDatabase } from 'bun:sqlite'
import fs from 'fs'
import path from 'path'
import { parse } from 'csv-parse'
import { pipeline } from 'stream/promises'
import fetch from 'node-fetch'
import type { ContactCreate } from './types'

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
        // Set high concurrency for parallel operations
        concurrency: 100
      })
    }
    
    logger.info(`Database connected to: ${this.isLocal ? dbName : this.url}`)
  }

  static async getOrgDb(orgId: string): Promise<Database> {
    const mainDb = new Database()
    const org = await mainDb.fetchOne<{ turso_db_url: string; turso_auth_token: string }>(
      'SELECT turso_db_url, turso_auth_token FROM organizations WHERE id = ?',
      [orgId]
    )
    if (!org?.turso_db_url) {
      logger.error(`No database URL found for org ${orgId}`)
      throw new Error('Organization database not configured')
    }
    
    // If using local SQLite, we don't need the auth token
    if (config.USE_LOCAL_SQLITE) {
      logger.info(`Creating local SQLite client for org ${orgId}`)
      return new Database(org.turso_db_url)
    } else {
      if (!org.turso_auth_token) {
        logger.error(`No auth token found for org ${orgId}`)
        throw new Error('Organization database not configured')
      }
      logger.info(`Creating Turso client for org ${orgId}`)
      return new Database(org.turso_db_url, org.turso_auth_token)
    }
  }

  /**
   * Get organization's database or initialize it if it doesn't exist
   * This method is used as a fallback when the database needs to be created on the fly
   */
  static async getOrInitOrgDb(orgId: string): Promise<Database> {
    try {
      const db = await Database.getOrgDb(orgId)
      await Database.ensureOrgSchema(db)
      return db
    } catch (error) {
      if (error instanceof Error && error.message === 'Organization database not configured') {
        logger.info(`No database found for org ${orgId}, initializing new database`)
        const mainDb = new Database()
        const orgExists = await mainDb.fetchOne<{ id: number }>(
          'SELECT id FROM organizations WHERE id = ?',
          [orgId]
        )
        if (!orgExists) throw new Error('Organization not found')

        const turso = new TursoService()
        logger.info(`Creating new Turso database for org ${orgId}`)
        const { url, token } = await turso.createOrganizationDatabase(orgId)
        logger.info(`Got new database URL: ${url}`)
        logger.info(`Got new database token (length: ${token.length})`)

        // Verify we can connect with the new credentials
        try {
          logger.info('Verifying connection with new credentials...')
          const testDb = new Database(url, token)
          await testDb.execute('SELECT 1')
          logger.info('Successfully verified connection with new credentials')
        } catch (connError) {
          logger.error(`Failed to verify connection with new credentials: ${connError instanceof Error ? connError.message : String(connError)}`)
          throw new Error('Failed to verify connection with new database credentials')
        }

        // Update organization with new credentials
        logger.info(`Updating organization ${orgId} with new database credentials`)
        await mainDb.execute(
          'UPDATE organizations SET turso_db_url = ?, turso_auth_token = ? WHERE id = ?',
          [url, token, orgId]
        )

        // Verify the update
        const updatedOrg = await mainDb.fetchOne<{ turso_db_url: string, turso_auth_token: string }>(
          'SELECT turso_db_url, turso_auth_token FROM organizations WHERE id = ?',
          [orgId]
        )
        if (!updatedOrg) {
          logger.error('Failed to fetch updated organization after credential update')
          throw new Error('Failed to update organization credentials')
        }
        if (updatedOrg.turso_db_url !== url || updatedOrg.turso_auth_token !== token) {
          logger.error('Organization credentials mismatch after update')
          logger.error(`Expected URL: ${url}, got: ${updatedOrg.turso_db_url}`)
          logger.error(`Expected token length: ${token.length}, got: ${updatedOrg.turso_auth_token.length}`)
          throw new Error('Organization credentials mismatch after update')
        }

        logger.info(`Successfully initialized database for organization ${orgId}`)
        const newDb = new Database(url, token)
        await Database.ensureOrgSchema(newDb)
        return newDb
      }
      logger.error(`Error in getOrInitOrgDb: ${error instanceof Error ? error.message : String(error)}`)
      if (error instanceof Error && error.stack) {
        logger.error(`Stack trace: ${error.stack}`)
      }
      throw error
    }
  }

  /**
   * Ensure that the organization database has all required tables
   * This can be used to add new tables to existing databases when the schema changes
   */
  static async ensureDatabaseSchema(orgId: string): Promise<void> {
    const orgDb = await Database.getOrgDb(orgId);
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
            FOREIGN KEY (contact_id) REFERENCES contacts(id),
            FOREIGN KEY (lead_id) REFERENCES leads(id)
          )`,
          indexStatements: [
            `CREATE INDEX IF NOT EXISTS idx_contact_events_contact_id ON contact_events(contact_id)`,
            `CREATE INDEX IF NOT EXISTS idx_contact_events_lead_id ON contact_events(lead_id)`,
            `CREATE INDEX IF NOT EXISTS idx_contact_events_type ON contact_events(event_type)`
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
        }
    ];
      
      for (const table of tables) {
      const tableExists = await orgDb.fetchOne<{ cnt: number }>(
            `SELECT COUNT(*) as cnt FROM sqlite_master WHERE type='table' AND name=?`,
            [table.name]
      );
      if (!tableExists || tableExists.cnt === 0) {
        logger.info(`Creating missing table ${table.name} for org ${orgId}`);
        await orgDb.execute(table.createStatement);
            for (const indexStatement of table.indexStatements) {
          await orgDb.execute(indexStatement);
        }
      }
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
      // Check if it's a response too large error
      if (error instanceof Error && error.message.includes('RESPONSE_TOO_LARGE')) {
        logger.error('Response too large, try using pagination');
        throw new Error('Response too large, try using pagination');
      }
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
   * Bulk import contacts by merging existing data with new CSV data
   * Optimized for performance with larger batch sizes, transaction handling,
   * and reduced network round trips
   */
  static async bulkImportContacts(orgId: string, csvFilePath: string, overwriteExisting: boolean = false): Promise<void> {
    try {
      logger.info(`Starting bulk import for org ${orgId}`);
      
      // Get organization database connection
      const orgDb = await Database.getOrgDb(orgId);
      if (!orgDb) {
        throw new Error('Failed to get organization database connection');
      }

      // Enhanced column mapping with more variations
      const columnMap: Record<string, string> = {
        // First Name variations
        first_name: 'first_name',
        firstName: 'first_name',
        'first name': 'first_name',
        'First Name': 'first_name',
        'FirstName': 'first_name',
        fname: 'first_name',
        'first-name': 'first_name',
        
        // Last Name variations
        last_name: 'last_name',
        lastName: 'last_name',
        'last name': 'last_name',
        'Last Name': 'last_name',
        'LastName': 'last_name',
        lname: 'last_name',
        'last-name': 'last_name',
        
        // Email variations
        email: 'email',
        'Email': 'email',
        'email_address': 'email',
        'emailAddress': 'email',
        'email address': 'email',
        'Email Address': 'email',
        
        // Carrier variations
        current_carrier: 'current_carrier',
        currentCarrier: 'current_carrier',
        'current carrier': 'current_carrier',
        carrier: 'current_carrier',
        'Current Carrier': 'current_carrier',
        'insurance carrier': 'current_carrier',
        'Insurance Carrier': 'current_carrier',
        
        // Plan Type variations
        plan_type: 'plan_type',
        planType: 'plan_type',
        'plan type': 'plan_type',
        'Plan Type': 'plan_type',
        'insurance type': 'plan_type',
        'Insurance Type': 'plan_type',
        
        // Effective Date variations
        effective_date: 'effective_date',
        effectiveDate: 'effective_date',
        'effective date': 'effective_date',
        'Effective Date': 'effective_date',
        'start date': 'effective_date',
        'Start Date': 'effective_date',
        
        // Birth Date variations
        birth_date: 'birth_date',
        birthDate: 'birth_date',
        'birth date': 'birth_date',
        'Birth Date': 'birth_date',
        dob: 'birth_date',
        'DOB': 'birth_date',
        'date of birth': 'birth_date',
        'Date of Birth': 'birth_date',
        
        // Tobacco User variations
        tobacco_user: 'tobacco_user',
        tobaccoUser: 'tobacco_user',
        'tobacco user': 'tobacco_user',
        'Tobacco User': 'tobacco_user',
        'uses tobacco': 'tobacco_user',
        'Uses Tobacco': 'tobacco_user',
        smoker: 'tobacco_user',
        'Smoker': 'tobacco_user',
        
        // Gender variations
        gender: 'gender',
        'Gender': 'gender',
        sex: 'gender',
        'Sex': 'gender',
        
        // State variations
        state: 'state',
        'State': 'state',
        'state code': 'state',
        'State Code': 'state',
        
        // Zip Code variations
        zip_code: 'zip_code',
        zipCode: 'zip_code',
        'zip code': 'zip_code',
        'Zip Code': 'zip_code',
        zip: 'zip_code',
        'ZIP': 'zip_code',
        postal: 'zip_code',
        'Postal': 'zip_code',
        
        // Phone Number variations
        phone_number: 'phone_number',
        phoneNumber: 'phone_number',
        'phone number': 'phone_number',
        'Phone Number': 'phone_number',
        phone: 'phone_number',
        'Phone': 'phone_number',
        tel: 'phone_number',
        'Tel': 'phone_number',
        
        // Agent ID variations
        agent_id: 'agent_id',
        agentId: 'agent_id',
        'agent id': 'agent_id',
        'Agent ID': 'agent_id',
        'agent number': 'agent_id',
        'Agent Number': 'agent_id'
      };

      // Helper function to get field value
      const getValue = (record: any, fieldName: string, defaultValue: string = ''): string => {
        // Try all possible mappings
        for (const [key, mappedField] of Object.entries(columnMap)) {
          if (mappedField === fieldName && record[key] !== undefined) {
            return record[key] || defaultValue;
          }
        }
        // Try direct field name
        return record[fieldName] || defaultValue;
      };

      // Use streaming to parse CSV, improving memory efficiency
      logger.info(`Streaming CSV file: ${csvFilePath}`);
      const emailMap = new Map<string, ContactCreate>();
      let processed = 0;
      let skipped = 0;
      let firstRecord = true;
      let columnHeaders: string[] = [];

      // Stream and process CSV
      await pipeline(
        fs.createReadStream(csvFilePath),
        parse({ columns: true, skip_empty_lines: true, trim: true }),
        async function* (source) {
          for await (const record of source) {
            processed++;
            
            // Log first record for debugging
            if (firstRecord) {
              columnHeaders = Object.keys(record);
              logger.info(`CSV column headers: ${columnHeaders.join(', ')}`);
              logger.info(`First record raw data: ${JSON.stringify(record)}`);
              
              // Log column mapping results
              const mappedFields = new Set<string>();
              columnHeaders.forEach(key => {
                const mappedTo = columnMap[key];
                if (mappedTo) {
                  mappedFields.add(mappedTo);
                  logger.info(`Mapped column '${key}' to '${mappedTo}'`);
                } else {
                  logger.warn(`No mapping found for column: '${key}'`);
                }
              });
              
              // Log missing required fields
              const requiredFields = ['first_name', 'last_name', 'email', 'effective_date', 'birth_date'];
              const missingRequired = requiredFields.filter(field => !mappedFields.has(field));
              if (missingRequired.length > 0) {
                logger.warn(`Missing required fields in CSV: ${missingRequired.join(', ')}`);
              }
              firstRecord = false;
            }
            
            try {
              const contact: ContactCreate = {
                first_name: getValue(record, 'first_name'),
                last_name: getValue(record, 'last_name'),
                email: getValue(record, 'email'),
                current_carrier: getValue(record, 'current_carrier'),
                plan_type: getValue(record, 'plan_type'),
                effective_date: getValue(record, 'effective_date'),
                birth_date: getValue(record, 'birth_date'),
                tobacco_user: !!(getValue(record, 'tobacco_user') === '1' || getValue(record, 'tobacco_user').toLowerCase() === 'true'),
                gender: getValue(record, 'gender'),
                state: getValue(record, 'state'),
                zip_code: getValue(record, 'zip_code'),
                phone_number: getValue(record, 'phone_number'),
                agent_id: getValue(record, 'agent_id') ? Number(getValue(record, 'agent_id')) : null,
              };
              
              // Log sample data for the first few records
              if (processed <= 3) {
                logger.info(`Sample mapped contact data: ${JSON.stringify(contact)}`);
              }
              
              if (!contact.first_name || !contact.last_name || !contact.email || !contact.effective_date || !contact.birth_date) {
                logger.warn(`Skipping record due to missing required fields: ${JSON.stringify(contact)}`);
                skipped++;
                continue;
              }

              const email = contact.email.toLowerCase().trim();
              emailMap.set(email, contact);
            } catch (error) {
              logger.warn(`Error processing record: ${error}`);
              skipped++;
            }
          }
        }
      );

      const emails = Array.from(emailMap.keys());
      const contactsToInsert = Array.from(emailMap.values());
      const totalContacts = contactsToInsert.length;
      
      logger.info(`Processed ${processed} records, valid contacts: ${totalContacts}, skipped: ${skipped}`);
      
      // Early exit if no valid contacts
      if (totalContacts === 0) {
        logger.info('No valid contacts to import, exiting early');
        return;
      }

      // Get existing emails from the database
      logger.info('Fetching existing emails in a single query');
      const existingEmailsResult = await orgDb.fetchAll(
        'SELECT LOWER(TRIM(email)) as email FROM contacts'
      );
      const existingEmails = new Set<string>();
      existingEmailsResult.forEach((row: { email: string }) => 
        existingEmails.add(row.email.toLowerCase().trim())
      );
      
      logger.info(`Found ${existingEmails.size} existing email addresses`);

      // Split contacts for insert and update
      const newContacts = contactsToInsert.filter(contact => 
        !existingEmails.has(contact.email.toLowerCase().trim()));
      const updateContacts = overwriteExisting 
        ? contactsToInsert.filter(contact => 
            existingEmails.has(contact.email.toLowerCase().trim())) 
        : [];
        
      logger.info(`Split contacts: ${newContacts.length} new, ${updateContacts.length} updates`);

      // Function to create insert statements for a batch of contacts
      const createInsertBatch = (contacts: ContactCreate[]) => {
        const placeholders = contacts.map(() => 
          '(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)'
        ).join(',');
        
        const values = contacts.flatMap(contact => [
          contact.first_name,
          contact.last_name,
          contact.email.toLowerCase().trim(),
          contact.current_carrier,
          contact.plan_type,
          contact.effective_date,
          contact.birth_date,
          contact.tobacco_user ? 1 : 0,
          contact.gender,
          contact.state,
          contact.zip_code,
          contact.phone_number,
          contact.agent_id === null ? null : Number(contact.agent_id)
        ]);
        
        return {
          sql: `INSERT INTO contacts (
            first_name, last_name, email, current_carrier, plan_type, effective_date,
            birth_date, tobacco_user, gender, state, zip_code, phone_number, agent_id,
            created_at, updated_at
          ) VALUES ${placeholders}`,
          args: values
        };
      };

      // Function to create update statements for a batch of contacts
      const createUpdateBatch = (contacts: ContactCreate[]) => {
        return contacts.map(contact => ({
          sql: `UPDATE contacts SET 
            first_name = ?, last_name = ?, current_carrier = ?, plan_type = ?, effective_date = ?, 
            birth_date = ?, tobacco_user = ?, gender = ?, state = ?, zip_code = ?, 
            phone_number = ?, agent_id = ? 
            WHERE LOWER(TRIM(email)) = ?`,
          args: [
            contact.first_name,
            contact.last_name,
            contact.current_carrier,
            contact.plan_type,
            contact.effective_date,
            contact.birth_date,
            contact.tobacco_user ? 1 : 0,
            contact.gender,
            contact.state,
            contact.zip_code,
            contact.phone_number,
            contact.agent_id === null ? null : Number(contact.agent_id),
            contact.email.toLowerCase().trim()
          ]
        }));
      };
      
      // IMPROVED TRANSACTION HANDLING: Process a single chunk with proper transaction handling
      const processInsertChunk = async (chunk: ContactCreate[], chunkIndex: number, totalChunks: number): Promise<boolean> => {
        let retryCount = 0;
        const MAX_RETRIES = 3;
        const RETRY_DELAY = 1000; // ms
        
        while (retryCount < MAX_RETRIES) {
          let db = null;
          let tx = null;
          
          try {
            // Get fresh DB connection
            db = await Database.getOrgDb(orgId);
            
            if (db.isLocal && db.bunDb) {
              // Local SQLite uses standard transaction approach
              db.bunDb.exec('BEGIN TRANSACTION');
              const insertStmt = createInsertBatch(chunk);
              await db.execute(insertStmt.sql, insertStmt.args);
              db.bunDb.exec('COMMIT');
            } else {
              // Turso - use transaction API instead of manual BEGIN/COMMIT
              tx = await db.client.transaction('write');
              
              const insertStmt = createInsertBatch(chunk);
              await tx.execute(insertStmt);
              await tx.commit();
            }
            
            logger.info(`✓ [${chunkIndex + 1}/${totalChunks}] Completed insert chunk with ${chunk.length} contacts`);
            return true;
          } catch (error) {
            retryCount++;
            
            // Proper transaction rollback
            if (tx) {
              try {
                await tx.rollback();
              } catch (rollbackError) {
                logger.warn(`Warning: Rollback failed: ${rollbackError}`);
              }
            } else if (db?.isLocal && db.bunDb) {
              try {
                db.bunDb.exec('ROLLBACK');
              } catch (rollbackError) {
                logger.warn(`Warning: Local rollback failed: ${rollbackError}`);
              }
            }

            // Check for unique constraint violations
            const errorMsg = error instanceof Error ? error.message : String(error);
            if (errorMsg.includes('UNIQUE constraint failed')) {
              logger.warn(`Unique constraint violation in chunk ${chunkIndex + 1}, skipping after ${retryCount} attempts`);
              return false;
            }

            if (retryCount < MAX_RETRIES) {
              const delay = RETRY_DELAY * Math.pow(2, retryCount - 1); // Exponential backoff
              logger.warn(`Retrying chunk ${chunkIndex + 1} after error (attempt ${retryCount}): ${error}`);
              await new Promise(resolve => setTimeout(resolve, delay));
            } else {
              logger.error(`✗ [${chunkIndex + 1}/${totalChunks}] Error in insert chunk after ${MAX_RETRIES} attempts: ${error}`);
              throw error;
            }
          } finally {
            // Clean up resources
            if (db && !db.isLocal) {
              try {
                if (typeof db.client.close === 'function') {
                  await db.client.close();
                }
              } catch (closeError) {
                logger.warn(`Warning: Error closing database connection: ${closeError}`);
              }
            }
          }
        }
        
        return false;
      };

      // Similar function for update operations
      const processUpdateChunk = async (statements: any[], chunkIndex: number, totalChunks: number): Promise<boolean> => {
        let retryCount = 0;
        const MAX_RETRIES = 3;
        const RETRY_DELAY = 1000; // ms
        
        while (retryCount < MAX_RETRIES) {
          let db = null;
          let tx = null;
          
          try {
            db = await Database.getOrgDb(orgId);
            
            if (db.isLocal && db.bunDb) {
              // Local SQLite
              db.bunDb.exec('BEGIN TRANSACTION');
              
              for (const stmt of statements) {
                await db.execute(stmt.sql, stmt.args);
              }
              
              db.bunDb.exec('COMMIT');
            } else {
              // Turso - use transaction API
              tx = await db.client.transaction('write');
              
              for (const stmt of statements) {
                await tx.execute(stmt);
              }
              
              await tx.commit();
            }
            
            logger.info(`✓ [${chunkIndex + 1}/${totalChunks}] Completed update chunk with ${statements.length} contacts`);
            return true;
          } catch (error) {
            retryCount++;
            
            // Handle rollback
            if (tx) {
              try {
                await tx.rollback();
              } catch (rollbackError) {
                logger.warn(`Warning: Rollback failed: ${rollbackError}`);
              }
            } else if (db?.isLocal && db.bunDb) {
              try {
                db.bunDb.exec('ROLLBACK');
              } catch (rollbackError) {
                logger.warn(`Warning: Local rollback failed: ${rollbackError}`);
              }
            }

            if (retryCount < MAX_RETRIES) {
              const delay = RETRY_DELAY * Math.pow(2, retryCount - 1);
              logger.warn(`Retrying update chunk ${chunkIndex + 1} after error (attempt ${retryCount}): ${error}`);
              await new Promise(resolve => setTimeout(resolve, delay));
            } else {
              logger.error(`✗ [${chunkIndex + 1}/${totalChunks}] Error in update chunk after ${MAX_RETRIES} attempts: ${error}`);
              throw error;
            }
          } finally {
            // Clean up resources
            if (db && !db.isLocal) {
              try {
                if (typeof db.client.close === 'function') {
                  await db.client.close();
                }
              } catch (closeError) {
                logger.warn(`Warning: Error closing database connection: ${closeError}`);
              }
            }
          }
        }
        
        return false;
      };

      if (newContacts.length > 0 || updateContacts.length > 0) {
        // Process batch operations with concurrent tasks

        // Process inserts with proper transaction handling
        if (newContacts.length > 0) {
          const CONCURRENT_TRANSACTIONS = 20; // Keep high concurrency as requested
          const ROWS_PER_INSERT = 1000;
          const totalInsertChunks = Math.ceil(newContacts.length / ROWS_PER_INSERT);
          
          logger.info(`Processing ${newContacts.length} inserts concurrently in ${totalInsertChunks} chunks`);
          
          const tasks: Promise<boolean>[] = [];
          const results = { success: 0, failed: 0 };
          
          for (let i = 0; i < newContacts.length; i += ROWS_PER_INSERT) {
            const chunk = newContacts.slice(i, Math.min(i + ROWS_PER_INSERT, newContacts.length));
            const chunkIndex = Math.floor(i / ROWS_PER_INSERT);
            
            // Create task for this chunk with proper completion handling
            const task = processInsertChunk(chunk, chunkIndex, totalInsertChunks)
              .then(success => {
                if (success) results.success++;
                else results.failed++;
                
                // Remove this task from active tasks when done
                const index = tasks.indexOf(task);
                if (index > -1) tasks.splice(index, 1);
                
                return success;
              })
              .catch(error => {
                results.failed++;
                logger.error(`Insert chunk ${chunkIndex + 1} failed: ${error}`);
                
                // Remove this task from active tasks when done
                const index = tasks.indexOf(task);
                if (index > -1) tasks.splice(index, 1);
                
                return false;
              });
            
            tasks.push(task);
            
            // Wait if we've hit concurrency limit
            if (tasks.length >= CONCURRENT_TRANSACTIONS) {
              await Promise.race(tasks);
            }
          }
          
          // Wait for all remaining tasks to complete
          if (tasks.length > 0) {
            await Promise.all(tasks);
          }
          
          logger.info(`Insert operations completed: ${results.success} chunks successful, ${results.failed} chunks failed`);
        }
        
        // Process updates with proper transaction handling
        if (updateContacts.length > 0) {
          const CONCURRENT_TRANSACTIONS = 20;
          const ROWS_PER_UPDATE_BATCH = 2000;
          const totalUpdateChunks = Math.ceil(updateContacts.length / ROWS_PER_UPDATE_BATCH);
          
          logger.info(`Processing ${updateContacts.length} updates concurrently in ${totalUpdateChunks} chunks`);
          
          const tasks: Promise<boolean>[] = [];
          const results = { success: 0, failed: 0 };
          
          for (let i = 0; i < updateContacts.length; i += ROWS_PER_UPDATE_BATCH) {
            const chunk = updateContacts.slice(i, Math.min(i + ROWS_PER_UPDATE_BATCH, updateContacts.length));
            const chunkIndex = Math.floor(i / ROWS_PER_UPDATE_BATCH);
            
            // Create update statements for this chunk
            const updateStatements = createUpdateBatch(chunk);
            
            // Create task for this chunk with proper completion handling
            const task = processUpdateChunk(updateStatements, chunkIndex, totalUpdateChunks)
              .then(success => {
                if (success) results.success++;
                else results.failed++;
                
                // Remove this task from active tasks when done
                const index = tasks.indexOf(task);
                if (index > -1) tasks.splice(index, 1);
                
                return success;
              })
              .catch(error => {
                results.failed++;
                logger.error(`Update chunk ${chunkIndex + 1} failed: ${error}`);
                
                // Remove this task from active tasks when done
                const index = tasks.indexOf(task);
                if (index > -1) tasks.splice(index, 1);
                
                return false;
              });
            
            tasks.push(task);
            
            // Wait if we've hit concurrency limit
            if (tasks.length >= CONCURRENT_TRANSACTIONS) {
              await Promise.race(tasks);
            }
          }
          
          // Wait for all remaining tasks to complete
          if (tasks.length > 0) {
            await Promise.all(tasks);
          }
          
          logger.info(`Update operations completed: ${results.success} chunks successful, ${results.failed} chunks failed`);
        }
        
        // Verify final counts
        try {
          const countResult = await orgDb.fetchOne<{ count: number }>(
            'SELECT COUNT(*) as count FROM contacts'
          );
          logger.info(`Final contact count in database: ${countResult?.count || 0}`);
        } catch (error) {
          logger.warn(`Could not verify final contact count: ${error}`);
        }
        
        logger.info(`✓ Import Summary:
• Total contacts processed: ${processed}
• Valid contacts: ${contactsToInsert.length}
• New contacts inserted: ${newContacts.length}
• Existing contacts updated: ${updateContacts.length}
• Skipped contacts: ${skipped}`);
      }
      
      logger.info(`Bulk import completed successfully: ${processed} processed, ${contactsToInsert.length} valid contacts, ${skipped} skipped`);

    } catch (error) {
      logger.error(`Bulk import failed: ${error}`);
      throw error;
    }
  }

  static getUserFromSession = getUserFromSession;
  static getOrganizationById = getOrganizationById;

  static async ensureOrgSchema(orgDb: Database): Promise<void> {
    try {
      // Check if contacts table exists
      const tableExists = await orgDb.fetchOne(
        "SELECT name FROM sqlite_master WHERE type='table' AND name='contacts'"
      );

      if (!tableExists) {
        logger.info('Creating contacts table schema');
        // Create contacts table
        await orgDb.execute(`
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
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
          )
        `);

        // Create indexes for contacts table
        await orgDb.execute(`CREATE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email)`);
        await orgDb.execute(`CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email_unique ON contacts(LOWER(TRIM(email)))`);
      }

      // Check if contact_events table exists
      const eventsTableExists = await orgDb.fetchOne(
        "SELECT name FROM sqlite_master WHERE type='table' AND name='contact_events'"
      );

      if (!eventsTableExists) {
        logger.info('Creating contact_events table schema');
        // Create contact_events table
        await orgDb.execute(`
          CREATE TABLE IF NOT EXISTS contact_events (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            contact_id INTEGER,
            lead_id INTEGER,
            event_type TEXT NOT NULL,
            metadata TEXT,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            FOREIGN KEY (contact_id) REFERENCES contacts(id),
            FOREIGN KEY (lead_id) REFERENCES leads(id)
          )
        `);

        // Create indexes for contact_events table
        await orgDb.execute(`CREATE INDEX IF NOT EXISTS idx_contact_events_contact_id ON contact_events(contact_id)`);
        await orgDb.execute(`CREATE INDEX IF NOT EXISTS idx_contact_events_lead_id ON contact_events(lead_id)`);
        await orgDb.execute(`CREATE INDEX IF NOT EXISTS idx_contact_events_type ON contact_events(event_type)`);
      }

      // Check if leads table exists
      const leadsTableExists = await orgDb.fetchOne(
        "SELECT name FROM sqlite_master WHERE type='table' AND name='leads'"
      );

      if (!leadsTableExists) {
        logger.info('Creating leads table schema');
        // Create leads table
        await orgDb.execute(`
          CREATE TABLE IF NOT EXISTS leads (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            name TEXT NOT NULL,
            email TEXT NOT NULL,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            UNIQUE(email)
          )
        `);

        // Create index for leads table
        await orgDb.execute(`CREATE INDEX IF NOT EXISTS idx_leads_email ON leads(email)`);
      }

      // Check if eligibility_answers table exists
      const eligibilityTableExists = await orgDb.fetchOne(
        "SELECT name FROM sqlite_master WHERE type='table' AND name='eligibility_answers'"
      );

      if (!eligibilityTableExists) {
        logger.info('Creating eligibility_answers table schema');
        // Create eligibility_answers table
        await orgDb.execute(`
          CREATE TABLE IF NOT EXISTS eligibility_answers (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            contact_id INTEGER NOT NULL,
            quote_id TEXT NOT NULL,
            answers TEXT NOT NULL,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            FOREIGN KEY (contact_id) REFERENCES contacts(id)
          )
        `);

        // Create index for eligibility_answers table
        await orgDb.execute(`CREATE INDEX IF NOT EXISTS idx_eligibility_answers_contact_id ON eligibility_answers(contact_id)`);
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

  /**
   * Get paginated contacts with total count
   */
  async getPaginatedContacts(page: number = 1, limit: number = 100, filters: any = {}): Promise<{ contacts: any[], total: number }> {
    try {
      // Build the base query
      let query = 'SELECT * FROM contacts';
      let countQuery = 'SELECT COUNT(*) as total FROM contacts';
      const queryParams: any[] = [];
      const conditions: string[] = [];

      // Add filter conditions
      if (filters.search) {
        conditions.push('(LOWER(first_name) LIKE ? OR LOWER(last_name) LIKE ? OR LOWER(email) LIKE ?)');
        const searchTerm = `%${filters.search.toLowerCase()}%`;
        queryParams.push(searchTerm, searchTerm, searchTerm);
      }

      if (filters.states && filters.states.length > 0) {
        conditions.push(`state IN (${filters.states.map(() => '?').join(',')})`);
        queryParams.push(...filters.states);
      }

      if (filters.carriers && filters.carriers.length > 0) {
        conditions.push(`current_carrier IN (${filters.carriers.map(() => '?').join(',')})`);
        queryParams.push(...filters.carriers);
      }

      if (filters.agents && filters.agents.length > 0) {
        conditions.push(`agent_id IN (${filters.agents.map(() => '?').join(',')})`);
        queryParams.push(...filters.agents);
      }

      // Add WHERE clause if there are conditions
      if (conditions.length > 0) {
        const whereClause = ` WHERE ${conditions.join(' AND ')}`;
        query += whereClause;
        countQuery += whereClause;
      }

      // Add pagination
      const offset = (page - 1) * limit;
      query += ' ORDER BY created_at DESC LIMIT ? OFFSET ?';
      queryParams.push(limit, offset);

      // Get total count
      const totalResult = await this.fetchOne<{ total: number }>(countQuery, queryParams.slice(0, -2));
      const total = totalResult?.total || 0;

      // Get paginated results
      const contacts = await this.fetchAll(query, queryParams);

      return {
        contacts,
        total
      };
    } catch (error) {
      logger.error(`Error getting paginated contacts: ${error}`);
      throw error;
    }
  }

  /**
   * Get available filter options
   */
  async getFilterOptions(): Promise<{ carriers: string[], states: string[] }> {
    try {
      const carriers = await this.fetchAll(
        'SELECT DISTINCT current_carrier FROM contacts WHERE current_carrier IS NOT NULL'
      );
      const states = await this.fetchAll(
        'SELECT DISTINCT state FROM contacts WHERE state IS NOT NULL'
      );

      return {
        carriers: carriers.map((row: { current_carrier: string }) => row.current_carrier).filter(Boolean),
        states: states.map((row: { state: string }) => row.state).filter(Boolean)
      };
    } catch (error) {
      logger.error(`Error getting filter options: ${error}`);
      throw error;
    }
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

    const user = await db.query(
      'SELECT * FROM users WHERE session_id = ? AND session_expires > datetime("now")',
      [sessionCookie]
    );

    if (!user || user.length === 0) {
      logger.info('No valid user session found');
      return null;
    }
    return user[0];
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