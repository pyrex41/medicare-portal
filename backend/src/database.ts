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
        concurrency: 50 // Increase concurrency for higher throughput
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
    
    // Get all existing tables in one query for efficiency
    const existingTables = await orgDb.fetchAll(
      `SELECT name FROM sqlite_master WHERE type='table' AND name IN (${tables.map(t => `'${t.name}'`).join(', ')})`
    );
    
    // Create a set of existing table names for faster lookup
    const tableSet = new Set(existingTables.map((row: any) => row.name || row[0]));
    
    // Prepare batch operations
    const batchOperations = [];
    
    // Add table creation and index statements for missing tables
    for (const table of tables) {
      if (!tableSet.has(table.name)) {
        logger.info(`Adding schema operations for missing table ${table.name} for org ${orgId}`);
        
        // Add create table statement
        batchOperations.push({
          sql: table.createStatement,
          args: []
        });
        
        // Add index statements
        for (const indexStatement of table.indexStatements) {
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
      logger.info(`All required tables already exist for org ${orgId}, no schema changes needed`);
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
   * Bulk import contacts from CSV directly into the database with high concurrency
   */
  static async bulkImportContacts(orgId: string, csvFilePath: string, overwriteExisting: boolean = false): Promise<void> {
    try {
      logger.info(`Starting bulk import for org ${orgId}`);
      
      // Get database credentials
      const mainDb = new Database();
      const org = await mainDb.fetchOne<{ turso_db_url: string; turso_auth_token: string }>(
        'SELECT turso_db_url, turso_auth_token FROM organizations WHERE id = ?',
        [orgId]
      );
      
      if (!org?.turso_db_url || !org?.turso_auth_token) {
        throw new Error('Organization database not configured');
      }
      
      // Connect to the organization's database 
      const orgDb = new Database(org.turso_db_url, org.turso_auth_token);
      logger.info(`Connected to organization database: ${org.turso_db_url}`);
      
      // Parse CSV data
      logger.info(`Reading CSV file: ${csvFilePath}`);
      const csvContent = await fs.promises.readFile(csvFilePath, 'utf-8');
      
      // Parse the CSV content
      const records: any[] = [];
      await new Promise<void>((resolve, reject) => {
        parse(csvContent, {
          columns: true,
          skip_empty_lines: true,
          trim: true
        }, (err, output) => {
          if (err) reject(err);
          else {
            records.push(...output);
            resolve();
          }
        });
      });
      
      if (records.length === 0) {
        logger.info('No records found in CSV file, import completed');
        return;
      }
      
      // Log the first record and its keys to help debug column mapping
      const firstRecord = records[0];
      logger.info(`CSV column headers: ${Object.keys(firstRecord).join(', ')}`);
      logger.info(`First record raw data: ${JSON.stringify(firstRecord)}`);
      
      // Column mapping with variations
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
      
      // Log column mapping analysis
      const recordKeys = Object.keys(firstRecord);
      logger.info(`CSV columns found: ${recordKeys.join(', ')}`);
      
      const mappedFields = new Set<string>();
      recordKeys.forEach(key => {
        const mappedTo = columnMap[key];
        if (mappedTo) {
          mappedFields.add(mappedTo);
          logger.info(`Mapped column '${key}' to '${mappedTo}'`);
        } else {
          logger.warn(`No mapping found for column: '${key}'`);
        }
      });
      
      // Check for missing required fields
      const requiredFields = ['first_name', 'last_name', 'email', 'effective_date', 'birth_date'];
      const missingRequired = requiredFields.filter(field => !mappedFields.has(field));
      if (missingRequired.length > 0) {
        logger.warn(`Missing required fields in CSV: ${missingRequired.join(', ')}`);
        if (missingRequired.length >= 3) {
          throw new Error(`Too many required fields missing from CSV: ${missingRequired.join(', ')}`);
        }
      }
      
      // Helper function for normalized field value extraction
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
      
      // Get all existing emails if we need to check for duplicates
      let existingEmails = new Set<string>();
      if (!overwriteExisting) {
        logger.info('Fetching existing contacts to check for duplicates');
        const rows = await orgDb.fetchAll('SELECT LOWER(TRIM(email)) as email FROM contacts');
        existingEmails = new Set(rows.map((row: any) => row.email || row[0]));
        logger.info(`Found ${existingEmails.size} existing contacts`);
      }
      
      // Process in larger batches and execute them concurrently
      const BATCH_SIZE = 1000; // Larger batch size for better throughput
      const totalBatches = Math.ceil(records.length / BATCH_SIZE);
      
      logger.info(`Processing ${records.length} records in ${totalBatches} concurrent batches of up to ${BATCH_SIZE} records each`);
      
      // Prepare batch function to be called concurrently
      const processBatch = async (batchIndex: number): Promise<{added: number, skipped: number}> => {
        const startIdx = batchIndex * BATCH_SIZE;
        const endIdx = Math.min(startIdx + BATCH_SIZE, records.length);
        const batchRecords = records.slice(startIdx, endIdx);
        
        let batchAdded = 0;
        let batchSkipped = 0;
        
        // Prepare batch operations
        const batchOperations: { sql: string, args: any[] }[] = [];
        
        for (const record of batchRecords) {
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
            
            // Log sample data for first few records
            if (startIdx + batchAdded + batchSkipped < 3) {
              logger.info(`Sample mapped contact data: ${JSON.stringify(contact)}`);
            }
            
            if (!contact.first_name || !contact.last_name || !contact.email || !contact.effective_date || !contact.birth_date) {
              batchSkipped++;
              continue;
            }
            
            const email = contact.email.toLowerCase().trim();
            
            // Check if email exists (no synchronization needed since Set is thread-safe for reads)
            let emailExists = existingEmails.has(email);
            
            // Skip if email exists and we're not overwriting
            if (emailExists && !overwriteExisting) {
              batchSkipped++;
              continue;
            }
            
            // Add email to the set to prevent duplicates
            existingEmails.add(email);
            
            // If overwriting is enabled, add delete operation for existing contacts
            if (overwriteExisting && emailExists) {
              batchOperations.push({
                sql: 'DELETE FROM contacts WHERE LOWER(TRIM(email)) = ?',
                args: [email]
              });
            }
            
            // Add insert operation
            batchOperations.push({
              sql: `
                INSERT INTO contacts (
                  first_name, last_name, email, current_carrier, plan_type, effective_date,
                  birth_date, tobacco_user, gender, state, zip_code, phone_number, agent_id,
                  created_at, updated_at
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)
              `,
              args: [
                contact.first_name,
                contact.last_name,
                email,
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
              ]
            });
            
            batchAdded++;
          } catch (error) {
            batchSkipped++;
          }
        }
        
        // Execute batch operations with transaction support
        if (batchOperations.length > 0) {
          logger.info(`Executing batch ${batchIndex + 1}/${totalBatches} with ${batchOperations.length} operations`);
          try {
            await orgDb.batch(batchOperations, 'write');
            logger.info(`Batch ${batchIndex + 1}/${totalBatches} completed successfully: added ${batchAdded}, skipped ${batchSkipped}`);
            return { added: batchAdded, skipped: batchSkipped };
          } catch (error) {
            logger.error(`Error executing batch ${batchIndex + 1}/${totalBatches}: ${error}`);
            // Continue with the next batch even if this one failed
            return { added: 0, skipped: batchRecords.length };
          }
        } else {
          logger.info(`Batch ${batchIndex + 1}/${totalBatches} had no operations to execute`);
          return { added: 0, skipped: batchRecords.length };
        }
      };
      
      // Launch all batches concurrently
      const batchPromises: Promise<{added: number, skipped: number}>[] = [];
      for (let batchIndex = 0; batchIndex < totalBatches; batchIndex++) {
        batchPromises.push(processBatch(batchIndex));
      }
      
      // Wait for all batches to complete
      logger.info(`Launched ${batchPromises.length} concurrent batch operations`);
      const results = await Promise.allSettled(batchPromises);
      
      // Compute total results
      let totalAdded = 0;
      let totalSkipped = 0;
      let totalProcessed = 0;
      
      results.forEach((result, index) => {
        if (result.status === 'fulfilled') {
          totalAdded += result.value.added;
          totalSkipped += result.value.skipped;
        } else {
          logger.error(`Batch ${index + 1} failed: ${result.reason}`);
          // Assume all records in this batch were skipped
          const batchSize = Math.min(BATCH_SIZE, records.length - (index * BATCH_SIZE));
          totalSkipped += batchSize;
        }
      });
      
      totalProcessed = totalAdded + totalSkipped;
      
      // Update the organization's last update timestamp
      await mainDb.execute(
        'UPDATE organizations SET updated_at = CURRENT_TIMESTAMP WHERE id = ?',
        [orgId]
      );
      
      // Get final count
      const finalCount = await orgDb.fetchOne<{ count: number }>('SELECT COUNT(*) as count FROM contacts');
      logger.info(`Bulk import completed for org ${orgId}: processed ${totalProcessed}, added ${totalAdded}, skipped ${totalSkipped}, final count: ${finalCount?.count || 'unknown'}`);
      
    } catch (error) {
      logger.error(`Bulk import failed: ${error}`);
      throw error;
    }
  }

  static getUserFromSession = getUserFromSession;
  static getOrganizationById = getOrganizationById;

  static async ensureOrgSchema(orgDb: Database): Promise<void> {
    try {
      // Get all existing tables at once
      const existingTables = await orgDb.fetchAll(
        "SELECT name FROM sqlite_master WHERE type='table' AND name IN ('contacts', 'contact_events', 'leads', 'eligibility_answers')"
      );

      // Create a set of existing table names for faster lookup
      const tableSet = new Set(existingTables.map((row: any) => row.name || row[0]));
      
      // Prepare all schema creation statements
      const schemaOperations = [];
      
      // Contacts table
      if (!tableSet.has('contacts')) {
        logger.info('Creating contacts table schema');
        schemaOperations.push({
          sql: `
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
          `,
          args: []
        });
        
        // Indexes for contacts table
        schemaOperations.push({
          sql: `CREATE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email)`,
          args: []
        });
        
        schemaOperations.push({
          sql: `CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email_unique ON contacts(LOWER(TRIM(email)))`,
          args: []
        });
      }
      
      // Contact events table
      if (!tableSet.has('contact_events')) {
        logger.info('Creating contact_events table schema');
        schemaOperations.push({
          sql: `
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
          `,
          args: []
        });
        
        // Indexes for contact_events table
        schemaOperations.push({
          sql: `CREATE INDEX IF NOT EXISTS idx_contact_events_contact_id ON contact_events(contact_id)`,
          args: []
        });
        
        schemaOperations.push({
          sql: `CREATE INDEX IF NOT EXISTS idx_contact_events_lead_id ON contact_events(lead_id)`,
          args: []
        });
        
        schemaOperations.push({
          sql: `CREATE INDEX IF NOT EXISTS idx_contact_events_type ON contact_events(event_type)`,
          args: []
        });
      }
      
      // Leads table
      if (!tableSet.has('leads')) {
        logger.info('Creating leads table schema');
        schemaOperations.push({
          sql: `
            CREATE TABLE IF NOT EXISTS leads (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              name TEXT NOT NULL,
              email TEXT NOT NULL,
              created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
              UNIQUE(email)
            )
          `,
          args: []
        });
        
        // Index for leads table
        schemaOperations.push({
          sql: `CREATE INDEX IF NOT EXISTS idx_leads_email ON leads(email)`,
          args: []
        });
      }
      
      // Eligibility answers table
      if (!tableSet.has('eligibility_answers')) {
        logger.info('Creating eligibility_answers table schema');
        schemaOperations.push({
          sql: `
            CREATE TABLE IF NOT EXISTS eligibility_answers (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              contact_id INTEGER NOT NULL,
              quote_id TEXT NOT NULL,
              answers TEXT NOT NULL,
              created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
              FOREIGN KEY (contact_id) REFERENCES contacts(id)
            )
          `,
          args: []
        });
        
        // Index for eligibility_answers table
        schemaOperations.push({
          sql: `CREATE INDEX IF NOT EXISTS idx_eligibility_answers_contact_id ON eligibility_answers(contact_id)`,
          args: []
        });
      }
      
      // Execute all schema operations in a single batch if there are any
      if (schemaOperations.length > 0) {
        logger.info(`Executing ${schemaOperations.length} schema operations in batch`);
        await orgDb.batch(schemaOperations, 'write');
        logger.info('Schema setup completed successfully');
      } else {
        logger.info('All required tables already exist, no schema changes needed');
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