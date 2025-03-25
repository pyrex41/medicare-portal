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
   */
  static async bulkImportContacts(orgId: string, csvFilePath: string, overwriteExisting: boolean = false): Promise<void> {
    const timestamp = Date.now();
    const backupDir = path.join(process.cwd(), 'backups');
    const dumpPath = path.join(backupDir, `org-${orgId}-${timestamp}.sql`);
    
    let localDb: BunDatabase | null = null;
    try {
      await fs.promises.mkdir(backupDir, { recursive: true });
      logger.info(`Starting bulk import for org ${orgId}`);
      
      // Step 1: Get existing database credentials and dump
      const mainDb = new Database();
      const org = await mainDb.fetchOne<{ turso_db_url: string; turso_auth_token: string }>(
        'SELECT turso_db_url, turso_auth_token FROM organizations WHERE id = ?',
        [orgId]
      );
      if (!org?.turso_db_url || !org?.turso_auth_token) throw new Error('Organization database not configured');

      const turso = new TursoService();
      logger.info(`Downloading existing dump from ${org.turso_db_url}`);
      const existingDump = await turso.downloadDatabaseDump(org.turso_db_url, org.turso_auth_token);

      // Step 2: Initialize in-memory SQLite with existing data
      localDb = new BunDatabase(':memory:', { create: true });
      logger.info('Loading existing dump into in-memory SQLite');
      
      // If the dump is empty or doesn't contain the contacts table, initialize the schema
      if (!existingDump || !existingDump.includes('CREATE TABLE contacts')) {
        logger.info('No existing schema found, initializing new schema');
        // Create contacts table
        localDb.exec(`
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
          );

          CREATE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email);
          CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email_unique ON contacts(LOWER(TRIM(email)));
        `);
      } else {
        localDb.exec(existingDump);
      }

      // Get the original schema but remove the unique index
      const origTableSchema = localDb.query("SELECT sql FROM sqlite_master WHERE type='table' AND name='contacts'").get() as { sql: string };
      if (!origTableSchema) throw new Error("Contacts table not found");
      
      // Save all contacts
      const allContacts = localDb.query("SELECT * FROM contacts").all();
      
      // Completely drop and recreate contacts table without unique constraints
      localDb.exec("DROP TABLE contacts");
      localDb.exec(origTableSchema.sql);
      
      // Reinsert existing contacts
      if (allContacts.length > 0) {
        // Create a list of column names dynamically from the first contact
        const columnsObj = allContacts[0] as Record<string, any>;
        const columns = Object.keys(columnsObj).filter(col => col !== 'updated_at'); // Exclude updated_at
        
        const insertStmt = localDb.prepare(`
          INSERT INTO contacts (${columns.join(', ')})
          VALUES (${columns.map(() => '?').join(', ')})
        `);
        
        for (const contact of allContacts) {
          const contactObj = contact as Record<string, any>;
          const values = columns.map(col => contactObj[col]);
          insertStmt.run(...values);
        }
      }
      
      const initialCount = localDb.query('SELECT COUNT(*) as count FROM contacts').get() as { count: number };
      logger.info(`Initial contact count: ${initialCount.count}`);
      
      // Optimize for bulk inserts
      localDb.exec('PRAGMA journal_mode = WAL');
      localDb.exec('PRAGMA synchronous = NORMAL');
      
      // Parse CSV
      logger.info(`Reading CSV file: ${csvFilePath}`);
      const csvContent = await fs.promises.readFile(csvFilePath, 'utf-8');
      
      // Parse the CSV content synchronously using the callback API
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
      
      // Log the first record and its keys to help debug column mapping
      if (records.length > 0) {
        const firstRecord = records[0];
        logger.info(`CSV column headers: ${Object.keys(firstRecord).join(', ')}`);
        logger.info(`First record raw data: ${JSON.stringify(firstRecord)}`);
      }
      
      // Create a map of all emails for faster lookup
      const emailMap = new Map<string, boolean>();
      if (allContacts.length > 0) {
        for (const contact of allContacts) {
          const contactObj = contact as Record<string, any>;
          const email = String(contactObj.email).toLowerCase().trim();
          emailMap.set(email, true);
        }
      }
      
      logger.info(`Processing ${records.length} records from CSV`);
      let processed = 0;
      let skipped = 0;
      let added = 0;
      
      // Begin transaction
      localDb.exec('BEGIN TRANSACTION');
      
      // Prepare statement outside the loop
      const insertStmt = localDb.prepare(`
        INSERT INTO contacts (
          first_name, last_name, email, current_carrier, plan_type, effective_date,
          birth_date, tobacco_user, gender, state, zip_code, phone_number, agent_id,
          created_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
      `);
      
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
      
      // Check column names in the first record and log mapping results
      if (records.length > 0) {
        const firstRecord = records[0];
        const recordKeys = Object.keys(firstRecord);
        logger.info(`CSV columns found: ${recordKeys.join(', ')}`);
        
        // Log which columns were mapped and which weren't
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
        
        // Log missing required fields
        const requiredFields = ['first_name', 'last_name', 'email', 'effective_date', 'birth_date'];
        const missingRequired = requiredFields.filter(field => !mappedFields.has(field));
        if (missingRequired.length > 0) {
          logger.warn(`Missing required fields in CSV: ${missingRequired.join(', ')}`);
        }
      }
      
      for (const record of records) {
        processed++;
        
        try {
          // Get normalized field values using the column map
          const getValue = (fieldName: string, defaultValue: string = ''): string => {
            // Try all possible mappings
            for (const [key, mappedField] of Object.entries(columnMap)) {
              if (mappedField === fieldName && record[key] !== undefined) {
                return record[key] || defaultValue;
              }
            }
            // Try direct field name
            return record[fieldName] || defaultValue;
          };
          
          const contact: ContactCreate = {
            first_name: getValue('first_name'),
            last_name: getValue('last_name'),
            email: getValue('email'),
            current_carrier: getValue('current_carrier'),
            plan_type: getValue('plan_type'),
            effective_date: getValue('effective_date'),
            birth_date: getValue('birth_date'),
            tobacco_user: !!(getValue('tobacco_user') === '1' || getValue('tobacco_user').toLowerCase() === 'true'),
            gender: getValue('gender'),
            state: getValue('state'),
            zip_code: getValue('zip_code'),
            phone_number: getValue('phone_number'),
            agent_id: getValue('agent_id') ? Number(getValue('agent_id')) : null,
          };
          
          // Log the mapped contact data for the first few records
          if (processed <= 3) {
            logger.info(`Sample mapped contact data: ${JSON.stringify(contact)}`);
          }
          
          if (!contact.first_name || !contact.last_name || !contact.email || !contact.effective_date || !contact.birth_date) {
            logger.warn(`Skipping record due to missing required fields: ${JSON.stringify(contact)}`);
            skipped++;
            continue;
          }
          
          const email = contact.email.toLowerCase().trim();
          
          // Check if email exists
          if (emailMap.has(email)) {
            if (overwriteExisting) {
              // Delete existing contact
              localDb.prepare('DELETE FROM contacts WHERE LOWER(TRIM(email)) = ?').run(email);
              // Don't increment skipped as we'll add a new record
            } else {
              skipped++;
              continue;
            }
          }
          
          // Add to email map to prevent duplicates within the import itself
          emailMap.set(email, true);
          
          const params = [
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
          ];
          
          insertStmt.run(...params);
          added++;
        } catch (error) {
          logger.warn(`Error processing record: ${error}`);
          skipped++;
        }
      }
      
      localDb.exec('COMMIT');
      
      const finalCount = localDb.query('SELECT COUNT(*) as count FROM contacts').get() as { count: number };
      logger.info(`Processed ${processed} CSV records: ${added} added, ${skipped} skipped. Final count: ${finalCount.count}`);
      
      // Step 4: Save in-memory database to a SQL dump file
      const tempDumpPath = path.join(backupDir, `org-${orgId}-${timestamp}.sql`);
      logger.info(`Creating SQL dump file: ${tempDumpPath}`);
      
      // Create SQL dump from the in-memory database
      const dump = [];
      
      // Add table schema
      const tables = localDb.query("SELECT sql FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%'").all();
      for (const table of tables) {
        dump.push((table as { sql: string }).sql + ';');
      }
      
      // Add indexes
      const indexes = localDb.query("SELECT sql FROM sqlite_master WHERE type='index' AND tbl_name='contacts'").all();
      for (const index of indexes) {
        dump.push((index as { sql: string }).sql + ';');
      }
      
      // Add data
      type Contact = {
        id: number;
        first_name: string;
        last_name: string;
        email: string;
        current_carrier: string;
        plan_type: string;
        effective_date: string;
        birth_date: string;
        tobacco_user: number;
        gender: string;
        state: string;
        zip_code: string;
        phone_number: string;
        agent_id: number | null;
        last_emailed: string | null;
        created_at: string;
      };

      const contacts = localDb.query('SELECT * FROM contacts').all() as Contact[];
      for (const contact of contacts) {
        const values = [
          `'${contact.first_name.replace(/'/g, "''")}'`,
          `'${contact.last_name.replace(/'/g, "''")}'`,
          `'${contact.email.replace(/'/g, "''")}'`,
          `'${contact.current_carrier.replace(/'/g, "''")}'`,
          `'${contact.plan_type.replace(/'/g, "''")}'`,
          `'${contact.effective_date.replace(/'/g, "''")}'`,
          `'${contact.birth_date.replace(/'/g, "''")}'`,
          contact.tobacco_user,
          `'${contact.gender.replace(/'/g, "''")}'`,
          `'${contact.state.replace(/'/g, "''")}'`,
          `'${contact.zip_code.replace(/'/g, "''")}'`,
          `'${contact.phone_number.replace(/'/g, "''")}'`,
          contact.agent_id === null ? 'NULL' : contact.agent_id,
          contact.last_emailed === null ? 'NULL' : `'${contact.last_emailed}'`,
          `'${contact.created_at}'`
        ];
        
        dump.push(`INSERT INTO contacts (
          first_name, last_name, email, current_carrier, plan_type, effective_date,
          birth_date, tobacco_user, gender, state, zip_code, phone_number, agent_id,
          last_emailed, created_at
        ) VALUES (${values.join(', ')});`);
      }
      
      // Write the SQL dump to file
      await fs.promises.writeFile(tempDumpPath, dump.join('\n'));
      logger.info(`SQL dump saved to ${tempDumpPath}`);

      // Step 5: Create new Turso database using the SQL dump
      const newDbName = `org-${orgId}-${timestamp}`;
      logger.info(`Creating new database: ${newDbName}`);

      // First upload the SQL dump file
      logger.info('Uploading SQL dump file');
      const formData = new FormData();
      formData.append('file', new Blob([await fs.promises.readFile(tempDumpPath)], { type: 'text/plain' }), 'dump.sql');

      const uploadResponse = await fetch(`https://api.turso.tech/v1/organizations/${config.TURSO_ORG_SLUG}/databases/dumps`, {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${config.TURSO_API_TOKEN}`,
        },
        body: formData
      });

      if (!uploadResponse.ok) {
        const errorText = await uploadResponse.text();
        logger.error(`Failed to upload SQL dump: ${errorText}`);
        throw new Error(`Failed to upload SQL dump: ${errorText}`);
      }

      const { dump_url } = await uploadResponse.json() as { dump_url: string };
      logger.info(`SQL dump uploaded successfully to ${dump_url}, creating database`);

      // Now create the database using the uploaded dump
      const createResponse = await fetch(`https://api.turso.tech/v1/organizations/${config.TURSO_ORG_SLUG}/databases`, {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${config.TURSO_API_TOKEN}`,
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({
          name: newDbName,
          group: config.TURSO_ORG_GROUP,
          seed: {
            type: 'dump',
            url: dump_url
          }
        })
      });

      if (!createResponse.ok) {
        const errorText = await createResponse.text();
        logger.error(`Failed to create database: ${errorText}`);
        throw new Error(`Failed to create database: ${errorText}`);
      }

      const response = await createResponse.json() as { database: { Hostname: string } };
      // Store database URL in libsql:// format for database connections
      const actualNewUrl = `libsql://${response.database.Hostname}`;
      logger.info(`Database created with URL: ${actualNewUrl}`);

      // Step 6: Get new auth token for the database
      logger.info('Getting new auth token for the database');
      const newTokenResponse = await fetch(`https://api.turso.tech/v1/organizations/${config.TURSO_ORG_SLUG}/databases/${newDbName}/auth/tokens`, {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${config.TURSO_API_TOKEN}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          expiration: 'never',
        }),
      });

      if (!newTokenResponse.ok) {
        const errorText = await newTokenResponse.text();
        logger.error(`Failed to create new token: ${errorText}`);
        throw new Error(`Failed to create new token: ${errorText}`);
      }

      const newTokenData = await newTokenResponse.json() as { jwt: string };
      if (!newTokenData.jwt) {
        logger.error('Failed to get new auth token - missing jwt in response');
        throw new Error('Failed to get new auth token');
      }

      const newToken = newTokenData.jwt;
      logger.info(`Got new auth token (length: ${newToken.length})`);

      // Step 7: Update organization with new URL and token
      await mainDb.execute(
        'UPDATE organizations SET turso_db_url = ?, turso_auth_token = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?',
        [actualNewUrl, newToken, orgId]
      );

      // Verify the update
      const updatedOrg = await mainDb.fetchOne<{ turso_db_url: string, turso_auth_token: string }>(
        'SELECT turso_db_url, turso_auth_token FROM organizations WHERE id = ?',
        [orgId]
      );
      if (!updatedOrg) {
        logger.error('Failed to fetch updated organization after credential update');
        throw new Error('Failed to update organization credentials');
      }
      if (updatedOrg.turso_db_url !== actualNewUrl || updatedOrg.turso_auth_token !== newToken) {
        logger.error('Organization credentials mismatch after update');
        logger.error(`Expected URL: ${actualNewUrl}, got: ${updatedOrg.turso_db_url}`);
        logger.error(`Expected token length: ${newToken.length}, got: ${updatedOrg.turso_auth_token.length}`);
        throw new Error('Organization credentials mismatch after update');
      }

      logger.info(`Successfully updated organization ${orgId} with new database URL and token`);

      // Step 8: Delete old database
      const oldDbName = org.turso_db_url.split('/').pop()?.split('.')[0];
      if (oldDbName) {
        // Extract just the database name without the org slug
        const baseDbName = oldDbName.split('-').slice(0, -1).join('-');
        logger.info(`Deleting old database: ${baseDbName}`);
        await turso.deleteOrganizationDatabase(baseDbName).catch(err => logger.warn(`Failed to delete old DB: ${err}`));
      }
      
      logger.info(`Bulk import completed: ${finalCount.count} contacts in new database`);
    } catch (error) {
      logger.error(`Bulk import failed: ${error}`);
      throw error;
    } finally {
      if (localDb) localDb.close();
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