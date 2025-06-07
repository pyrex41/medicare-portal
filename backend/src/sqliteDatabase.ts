import { Database as BunDatabase } from 'bun:sqlite';
import { litestreamManager } from './services/litestreamManager';
import { logger } from './logger';

export class SQLiteDatabase {
  private orgId: string;
  private dbInstance: BunDatabase | null = null;
  private dbPath: string | null = null;

  constructor(orgId: string) {
    this.orgId = orgId;
  }

  // A private method to ensure the DB is initialized
  private async getDb(): Promise<BunDatabase> {
    if (this.dbInstance) {
      return this.dbInstance;
    }

    // Ask the manager for the path. This will trigger restore/replication if needed.
    this.dbPath = await litestreamManager.getDbPath(this.orgId);
    
    // The manager ensures the file exists (or will be created), so we can connect.
    this.dbInstance = new BunDatabase(this.dbPath);
    this.dbInstance.exec('PRAGMA journal_mode = WAL;');
    this.dbInstance.exec('PRAGMA foreign_keys = ON;');
    
    // This is a new database file, we need to ensure the schema is applied
    await SQLiteDatabase.ensureOrgSchema(this);

    return this.dbInstance;
  }
  
  // Public methods now use the getter
  async query<T>(sql: string, params: any[] = []): Promise<T[]> {
    const db = await this.getDb();
    try {
      return db.prepare(sql).all(...params) as T[];
    } catch (error) {
      logger.error(`[SQLiteDB|Org ${this.orgId}] Query error: ${error instanceof Error ? error.message : String(error)}`);
      logger.error(`[SQLiteDB|Org ${this.orgId}] SQL: ${sql}`);
      logger.error(`[SQLiteDB|Org ${this.orgId}] Params: ${JSON.stringify(params)}`);
      throw error;
    }
  }

  async execute(sql: string, params: any[] = []): Promise<any> {
    const db = await this.getDb();
    try {
      return db.prepare(sql).run(...params);
    } catch (error) {
      logger.error(`[SQLiteDB|Org ${this.orgId}] Execute error: ${error instanceof Error ? error.message : String(error)}`);
      logger.error(`[SQLiteDB|Org ${this.orgId}] SQL: ${sql}`);
      logger.error(`[SQLiteDB|Org ${this.orgId}] Params: ${JSON.stringify(params)}`);
      throw error;
    }
  }

  async fetchOne<T>(sql: string, params: any[] = []): Promise<T | null> {
    const db = await this.getDb();
    try {
      const result = db.prepare(sql).get(...params);
      return result as T | null;
    } catch (error) {
      logger.error(`[SQLiteDB|Org ${this.orgId}] FetchOne error: ${error instanceof Error ? error.message : String(error)}`);
      logger.error(`[SQLiteDB|Org ${this.orgId}] SQL: ${sql}`);
      logger.error(`[SQLiteDB|Org ${this.orgId}] Params: ${JSON.stringify(params)}`);
      throw error;
    }
  }

  async fetchAll<T>(sql: string, params: any[] = []): Promise<T[]> {
    return this.query<T>(sql, params);
  }

  async batch(statements: { sql: string, args: any[] }[], mode: 'read' | 'write' = 'write'): Promise<any[]> {
    const db = await this.getDb();
    
    try {
      // SQLite doesn't have a direct batch method, so we'll execute in a transaction
      const transaction = db.transaction((stmts: { sql: string, args: any[] }[]) => {
        const results: any[] = [];
        for (const stmt of stmts) {
          if (stmt.sql.trim().toUpperCase().startsWith('SELECT')) {
            results.push(db.prepare(stmt.sql).all(...stmt.args));
          } else {
            results.push(db.prepare(stmt.sql).run(...stmt.args));
          }
        }
        return results;
      });

      return transaction(statements);
    } catch (error) {
      logger.error(`[SQLiteDB|Org ${this.orgId}] Batch error: ${error instanceof Error ? error.message : String(error)}`);
      throw error;
    }
  }

  async transaction<T>(callbackOrMode: ((tx: SQLiteDatabase) => T) | 'read' | 'write', callback?: (tx: SQLiteDatabase) => T): Promise<T> {
    const db = await this.getDb();
    
    let actualCallback: (tx: SQLiteDatabase) => T;
    
    if (typeof callbackOrMode === 'function') {
      actualCallback = callbackOrMode;
    } else if (callback) {
      actualCallback = callback;
    } else {
      throw new Error('Invalid transaction arguments');
    }

    try {
      // Create a transaction wrapper - Bun's transaction expects a synchronous function
      const transaction = db.transaction(() => {
        return actualCallback(this);
      });

      return transaction();
    } catch (error) {
      logger.error(`[SQLiteDB|Org ${this.orgId}] Transaction error: ${error instanceof Error ? error.message : String(error)}`);
      throw error;
    }
  }

  // The static method for schema creation
  static async ensureOrgSchema(db: SQLiteDatabase): Promise<void> {
    const instance = await db.getDb(); // Get the actual BunDatabase instance
    
    logger.info(`[SQLiteDB|Org ${db.orgId}] Ensuring organization schema`);

    try {
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
            FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
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
            contact_id INTEGER NOT NULL,
            url TEXT NOT NULL,
            user_agent TEXT,
            ip_address TEXT,
            referer TEXT,
            click_count INTEGER DEFAULT 1,
            first_clicked_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            last_clicked_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
          )`,
          indexStatements: [
            `CREATE INDEX IF NOT EXISTS idx_tracking_clicks_contact_id ON tracking_clicks(contact_id)`,
            `CREATE INDEX IF NOT EXISTS idx_tracking_clicks_url ON tracking_clicks(url)`
          ]
        },
        {
          name: 'leads',
          createStatement: `CREATE TABLE IF NOT EXISTS leads (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            first_name TEXT NOT NULL,
            last_name TEXT NOT NULL,
            email TEXT NOT NULL,
            phone_number TEXT,
            state TEXT,
            zip_code TEXT,
            agent_id INTEGER,
            status TEXT DEFAULT 'new',
            source TEXT,
            notes TEXT,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
          )`,
          indexStatements: [
            `CREATE INDEX IF NOT EXISTS idx_leads_email ON leads(email)`,
            `CREATE INDEX IF NOT EXISTS idx_leads_agent_id ON leads(agent_id)`,
            `CREATE INDEX IF NOT EXISTS idx_leads_status ON leads(status)`
          ]
        }
      ];

      // Create all tables and indexes
      for (const table of tables) {
        logger.info(`[SQLiteDB|Org ${db.orgId}] Creating table: ${table.name}`);
        instance.exec(table.createStatement);

        // Create indexes
        for (const indexStatement of table.indexStatements) {
          instance.exec(indexStatement);
        }
      }

      logger.info(`[SQLiteDB|Org ${db.orgId}] Schema initialization complete`);
    } catch (error) {
      logger.error(`[SQLiteDB|Org ${db.orgId}] Schema initialization error: ${error instanceof Error ? error.message : String(error)}`);
      throw error;
    }
  }

  // Get the organization ID for this database
  getOrgId(): string {
    return this.orgId;
  }

  // Close the database connection
  close() {
    if (this.dbInstance) {
      this.dbInstance.close();
      this.dbInstance = null;
      this.dbPath = null;
    }
  }
}