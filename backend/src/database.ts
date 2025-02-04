import { createClient, type Client } from '@libsql/client'
import { config } from './config'
import { logger } from './logger'

export class Database {
  private client: Client

  constructor() {
    if (!config.TURSO_DATABASE_URL || !config.TURSO_AUTH_TOKEN) {
      console.error('Config values:', config)
      throw new Error('Missing database credentials in .env file')
    }

    // Use template literals to ensure values are included in the string
    logger.info(`Initializing database with URL: ${config.TURSO_DATABASE_URL}`)
    logger.info(`Using auth token: ${config.TURSO_AUTH_TOKEN}`)

    try {
      this.client = createClient({
        url: "file:" + config.TURSO_DATABASE_PATH,
        syncUrl: config.TURSO_DATABASE_URL,
        authToken: config.TURSO_AUTH_TOKEN,
        offline: true,
      })
      logger.info('Database client created successfully')
    } catch (error) {
      logger.error(`Failed to create database client: ${error}`)
      throw error
    }
  }

  // Initialize separately since constructor can't be async
  async init() {
    try {
      await this.initTables()
      logger.info('Database tables initialized successfully')
      await this.client.sync();
      logger.info('Database synced successfully')

      // Check contact count
      const countResult = await this.client.execute('SELECT COUNT(*) as count FROM contacts')
      const count = countResult.rows[0].count
      logger.info(`Current number of contacts in database: ${count}`)

      // If count is 0, log a message
      if (count === 0) {
        logger.info('Database is empty - ready for new contacts')
      }

    } catch (error) {
      logger.error(`Failed to initialize database tables: ${error}`)
      throw error
    }
  }

  private async initTables() {
    // Create agents table
    await this.client.execute(`
      CREATE TABLE IF NOT EXISTS agents (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        first_name TEXT NOT NULL,
        last_name TEXT NOT NULL,
        email TEXT NOT NULL UNIQUE,
        phone TEXT NOT NULL,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP
      )
    `)

    // Create contacts table
    await this.client.execute(`
      CREATE TABLE IF NOT EXISTS contacts (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        first_name TEXT NOT NULL,
        last_name TEXT NOT NULL,
        email TEXT NOT NULL,
        current_carrier TEXT,
        plan_type TEXT,
        effective_date DATE,
        birth_date DATE,
        tobacco_user BOOLEAN DEFAULT FALSE,
        gender TEXT,
        state TEXT,
        zip_code TEXT,
        agent_id INTEGER,
        last_emailed DATETIME,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (agent_id) REFERENCES agents(id)
      )
    `)
  }

  async execute(sql: string, args: any[] = []) {
    try {
      const result = await this.client.execute({
        sql,
        args
      })
      await this.client.sync();
      return result.rows
    } catch (error) {
      logger.error(`Database execute error: ${error}`)
      throw error
    }
  }

  async executeMany(statements: (string | { sql: string, args?: any[] })[]) {
    try {
      const results = await this.client.batch(statements, "write");
      await this.client.sync();
      return results.map(result => result.rows);
    } catch (error) {
      logger.error(`Database executeMany error: ${error}`);
      throw error;
    }
  }

  async fetchAll(sql: string, args: any[] = []) {
    try {
      const result = await this.client.execute({
        sql,
        args
      })
      return result.rows || []
    } catch (error) {
      logger.error(`Database fetchAll error: ${error}`)
      throw error
    }
  }

  async fetchOne(sql: string, args: any[] = []) {
    try {
      const result = await this.client.execute({
        sql,
        args
      })
      return result.rows[0] || null
    } catch (error) {
      logger.error(`Database fetchOne error: ${error}`)
      throw error
    }
  }
} 