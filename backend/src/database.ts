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
    // Create organizations table
    await this.client.execute(`
      CREATE TABLE IF NOT EXISTS organizations (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        slug TEXT NOT NULL,
        subscription_tier TEXT NOT NULL DEFAULT 'basic',
        agent_limit INTEGER NOT NULL DEFAULT 5,
        contact_limit INTEGER NOT NULL DEFAULT 100,
        stripe_customer_id TEXT,
        stripe_subscription_id TEXT,
        settings JSON,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        UNIQUE(slug)
      )
    `)

    // Ensure slug column exists and is populated
    try {
      await this.client.execute(`
        UPDATE organizations 
        SET slug = LOWER(REPLACE(name, ' ', '-'))
        WHERE slug IS NULL OR slug = ''
      `)
    } catch (error) {
      logger.error('Error updating organization slugs:', error)
    }

    // Create users table
    await this.client.execute(`
      CREATE TABLE IF NOT EXISTS users (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        email TEXT UNIQUE NOT NULL,
        organization_id INTEGER NOT NULL,
        role TEXT CHECK(role IN ('admin', 'agent')) NOT NULL,
        is_active BOOLEAN DEFAULT true,
        last_login DATETIME,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (organization_id) REFERENCES organizations(id)
      )
    `)

    // Create magic_links table
    await this.client.execute(`
      CREATE TABLE IF NOT EXISTS magic_links (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        token TEXT UNIQUE NOT NULL,
        email TEXT NOT NULL,
        organization_id INTEGER NOT NULL,
        expires_at DATETIME NOT NULL,
        used_at DATETIME,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (organization_id) REFERENCES organizations(id)
      )
    `)

    // Modify existing agents table to include organization
    await this.client.execute(`
      CREATE TABLE IF NOT EXISTS agents (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        organization_id INTEGER NOT NULL,
        first_name TEXT NOT NULL,
        last_name TEXT NOT NULL,
        email TEXT NOT NULL,
        phone TEXT NOT NULL,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (organization_id) REFERENCES organizations(id),
        UNIQUE(email, organization_id)
      )
    `)

    // Modify existing contacts table to include organization
    await this.client.execute(`
      CREATE TABLE IF NOT EXISTS contacts (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        organization_id INTEGER NOT NULL,
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
        FOREIGN KEY (agent_id) REFERENCES agents(id),
        FOREIGN KEY (organization_id) REFERENCES organizations(id)
      )
    `)

    // Create subscription_tiers table
    await this.client.execute(`
      CREATE TABLE IF NOT EXISTS subscription_tiers (
        id TEXT PRIMARY KEY,
        name TEXT NOT NULL,
        agent_limit INTEGER NOT NULL,
        contact_limit INTEGER NOT NULL,
        price_monthly INTEGER NOT NULL,
        price_yearly INTEGER NOT NULL,
        features JSON NOT NULL
      )
    `)

    // Insert default subscription tiers if they don't exist
    await this.client.execute(`
      INSERT OR IGNORE INTO subscription_tiers (id, name, agent_limit, contact_limit, price_monthly, price_yearly, features)
      VALUES 
        ('basic', 'Basic', 5, 100, 2900, 29000, '{"emailAutomation": false, "customBranding": false}'),
        ('pro', 'Professional', 20, 1000, 7900, 79000, '{"emailAutomation": true, "customBranding": false}'),
        ('enterprise', 'Enterprise', 100, 10000, 19900, 199000, '{"emailAutomation": true, "customBranding": true}')
    `)

    // Create sessions table
    await this.client.execute(`
      CREATE TABLE IF NOT EXISTS sessions (
        id TEXT PRIMARY KEY,
        user_id INTEGER NOT NULL,
        expires_at DATETIME NOT NULL,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (user_id) REFERENCES users(id)
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