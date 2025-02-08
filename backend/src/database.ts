import { createClient } from '@libsql/client'
import { config } from './config'
import { logger } from './logger'
import { readFileSync, readdirSync } from 'fs'
import path from 'path'

export class Database {
  private client: any
  private initialized = false
  private connection: any = null  // Store the connection for transactions

  constructor() {
    if (!config.TURSO_DATABASE_URL || !config.TURSO_AUTH_TOKEN) {
      console.error('Config values:', config)
      throw new Error('Missing database credentials in .env file')
    }

    this.client = createClient({
      url: config.TURSO_DATABASE_URL,
      authToken: config.TURSO_AUTH_TOKEN,
    })
  }

  async init() {
    if (this.initialized) {
      return
    }

    try {
      logger.info(`Initializing database with URL: ${config.TURSO_DATABASE_URL}`)
      logger.info(`Using auth token: ${config.TURSO_AUTH_TOKEN}`)

      // Create tables if they don't exist
      await this.client.execute(`
        CREATE TABLE IF NOT EXISTS organizations (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          name TEXT NOT NULL UNIQUE,
          turso_db_url TEXT,
          turso_auth_token TEXT,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP
        );
      `)

      await this.client.execute(`
        CREATE TABLE IF NOT EXISTS users (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          email TEXT NOT NULL UNIQUE,
          first_name TEXT NOT NULL,
          last_name TEXT NOT NULL,
          organization_id INTEGER NOT NULL,
          role TEXT NOT NULL,
          is_active BOOLEAN NOT NULL DEFAULT false,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
          FOREIGN KEY (organization_id) REFERENCES organizations(id)
        );
      `)

      await this.client.execute(`
        CREATE TABLE IF NOT EXISTS verification_tokens (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          token TEXT NOT NULL UNIQUE,
          email TEXT NOT NULL,
          organization_id INTEGER NOT NULL,
          expires_at DATETIME NOT NULL,
          created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
          FOREIGN KEY (organization_id) REFERENCES organizations(id)
        );
      `)

      logger.info('Database client created successfully')
      this.initialized = true
    } catch (error) {
      logger.error('Failed to initialize database tables:', error)
      throw error
    }
  }

  async execute<T = any>(query: string, params: any[] = []): Promise<T> {
    if (!this.initialized) {
      await this.init()
    }
    const result = await this.client.execute({ sql: query, args: params })
    return result.rows[0] as T
  }

  async query<T = any>(query: string, params: any[] = []): Promise<T[]> {
    if (!this.initialized) {
      await this.init()
    }
    const result = await this.client.execute({ sql: query, args: params })
    return result.rows as T[]
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

  async runMigrations() {
    try {
      // Create migrations table if it doesn't exist
      await this.execute(`
        CREATE TABLE IF NOT EXISTS migrations (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          name TEXT NOT NULL UNIQUE,
          executed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
      `)

      // Get list of executed migrations
      const executed = await this.fetchAll('SELECT name FROM migrations')
      const executedNames = new Set(executed.map(row => row[0]))

      // Read migration files from migrations directory
      const migrationsDir = path.join(__dirname, '../../migrations')
      const files = readdirSync(migrationsDir)
        .filter(f => f.endsWith('.sql'))
        .sort()

      // Run pending migrations
      for (const file of files) {
        if (!executedNames.has(file)) {
          const sql = readFileSync(path.join(migrationsDir, file), 'utf-8')
          await this.execute(sql)
          await this.execute('INSERT INTO migrations (name) VALUES (?)', [file])
          console.log(`Executed migration: ${file}`)
        }
      }
    } catch (e) {
      console.error('Migration error:', e)
      throw e
    }
  }

  async transaction<T>(callback: (db: Database) => Promise<T>): Promise<T> {
    if (!this.initialized) {
      await this.init()
    }

    const transaction = await this.client.transaction('write')
    
    try {
      const result = await callback(this)
      await transaction.commit()
      return result
    } catch (error) {
      try {
        await transaction.rollback()
      } catch (rollbackError) {
        logger.error('Error rolling back transaction:', rollbackError)
      }
      throw error
    } finally {
      await transaction.close()
    }
  }
}

export const db = new Database() 