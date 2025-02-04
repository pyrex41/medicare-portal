import { Database as LibSQLDatabase, Client } from '@libsql/client'
import { config } from './config'
import { logger } from './logger'

export class Database {
  private client: Client

  constructor() {
    if (!config.TURSO_DATABASE_URL || !config.TURSO_AUTH_TOKEN) {
      throw new Error('Missing database credentials in .env file')
    }

    this.client = new LibSQLDatabase({
      url: config.TURSO_DATABASE_URL,
      authToken: config.TURSO_AUTH_TOKEN
    })

    this.initTables()
  }

  private async initTables() {
    await this.client.execute(`
      CREATE TABLE IF NOT EXISTS agents (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        first_name TEXT NOT NULL,
        last_name TEXT NOT NULL,
        email TEXT NOT NULL UNIQUE,
        phone TEXT NOT NULL,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP
      );

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
      );
    `)
  }

  async execute(query: string, params?: any[]) {
    const result = await this.client.execute({ sql: query, args: params })
    return result.rows
  }

  async fetchAll(query: string, params?: any[]) {
    const result = await this.client.execute({ sql: query, args: params })
    return result.rows
  }

  async fetchOne(query: string, params?: any[]) {
    const result = await this.client.execute({ sql: query, args: params })
    return result.rows[0]
  }
} 