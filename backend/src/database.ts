import { createClient } from '@libsql/client'
import { config } from './config'
import { logger } from './logger'

export class Database {
  private client: any

  constructor() {
    if (!config.TURSO_DATABASE_URL || !config.TURSO_AUTH_TOKEN) {
      logger.error('Missing database credentials')
      throw new Error('Missing database credentials in .env file')
    }

    this.client = createClient({
      url: config.TURSO_DATABASE_URL,
      authToken: config.TURSO_AUTH_TOKEN,
    })
  }

  getClient() {
    return this.client
  }

  async execute(sql: string, args: any[] = []) {
    try {
      const result = await this.client.execute({
        sql,
        args
      })
      return result
    } catch (error) {
      logger.error(`Database execute error: ${error}`)
      throw error
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

  async fetchOne<T>(sql: string, args: any[] = []): Promise<T | null> {
    try {
      const result = await this.client.execute({
        sql,
        args
      })
      if (!result.rows || result.rows.length === 0) {
        return null
      }
      // Convert array row to object using column names
      const row = result.rows[0]
      const columns = result.columns || []
      const obj: any = {}
      columns.forEach((col: string, i: number) => {
        obj[col] = row[i]
      })
      return obj as T
    } catch (error) {
      logger.error(`Database fetchOne error: ${error}`)
      throw error
    }
  }

  // Compatibility method for old query interface
  async query<T = any>(sql: string, args: any[] = []): Promise<T[]> {
    try {
      const result = await this.client.execute({
        sql,
        args
      })
      return result.rows || []
    } catch (error) {
      logger.error(`Database query error: ${error}`)
      throw error
    }
  }

  // Transaction support with function overloads
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

    const tx = await this.client.transaction(mode)
    try {
      // Create a Database-like wrapper around the transaction
      const txWrapper = new Database()
      // Override the client with the transaction
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

export const db = new Database() 