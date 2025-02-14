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
      return result.rows
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
}

export const db = new Database() 