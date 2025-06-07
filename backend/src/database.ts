import { createClient } from '@libsql/client'
import { config } from './config'
import { logger } from './logger'

export class Database {
  private client: any
  private url: string

  constructor(dbUrl?: string, authToken?: string) {
    const url = dbUrl || config.TURSO_DATABASE_URL
    const token = authToken || config.TURSO_AUTH_TOKEN

    if (!url || !token) {
      logger.error('Missing database credentials')
      throw new Error('Missing database credentials')
    }

    this.url = url
    this.client = createClient({
      url: url,
      authToken: token,
    })
    
    logger.info(`Database connected to: ${this.url}`)
  }

  static async getOrgDb(orgId: string): Promise<Database> {
    try {
      // Use main DB to get org's Turso credentials
      const mainDb = new Database()
      const org = await mainDb.fetchOne<{turso_db_url: string, turso_auth_token: string}>(
        'SELECT turso_db_url, turso_auth_token FROM organizations WHERE id = ?',
        [orgId]
      )

      if (!org?.turso_db_url || !org?.turso_auth_token) {
        logger.error(`No Turso credentials found for org ${orgId}`)
        throw new Error('Organization database not configured')
      }

      logger.info(`Creating client for org ${orgId} database: ${org.turso_db_url}`)
      return new Database(org.turso_db_url, org.turso_auth_token)
    } catch (error) {
      logger.error(`Failed to get org database: ${error}`)
      throw error
    }
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

  // Bulk import method that properly handles temporary database validation
  async bulkImportContactsSQLite(tempDbPath: string, orgId: string): Promise<void> {
    try {
      // Get the organization's database instance for the final import
      const orgDb = await Database.getOrgDb(orgId)
      
      // Create a temporary database instance specifically for the import file
      // This ensures schema validation happens on the correct database
      const tempDbUrl = `file://${tempDbPath}`
      const tempDb = new Database(tempDbUrl, '') // Empty auth token for local file
      
      // Validate schema on the temporary database (not the live one)
      await this.validateImportSchema(tempDb)
      
      // Process the import data from the temporary database
      const importData = await tempDb.fetchAll('SELECT * FROM contacts_import')
      
      // Import into the organization's live database
      await orgDb.transaction(async (tx) => {
        for (const contact of importData) {
          await tx.execute(
            'INSERT INTO contacts (first_name, last_name, email, phone, organization_id) VALUES (?, ?, ?, ?, ?)',
            [contact.first_name, contact.last_name, contact.email, contact.phone, orgId]
          )
        }
      })
      
      logger.info(`Successfully imported ${importData.length} contacts for org ${orgId}`)
    } catch (error) {
      logger.error(`Bulk import failed for org ${orgId}: ${error}`)
      throw error
    }
  }
  
  private async validateImportSchema(tempDb: Database): Promise<void> {
    try {
      // Validate that the temporary database has the expected schema
      const tables = await tempDb.fetchAll(
        "SELECT name FROM sqlite_master WHERE type='table' AND name='contacts_import'"
      )
      
      if (tables.length === 0) {
        throw new Error('Import database missing required contacts_import table')
      }
      
      // Validate required columns exist
      const columns = await tempDb.fetchAll(
        "PRAGMA table_info(contacts_import)"
      )
      
      const requiredColumns = ['first_name', 'last_name', 'email']
      const existingColumns = columns.map((col: any) => col.name)
      
      for (const required of requiredColumns) {
        if (!existingColumns.includes(required)) {
          throw new Error(`Missing required column: ${required}`)
        }
      }
      
      logger.info('Import schema validation passed')
    } catch (error) {
      logger.error(`Schema validation failed: ${error}`)
      throw error
    }
  }
}

export const db = new Database() 