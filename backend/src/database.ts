import { createClient } from '@libsql/client'
import { config } from './config'
import { logger } from './logger'
import { TursoService } from './services/turso'

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

  /**
   * Get organization's database or initialize it if it doesn't exist
   * This method is used as a fallback when the database needs to be created on the fly
   */
  static async getOrInitOrgDb(orgId: string): Promise<Database> {
    try {
      // First try to get the existing database
      const db = await Database.getOrgDb(orgId)
      
      // Ensure schema is up to date
      await Database.ensureDatabaseSchema(orgId)
      
      return db
    } catch (error) {
      // If the error is that the database doesn't exist, initialize it
      if (error instanceof Error && error.message === 'Organization database not configured') {
        logger.info(`No database found for org ${orgId}, initializing new database`)
        
        try {
          // Use main DB to check if the organization exists
          const mainDb = new Database()
          const orgExists = await mainDb.fetchOne<{id: number}>(
            'SELECT id FROM organizations WHERE id = ?',
            [orgId]
          )

          if (!orgExists) {
            logger.error(`Cannot initialize database: Organization ${orgId} does not exist`)
            throw new Error('Organization not found')
          }

          // Create the database using the TursoService
          const turso = new TursoService()
          const { url, token } = await turso.createOrganizationDatabase(orgId)

          // Update the organization record with the new database credentials
          await mainDb.execute(
            'UPDATE organizations SET turso_db_url = ?, turso_auth_token = ? WHERE id = ?',
            [url, token, orgId]
          )

          logger.info(`Successfully initialized database for organization ${orgId}`)
          
          // Return a new database instance with the created credentials
          return new Database(url, token)
        } catch (initError) {
          logger.error(`Failed to initialize database for org ${orgId}: ${initError}`)
          throw initError
        }
      } else {
        // If it's some other error, rethrow it
        throw error
      }
    }
  }

  /**
   * Ensure that the organization database has all required tables
   * This can be used to add new tables to existing databases when the schema changes
   */
  static async ensureDatabaseSchema(orgId: string): Promise<void> {
    try {
      // Get the organization database
      const orgDb = await Database.getOrgDb(orgId)
      
      // Check for existence of each table and create if missing
      const tables = [
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
        }
        // Add more tables here as needed
      ]
      
      // Execute each table creation statement
      for (const table of tables) {
        try {
          // Check if table exists
          const tableExists = await orgDb.fetchOne<{cnt: number}>(
            `SELECT COUNT(*) as cnt FROM sqlite_master WHERE type='table' AND name=?`,
            [table.name]
          )
          
          if (!tableExists || (tableExists.cnt === 0)) {
            logger.info(`Creating missing table ${table.name} for org ${orgId}`)
            await orgDb.execute(table.createStatement)
            
            // Create indexes
            for (const indexStatement of table.indexStatements) {
              await orgDb.execute(indexStatement)
            }
          }
        } catch (e) {
          logger.error(`Error checking/creating table ${table.name}: ${e}`)
          // Continue to next table
        }
      }
    } catch (error) {
      logger.error(`Error ensuring database schema for org ${orgId}: ${error}`)
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
}

export const db = new Database() 