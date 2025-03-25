import { Database } from '../backend/src/database'
import { config } from '../backend/src/config'
import { logger } from '../backend/src/logger'
import path from 'path'
import fs from 'fs/promises'
import { Database as BunDatabase } from 'bun:sqlite'

// Set required environment variables for local SQLite
process.env.USE_LOCAL_SQLITE = 'true'
process.env.LOCAL_DB_PATH = process.env.LOCAL_DB_PATH || 'data/organizations'

// Define table dependencies
const TABLE_ORDER = [
  'leads',
  'contacts',
  'eligibility_answers',
  'contact_events',
  'contact_requests'
]

async function migrateToSqlite() {
  try {
    // Ensure local DB directory exists
    const dbDir = path.join(process.cwd(), 'data/organizations')
    await fs.mkdir(dbDir, { recursive: true })
    
    // Get main database connection with Turso (force Turso for source)
    const mainDb = new Database(config.TURSO_DATABASE_URL, config.TURSO_AUTH_TOKEN)
    
    // Get all organizations
    const orgs = await mainDb.fetchAll('SELECT id, turso_db_url, turso_auth_token FROM organizations')
    logger.info(`Found ${orgs.length} organizations to migrate`)
    
    for (const org of orgs) {
      try {
        if (!org.turso_db_url || !org.turso_auth_token) {
          logger.warn(`Skipping organization ${org.id} - missing database credentials`)
          continue
        }

        const { dbName } = Database.normalizeDbUrl(org.turso_db_url)
        if (!dbName) {
          logger.warn(`Skipping organization ${org.id} - invalid database URL format`)
          continue
        }

        const localDbPath = path.join(dbDir, `${dbName}.sqlite`)
        
        // Check if database already exists
        const dbExists = await fs.access(localDbPath).then(() => true).catch(() => false)
        if (dbExists) {
          logger.info(`Database already exists for org ${org.id}, removing it first`)
          await fs.unlink(localDbPath)
        }
        
        logger.info(`Migrating organization ${org.id} to ${localDbPath}`)
        
        // Get Turso database connection (force Turso for source)
        const tursoDb = new Database(org.turso_db_url, org.turso_auth_token)
        
        // Get schema and data
        const tables = await tursoDb.fetchAll(`
          SELECT name, sql 
          FROM sqlite_master 
          WHERE type='table' AND name NOT LIKE 'sqlite_%'
        `)
        
        if (!tables || tables.length === 0) {
          logger.warn(`No tables found for organization ${org.id}`)
          continue
        }

        logger.info(`Found ${tables.length} tables to migrate`)

        // Create local database file
        const localDb = new BunDatabase(localDbPath)
        
        // Enable foreign keys
        localDb.exec('PRAGMA foreign_keys = ON;')
        
        // First create all tables
        for (const table of tables) {
          if (!table.name || !table.sql) {
            logger.warn(`Skipping invalid table definition for org ${org.id}`)
            continue
          }

          logger.info(`Creating table: ${table.name}`)
          try {
            localDb.exec(table.sql)
          } catch (error) {
            logger.error(`Error creating table ${table.name}: ${error}`)
            throw error
          }
        }

        // Create indexes
        const indexes = await tursoDb.fetchAll(`
          SELECT sql 
          FROM sqlite_master 
          WHERE type='index' AND sql IS NOT NULL
        `)
        
        for (const index of indexes) {
          try {
            localDb.exec(index.sql)
          } catch (error) {
            logger.warn(`Error creating index: ${error}`)
            // Continue even if index creation fails
          }
        }

        // Then copy data for all tables in dependency order
        const tableMap = new Map(tables.map(t => [t.name, t]))
        for (const tableName of TABLE_ORDER) {
          const table = tableMap.get(tableName)
          if (!table) continue // Skip if table doesn't exist in this org's schema

          // Copy data
          const data = await tursoDb.fetchAll(`SELECT * FROM ${tableName}`)
          if (data.length > 0) {
            logger.info(`Copying ${data.length} rows for table ${tableName}`)
            
            const columns = Object.keys(data[0])
            const placeholders = columns.map(() => '?').join(',')
            const sql = `INSERT INTO ${tableName} (${columns.join(',')}) VALUES (${placeholders})`
            
            // Use transaction for better performance
            localDb.exec('BEGIN TRANSACTION')
            
            try {
              const stmt = localDb.prepare(sql)
              for (const row of data) {
                stmt.run(...Object.values(row))
              }
              localDb.exec('COMMIT')
            } catch (error) {
              localDb.exec('ROLLBACK')
              logger.error(`Error inserting data for table ${tableName} in org ${org.id}: ${error}`)
              throw error
            }
          }
        }
        
        // Close the local database
        localDb.close()
        
        logger.info(`Successfully migrated organization ${org.id}`)
      } catch (error) {
        logger.error(`Error migrating organization ${org.id}: ${error}`)
        // Continue with next organization instead of failing completely
        continue
      }
    }
    
    logger.info('Migration complete')
  } catch (error) {
    logger.error(`Migration failed: ${error}`)
    process.exit(1)
  }
}

migrateToSqlite() 