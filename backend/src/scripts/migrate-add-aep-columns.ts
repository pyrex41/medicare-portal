import { Database } from '../database';
import { logger } from '../logger';

async function migrateAddAepColumns() {
  try {
    logger.info('Starting migration to add AEP request columns to all org databases');
    
    // Connect to main database
    const mainDb = new Database();
    
    // Get all organization IDs, database URLs and tokens
    const orgs = await mainDb.fetchAll(
      'SELECT id, turso_db_url, turso_auth_token FROM organizations WHERE turso_db_url IS NOT NULL'
    );
    
    logger.info(`Found ${orgs.length} organizations with databases to migrate`);
    
    for (const org of orgs) {
      const orgId = org.id || org[0];
      const dbUrl = org.turso_db_url || org[1];
      const authToken = org.turso_auth_token || org[2];
      
      if (!dbUrl || !authToken) {
        logger.warn(`Skipping org ${orgId} due to missing database credentials`);
        continue;
      }
      
      logger.info(`Migrating org ${orgId} with database URL ${dbUrl}`);
      
      try {
        // Connect to org database
        const orgDb = new Database(dbUrl, authToken);
        
        // Check if columns already exist
        const tableInfo = await orgDb.fetchAll("PRAGMA table_info(contacts)");
        const columns = tableInfo.map((col: any) => col.name || col[1]);
        
        if (!columns.includes('aep_request')) {
          logger.info(`Adding aep_request column to contacts table for org ${orgId}`);
          await orgDb.execute('ALTER TABLE contacts ADD COLUMN aep_request BOOLEAN DEFAULT FALSE');
        } else {
          logger.info(`aep_request column already exists for org ${orgId}`);
        }
        
        if (!columns.includes('aep_request_date')) {
          logger.info(`Adding aep_request_date column to contacts table for org ${orgId}`);
          await orgDb.execute('ALTER TABLE contacts ADD COLUMN aep_request_date DATETIME');
        } else {
          logger.info(`aep_request_date column already exists for org ${orgId}`);
        }
        
        logger.info(`Successfully migrated org ${orgId}`);
      } catch (error) {
        logger.error(`Error migrating org ${orgId}: ${error}`);
      }
    }
    
    logger.info('Migration completed');
  } catch (error) {
    logger.error(`Migration failed: ${error}`);
    throw error;
  }
}

// Execute migration if this file is run directly
if (import.meta.main) {
  migrateAddAepColumns()
    .then(() => {
      logger.info('Migration script completed successfully');
      process.exit(0);
    })
    .catch((error) => {
      logger.error(`Migration script failed: ${error}`);
      process.exit(1);
    });
}

export { migrateAddAepColumns }; 