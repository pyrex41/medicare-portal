#!/usr/bin/env node

/**
 * Migration script to add tracking_clicks table to all organization databases
 * Usage: node migrate-tracking-clicks.js
 */

const fs = require('fs');
const path = require('path');
const { Database } = require('../dist/database');
const { logger } = require('../dist/logger');

// Path to the SQL migration
const MIGRATION_FILE = path.join(__dirname, '../data/migrations/003_add_tracking_clicks.sql');

async function main() {
  try {
    // Read the migration SQL
    const migrationSql = fs.readFileSync(MIGRATION_FILE, 'utf-8');
    logger.info(`Loaded migration SQL from ${MIGRATION_FILE}`);
    
    // Initialize the main database
    const db = new Database();
    logger.info('Connected to main database');

    // Get all organization IDs and their database URLs
    const orgs = await db.fetchAll(
      'SELECT id, name, slug, turso_db_url, turso_auth_token FROM organizations WHERE turso_db_url IS NOT NULL AND turso_auth_token IS NOT NULL'
    );
    
    logger.info(`Found ${orgs.length} organizations with database configurations`);

    // Process each organization
    for (const org of orgs) {
      const { id, name, slug, turso_db_url, turso_auth_token } = org;
      
      try {
        logger.info(`Migrating organization: ${name} (${id})`);
        
        // Connect to the organization's database
        const orgDb = new Database(turso_db_url, turso_auth_token);
        
        // Execute the migration SQL
        await orgDb.execute(migrationSql);
        
        // Verify the table was created
        const tableExists = await orgDb.fetchOne(
          "SELECT name FROM sqlite_master WHERE type='table' AND name='tracking_clicks'"
        );
        
        if (tableExists) {
          logger.info(`Successfully migrated organization ${name} (${id})`);
        } else {
          logger.error(`Migration failed for organization ${name} (${id}): table not created`);
        }
      } catch (error) {
        logger.error(`Error migrating organization ${name} (${id}): ${error.message}`);
      }
    }
    
    logger.info('Migration complete!');
    process.exit(0);
  } catch (error) {
    logger.error(`Migration failed: ${error.message}`);
    logger.error(error.stack);
    process.exit(1);
  }
}

// Start the migration
main(); 