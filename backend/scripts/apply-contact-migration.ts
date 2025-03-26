#!/usr/bin/env bun

import { Database } from '../src/database';
import { logger } from '../src/logger';
import fs from 'fs/promises';
import path from 'path';

/**
 * This script applies the contact email uniqueness migration to all organization databases.
 * It:
 * 1. Fetches all organizations from the main database
 * 2. For each organization, connects to its database
 * 3. Applies the migration to ensure contacts have unique emails
 * 4. Reports counts before and after deduplication
 */
async function main() {
  try {
    logger.info('Starting contact migration script');
    
    // Connect to main database
    const mainDb = new Database();
    
    // Get all organizations
    const orgs = await mainDb.fetchAll('SELECT id, name, turso_db_url, turso_auth_token FROM organizations WHERE turso_db_url IS NOT NULL');
    logger.info(`Found ${orgs.length} organizations with databases`);
    
    // Read migration SQL
    const migrationPath = path.join(process.cwd(), 'migrations', '20240326_fix_contact_email_unique.sql');
    const migrationSql = await fs.readFile(migrationPath, 'utf-8');
    
    // Process each organization
    for (let i = 0; i < orgs.length; i++) {
      const org = orgs[i];
      const orgId = org.id;
      const orgName = org.name;
      const dbUrl = org.turso_db_url;
      const authToken = org.turso_auth_token;
      
      logger.info(`Processing organization ${i+1}/${orgs.length}: ${orgName} (ID: ${orgId})`);
      
      try {
        // Connect to organization database
        const orgDb = new Database(dbUrl, authToken);
        
        // Check if contacts table exists
        const tableExists = await orgDb.fetchOne(
          "SELECT 1 FROM sqlite_master WHERE type='table' AND name='contacts'"
        );
        
        if (!tableExists) {
          logger.info(`No contacts table found for organization ${orgId}, skipping`);
          continue;
        }
        
        // Get contacts count before migration
        const beforeCount = await orgDb.fetchOne<{count: number}>('SELECT COUNT(*) as count FROM contacts');
        logger.info(`Organization ${orgId} has ${beforeCount?.count || 0} contacts before deduplication`);
        
        // Get duplicate emails count
        const duplicatesCount = await orgDb.fetchOne<{count: number}>(
          'SELECT COUNT(*) - COUNT(DISTINCT LOWER(TRIM(email))) as count FROM contacts'
        );
        logger.info(`Organization ${orgId} has ${duplicatesCount?.count || 0} duplicate emails`);
        
        if ((duplicatesCount?.count || 0) > 0) {
          // Apply migration
          await orgDb.execute(migrationSql);
          
          // Get contacts count after migration
          const afterCount = await orgDb.fetchOne<{count: number}>('SELECT COUNT(*) as count FROM contacts');
          logger.info(`Organization ${orgId} has ${afterCount?.count || 0} contacts after deduplication`);
          logger.info(`Removed ${(beforeCount?.count || 0) - (afterCount?.count || 0)} duplicate contacts`);
        } else {
          logger.info(`No duplicates found for organization ${orgId}, skipping migration`);
        }
      } catch (error) {
        logger.error(`Error processing organization ${orgId}: ${error}`);
      }
    }
    
    logger.info('Contact migration completed');
  } catch (error) {
    logger.error(`Error in migration script: ${error}`);
    process.exit(1);
  }
}

main();