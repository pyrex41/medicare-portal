#!/usr/bin/env bun
/**
 * Script to apply contact deduplication and index creation to all organization databases
 * Usage: bun run apply-contact-migration.ts
 */

import { Database } from '../src/database'
import { logger } from '../src/logger'
import fs from 'fs/promises'
import path from 'path'

async function main() {
  try {
    logger.info('Starting contact migration script')
    
    // Connect to main database
    const mainDb = new Database()
    
    // Get all organizations
    const organizations = await mainDb.fetchAll('SELECT id, name, turso_db_url, turso_auth_token FROM organizations WHERE turso_db_url IS NOT NULL')
    logger.info(`Found ${organizations.length} organizations to process`)
    
    // Read the migration SQL file
    const migrationPath = path.join(process.cwd(), 'migrations', '20240326_deduplicate_contacts.sql')
    
    try {
      await fs.access(migrationPath)
    } catch (error) {
      logger.error(`Migration file not found: ${migrationPath}`)
      process.exit(1)
    }
    
    const migrationSql = await fs.readFile(migrationPath, 'utf-8')
    logger.info(`Loaded migration from: ${migrationPath}`)
    
    // Process each organization
    for (let i = 0; i < organizations.length; i++) {
      const org = organizations[i]
      const orgId = org.id
      const orgName = org.name
      const dbUrl = org.turso_db_url
      const authToken = org.turso_auth_token
      
      logger.info(`Processing organization ${i+1}/${organizations.length}: ${orgName} (ID: ${orgId})`)
      
      try {
        // Connect to the organization's database
        const orgDb = new Database(dbUrl, authToken)
        
        // Check if the contacts table exists
        const tableExists = await orgDb.fetchOne('SELECT name FROM sqlite_master WHERE type="table" AND name="contacts"')
        
        if (!tableExists) {
          logger.info(`Organization ${orgId} does not have a contacts table, skipping`)
          continue
        }
        
        // Get contact count before migration
        const beforeCount = await orgDb.fetchOne<{ count: number }>('SELECT COUNT(*) as count FROM contacts')
        logger.info(`Organization ${orgId} has ${beforeCount?.count || 0} contacts before migration`)
        
        // Check for duplicates
        const dupeCheck = await orgDb.fetchOne<{ dupes: number }>('SELECT COUNT(*) - COUNT(DISTINCT LOWER(TRIM(email))) as dupes FROM contacts')
        logger.info(`Organization ${orgId} has ${dupeCheck?.dupes || 0} duplicate email addresses`)
        
        // Check if unique index exists
        const indexExists = await orgDb.fetchOne('SELECT name FROM sqlite_master WHERE type="index" AND name="idx_contacts_email_unique"')
        
        if ((dupeCheck?.dupes || 0) === 0 && indexExists) {
          logger.info(`No duplicates found and unique index exists for organization ${orgId}, skipping`)
          continue
        }
        
        if ((dupeCheck?.dupes || 0) === 0) {
          // No duplicates, just create the index
          logger.info(`No duplicates found, creating unique email index for organization ${orgId}`)
          await orgDb.execute('CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email_unique ON contacts(LOWER(TRIM(email)))')
          logger.info(`Created unique email index for organization ${orgId}`)
          continue
        }
        
        // Execute the migration SQL for this organization
        logger.info(`Found ${dupeCheck?.dupes || 0} duplicates, applying full migration to organization ${orgId}`)
        
        // Split the SQL into statements
        const statements = migrationSql
          .split(';')
          .map(stmt => stmt.trim())
          .filter(stmt => stmt.length > 0 && !stmt.startsWith('--'))
        
        for (const statement of statements) {
          try {
            await orgDb.execute(statement)
          } catch (error) {
            // Log the error but continue with next statement
            logger.error(`Error executing statement for org ${orgId}: ${error}`)
            logger.error(`Statement: ${statement}`)
          }
        }
        
        // Get contact count after migration
        const afterCount = await orgDb.fetchOne<{ count: number }>('SELECT COUNT(*) as count FROM contacts')
        logger.info(`Organization ${orgId} has ${afterCount?.count || 0} contacts after migration`)
        
        // Calculate removed duplicates
        const removedCount = (beforeCount?.count || 0) - (afterCount?.count || 0)
        logger.info(`Removed ${removedCount} duplicate contacts from organization ${orgId}`)
        
      } catch (error) {
        // Log the error but continue with next organization
        logger.error(`Error processing organization ${orgId}: ${error}`)
      }
    }
    
    logger.info(`Migration completed for all organizations`)
    
  } catch (error) {
    logger.error(`Error in migration script: ${error}`)
    process.exit(1)
  }
}

main()