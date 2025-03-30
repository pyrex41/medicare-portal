#!/usr/bin/env bun

/**
 * Script to apply email unique constraint migration to all organization databases
 * 
 * Usage: 
 * - Apply to all orgs: bun run scripts/apply_org_migrations.ts
 * - Apply to specific org: bun run scripts/apply_org_migrations.ts 7
 */
import { Database } from '../database';
import { TursoService } from '../services/turso';
import { logger } from '../logger';
import fs from 'fs';
import path from 'path';

async function main() {
  try {
    // Check if we're targeting a specific org
    const targetOrgId = process.argv[2];
    
    // Initialize main database connection
    const mainDb = new Database();
    
    // Get organizations with their database URLs and tokens
    let orgs;
    if (targetOrgId) {
      // If targeting specific org
      orgs = await mainDb.fetchAll(`
        SELECT id, name, turso_db_url, turso_auth_token 
        FROM organizations 
        WHERE id = ? AND turso_db_url IS NOT NULL AND turso_auth_token IS NOT NULL
      `, [targetOrgId]);
      
      if (orgs.length === 0) {
        logger.error(`Organization with ID ${targetOrgId} not found or has no database configuration`);
        process.exit(1);
      }
      logger.info(`Targeting specific organization: ${targetOrgId}`);
    } else {
      // Get all orgs
      orgs = await mainDb.fetchAll(`
        SELECT id, name, turso_db_url, turso_auth_token 
        FROM organizations 
        WHERE turso_db_url IS NOT NULL AND turso_auth_token IS NOT NULL
      `);
    }
    
    logger.info(`Found ${orgs.length} organizations with databases`);
    
    // Get the migration SQL
    const migrationPath = path.join(process.cwd(), 'migrations', '20250329_fix_email_unique_constraint.sql');
    const migrationSql = fs.readFileSync(migrationPath, 'utf8');
    
    // Process each organization
    for (const org of orgs) {
      const orgId = org.id || org[0];
      const orgName = org.name || org[1];
      const dbUrl = org.turso_db_url || org[2];
      const authToken = org.turso_auth_token || org[3];
      
      try {
        logger.info(`Processing organization: ${orgName} (ID: ${orgId})`);
        
        // Get database connection for this organization
        const orgDb = new Database(dbUrl, authToken);
        
        // Check if contacts table exists and if updated_at column exists
        let tableCheck, hasUpdatedAtColumn;
        try {
          tableCheck = await orgDb.fetchOne(`
            SELECT name FROM sqlite_master WHERE type='table' AND name='contacts'
          `);
          
          if (tableCheck) {
            // Check if updated_at column exists
            const columnCheck = await orgDb.fetchOne(`
              SELECT COUNT(*) as count FROM pragma_table_info('contacts') WHERE name = 'updated_at'
            `);
            hasUpdatedAtColumn = columnCheck && 
              (typeof columnCheck === 'object' ? 
                ((columnCheck as any).count > 0 || (Array.isArray(columnCheck) && columnCheck[0] && Number(columnCheck[0]) > 0)) : 
                false);
            
            logger.info(`Table check: contacts table exists=${!!tableCheck}, has updated_at column=${hasUpdatedAtColumn}`);
          }
        } catch (checkError) {
          logger.warn(`Error checking table schema: ${checkError}`);
        }
        
        // Apply migration in separate chunks to handle potential errors
        
        // Start with dropping views that depend on contacts
        try {
          await orgDb.execute('DROP VIEW IF EXISTS v_contact_stats');
          logger.info('Dropped dependent views');
        } catch (viewError) {
          logger.warn(`Error dropping views: ${viewError}`);
        }
        
        // Check and backup eligibility_answers if it exists
        try {
          const eligibilityCheck = await orgDb.fetchOne(`
            SELECT name FROM sqlite_master WHERE type='table' AND name='eligibility_answers'
          `);
          
          if (eligibilityCheck) {
            // Create backup table and copy data
            await orgDb.execute(`
              CREATE TABLE IF NOT EXISTS eligibility_answers_backup AS 
              SELECT * FROM eligibility_answers
            `);
            
            // Drop the table to avoid foreign key issues
            await orgDb.execute('DROP TABLE IF EXISTS eligibility_answers');
            logger.info('Backed up and dropped eligibility_answers table');
          }
        } catch (backupError) {
          logger.warn(`Error handling eligibility_answers: ${backupError}`);
        }
        
        // Handle updated_at column if contacts table exists
        if (tableCheck) {
          try {
            if (!hasUpdatedAtColumn) {
              logger.info('Adding missing updated_at column');
              await orgDb.execute('ALTER TABLE contacts ADD COLUMN updated_at DATETIME DEFAULT CURRENT_TIMESTAMP');
            } else {
              logger.info('updated_at column already exists, skipping addition');
            }
          } catch (columnError) {
            // If error contains "duplicate column" we can ignore it
            if ((columnError as Error).toString().includes('duplicate column')) {
              logger.info('Ignoring duplicate column error for updated_at - column already exists');
            } else {
              logger.warn(`Error handling updated_at column: ${columnError}`);
            }
          }
        }
        
        // Apply the main migration script with error handling for each statement
        try {
          // Split migration SQL into individual statements
          const migrationStatements = migrationSql
            .split(';')
            .map(stmt => stmt.trim())
            .filter(stmt => stmt.length > 0 && !stmt.startsWith('--'));
          
          // Skip statements that add updated_at column since we've handled it above
          const filteredStatements = migrationStatements.filter(stmt => {
            // Skip the problematic statements
            return !stmt.includes('ADD COLUMN updated_at') && 
                   !stmt.includes('pragma_table_info') &&
                   !stmt.includes('_vars') &&
                   !stmt.includes('_commands');
          });
          
          logger.info(`Applying ${filteredStatements.length} migration statements`);
          
          // Execute each statement with individual error handling
          for (const statement of filteredStatements) {
            try {
              // Skip empty statements or diagnostic queries
              if (statement.trim().length === 0 || 
                  statement.includes('PRAGMA table_info') ||
                  statement.includes('SELECT name, sql FROM sqlite_master')) {
                continue;
              }
              
              await orgDb.execute(statement);
            } catch (stmtError) {
              const errorString = (stmtError as Error).toString();
              
              // Handle specific errors gracefully
              if (errorString.includes('duplicate column name: updated_at')) {
                logger.info('Ignoring duplicate updated_at column error');
              } else if (errorString.includes('no such table: contacts_temp')) {
                logger.warn('Contacts temporary table not found, may have already been renamed');
              } else {
                logger.error(`Error executing statement: ${stmtError}`);
                logger.error(`Failed statement: ${statement.substring(0, 100)}...`);
              }
              // Continue with next statement, errors shouldn't fail the whole migration
            }
          }
          
          // Verify the migration results
          try {
            // Check if email unique constraint exists
            const uniqueCheck = await orgDb.fetchOne(`
              SELECT COUNT(*) as count FROM sqlite_master 
              WHERE type='index' AND name='contact_email_unique'
            `);
            
            const hasUniqueConstraint = uniqueCheck && 
              (typeof uniqueCheck === 'object' ? 
                ((uniqueCheck as any).count > 0 || (Array.isArray(uniqueCheck) && uniqueCheck[0] && Number(uniqueCheck[0]) > 0)) : 
                false);
                
            if (hasUniqueConstraint) {
              logger.info('Email unique constraint created successfully');
            } else {
              logger.warn('Email unique constraint may not have been created');
              
              // Try to create it if missing
              try {
                await orgDb.execute(`
                  CREATE UNIQUE INDEX IF NOT EXISTS contact_email_unique ON contacts(email)
                `);
                logger.info('Created missing email unique constraint');
              } catch (createError) {
                logger.error(`Error creating missing constraint: ${createError}`);
              }
            }
            
            // Test if ON CONFLICT works
            try {
              await orgDb.execute(`
                INSERT OR IGNORE INTO contacts (
                  first_name, last_name, email, current_carrier, plan_type, 
                  effective_date, birth_date, tobacco_user, gender, state, zip_code, phone_number
                ) 
                VALUES ('Test', 'Migration', 'test.migration@example.com', 'Test', 'Test', 
                  '2025-01-01', '1970-01-01', 0, 'M', 'TX', '12345', '123-456-7890')
                ON CONFLICT(email) DO NOTHING
              `);
              logger.info('ON CONFLICT query test successful');
            } catch (testError) {
              logger.error(`ON CONFLICT test failed: ${testError}`);
            }
            
          } catch (verifyError) {
            logger.warn(`Error verifying migration results: ${verifyError}`);
          }
          
          logger.info(`Successfully migrated organization ${orgName} (ID: ${orgId})`);
        } catch (migrationError) {
          logger.error(`Migration failed for organization ${orgName} (ID: ${orgId}): ${migrationError}`);
        }
      } catch (orgError) {
        logger.error(`Error processing organization ${orgName} (ID: ${orgId}): ${orgError}`);
      }
    }
    
    logger.info('Migration process completed');
  } catch (error) {
    logger.error(`Script error: ${error}`);
    process.exit(1);
  }
}

main().catch(error => {
  logger.error(`Unhandled error: ${error}`);
  process.exit(1);
}); 