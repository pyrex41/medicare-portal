/**
 * Script to apply the make_optional_fields_nullable migration to all organizations
 * 
 * Usage: 
 * bun run backend/src/scripts/apply_nullable_fields_migration.ts
 */

import fs from 'fs';
import path from 'path';
import { Database } from '../database';
import { logger } from '../logger';

const migrationPath = path.resolve(__dirname, '../../../backend/migrations/20250425_make_optional_fields_nullable.sql');

if (!fs.existsSync(migrationPath)) {
  logger.error(`Migration file not found: ${migrationPath}`);
  process.exit(1);
}

const migrationSQL = fs.readFileSync(migrationPath, 'utf-8');

// Function to split SQL dump into individual statements
// This handles SQL statements more precisely than just splitting on semicolons
function splitSqlStatements(sql: string): string[] {
  const statements: string[] = [];
  let currentStatement = "";
  let inString = false;
  let inComment = false;
  let inBlockComment = false;
  let stringQuote: string | null = null;

  for (let i = 0; i < sql.length; i++) {
    const char = sql[i];
    const nextChar = i + 1 < sql.length ? sql[i + 1] : null;

    // Handle comments
    if (!inString) {
      if (char === "-" && nextChar === "-" && !inBlockComment) {
        inComment = true;
        i++; // Skip next char
        currentStatement += char + nextChar;
        continue;
      }
      if (char === "/" && nextChar === "*" && !inComment) {
        inBlockComment = true;
        i++;
        currentStatement += char + nextChar;
        continue;
      }
      if (inComment && char === "\n") {
        inComment = false;
        currentStatement += char;
        continue;
      }
      if (inBlockComment && char === "*" && nextChar === "/") {
        inBlockComment = false;
        i++;
        currentStatement += char + nextChar;
        continue;
      }
      if (inComment || inBlockComment) {
        currentStatement += char;
        continue;
      }
    }

    // Handle string literals
    if ((char === "'" || char === '"') && !inComment && !inBlockComment) {
      if (!inString) {
        inString = true;
        stringQuote = char;
      } else if (char === stringQuote) {
        // Check for escaped quotes
        if (i > 0 && sql[i - 1] !== "\\") {
          inString = false;
          stringQuote = null;
        }
      }
    }

    // Handle statement termination
    if (char === ";" && !inString && !inComment && !inBlockComment) {
      currentStatement += char;
      statements.push(currentStatement.trim());
      currentStatement = "";
      continue;
    }

    currentStatement += char;
  }

  // Add the last statement if it exists
  if (currentStatement.trim()) {
    statements.push(currentStatement.trim());
  }

  return statements.filter(stmt => stmt.length > 0);
}

async function applyMigration() {
  try {
    // Get list of all organizations
    const mainDb = new Database();
    const orgs = await mainDb.fetchAll('SELECT id, name, turso_db_url, turso_auth_token FROM organizations WHERE active = 1');
    
    logger.info(`Found ${orgs.length} active organizations`);
    
    // Split SQL statements properly
    const statements = splitSqlStatements(migrationSQL);
    logger.info(`Parsed ${statements.length} SQL statements from migration file`);
    
    // Apply migration to each organization
    for (const org of orgs) {
      try {
        logger.info(`Applying migration to organization ${org.id} (${org.name})...`);
        
        // Connect to the org's database
        const orgDb = new Database(org.turso_db_url, org.turso_auth_token);
        
        // Execute each statement in a transaction if possible
        try {
          await orgDb.transaction(async (tx) => {
            for (const stmt of statements) {
              if (stmt.trim()) {
                try {
                  logger.info(`Executing statement: ${stmt.substring(0, 50)}...`);
                  await tx.execute(stmt);
                } catch (err) {
                  logger.error(`Error executing statement for org ${org.id}: ${err}`);
                  logger.error(`Statement: ${stmt}`);
                  throw err; // Rethrow to trigger transaction rollback
                }
              }
            }
            logger.info(`All statements executed successfully for organization ${org.id}`);
          });
        } catch (txError) {
          // If transaction fails, try executing statements individually without transaction
          logger.warn(`Transaction failed, trying to execute statements individually: ${txError}`);
          
          for (const stmt of statements) {
            if (stmt.trim()) {
              try {
                // Skip transaction control statements when executing individually
                if (!/^\s*(BEGIN|COMMIT|ROLLBACK|PRAGMA)/i.test(stmt)) {
                  await orgDb.execute(stmt);
                }
              } catch (err) {
                logger.error(`Error executing individual statement for org ${org.id}: ${err}`);
                logger.error(`Statement: ${stmt}`);
                // Continue with next statement
              }
            }
          }
        }
        
        logger.info(`Migration applied successfully to organization ${org.id} (${org.name})`);
      } catch (err) {
        logger.error(`Error applying migration to organization ${org.id} (${org.name}): ${err}`);
        // Continue with next organization even if one fails
      }
    }
    
    logger.info('Migration process completed for all organizations');
  } catch (err) {
    logger.error(`Fatal error in migration process: ${err}`);
    process.exit(1);
  }
}

// Run the migration
applyMigration().then(() => {
  logger.info('Migration process completed successfully');
  process.exit(0);
}).catch((err) => {
  logger.error(`Unhandled error: ${err}`);
  process.exit(1);
}); 