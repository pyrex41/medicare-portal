import { createClient } from '@libsql/client';
import { TURSO_CONFIG } from '../config/turso';
import { logger } from '../logger';
import { config } from '../config'
import fetch from 'node-fetch'

// Use non-null assertion since we check these values immediately
const TURSO_DB_URL = config.TURSO_DATABASE_URL!;
const TURSO_AUTH_TOKEN = config.TURSO_AUTH_TOKEN!;

if (!config.TURSO_DATABASE_URL) {
  throw new Error('TURSO_DATABASE_URL is not set');
}

if (!config.TURSO_AUTH_TOKEN) {
  throw new Error('TURSO_AUTH_TOKEN is not set');
}

export const tursoClient = createClient({
  url: TURSO_DB_URL,
  authToken: TURSO_AUTH_TOKEN,
});

export async function getTursoClient() {
  try {
    await tursoClient.execute('SELECT 1');
    return tursoClient;
  } catch (error) {
    logger.error(`Error connecting to Turso: ${error instanceof Error ? error.message : String(error)}`);
    throw error;
  }
}

// Export a function to check if the client is healthy
export async function checkTursoHealth(): Promise<boolean> {
  try {
    await tursoClient.execute('SELECT 1');
    return true;
  } catch (error) {
    logger.error(`Turso health check failed: ${error instanceof Error ? error.message : String(error)}`);
    return false;
  }
}

interface TursoResponse {
  databases?: Array<{
    Name: string;
    DbId: string;
    Hostname: string;
    Region: string;
  }>;
  jwt?: string;
  [key: string]: unknown;
}

export class TursoService {
  private apiToken: string
  private client;

  constructor() {
    const token = config.TURSO_API_TOKEN;
    if (!token) {
      throw new Error('TURSO_API_TOKEN is not set');
    }
    this.apiToken = token;
    this.client = tursoClient;
    
    logger.info('TursoService initialized with API token')
  }

  async createOrganizationDatabase(orgId: string): Promise<{url: string, token: string}> {
    const dbName = `org-${orgId}-${Date.now()}`
    
    try {
      // Create database
      logger.info(`Creating Turso database for org ${orgId} with name ${dbName}`)
      const createResponse = await fetch('https://api.turso.tech/v1/databases', {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${this.apiToken}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          name: dbName,
          group: 'default',
        }),
      })

      if (!createResponse.ok) {
        const errorText = await createResponse.text()
        logger.error(`Failed to create database: ${errorText}`)
        throw new Error(`Failed to create database: ${errorText}`)
      }

      const createData = await createResponse.json() as any
      logger.info(`Database creation response: ${JSON.stringify(createData)}`)
      
      const hostname = createData.database?.Hostname
      if (!hostname) {
        logger.error(`Invalid database creation response - missing hostname: ${JSON.stringify(createData)}`)
        throw new Error('Failed to get database hostname')
      }
      logger.info(`Successfully created database with hostname: ${hostname}`)
      
      // Create access token
      logger.info(`Creating access token for database ${dbName}`)
      const tokenUrl = `https://api.turso.tech/v1/databases/${dbName}/auth/tokens`
      logger.info(`Token creation URL: ${tokenUrl}`)
      
      const tokenResponse = await fetch(tokenUrl, {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${this.apiToken}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          expiration: 'never',
        }),
      })

      if (!tokenResponse.ok) {
        const errorText = await tokenResponse.text()
        logger.error(`Failed to create token (status ${tokenResponse.status}): ${errorText}`)
        // Log response headers for debugging
        const headers: Record<string, string> = {}
        tokenResponse.headers.forEach((value, key) => {
          headers[key] = value
        })
        logger.error(`Token response headers: ${JSON.stringify(headers)}`)
        throw new Error(`Failed to create token: ${errorText}`)
      }

      const tokenData = await tokenResponse.json() as any
      logger.info(`Token creation response: ${JSON.stringify(tokenData)}`)
      
      const token = tokenData.jwt
      if (!token) {
        logger.error(`Invalid token response - missing jwt: ${JSON.stringify(tokenData)}`)
        throw new Error('Failed to get database access token')
      }
      
      logger.info(`Successfully created token for database ${dbName}`)
      
      return {
        url: hostname,
        token,
      }
    } catch (error) {
      logger.error(`Error creating organization database: ${error instanceof Error ? error.message : String(error)}`)
      if (error instanceof Error && error.stack) {
        logger.error(`Stack trace: ${error.stack}`)
      }
      throw error
    }
  }

  async deleteOrganizationDatabase(dbName: string): Promise<void> {
    try {
      logger.info(`Deleting Turso database ${dbName}`)
      const response = await fetch(`https://api.turso.tech/v1/databases/${dbName}`, {
        method: 'DELETE',
        headers: {
          'Authorization': `Bearer ${this.apiToken}`,
        },
      })

      if (!response.ok) {
        const errorText = await response.text()
        logger.error(`Failed to delete database: ${errorText}`)
        throw new Error(`Failed to delete database: ${errorText}`)
      }
      
      logger.info(`Successfully deleted database ${dbName}`)
    } catch (error) {
      logger.error(`Error deleting organization database: ${error}`)
      throw error
    }
  }

  async downloadDatabaseDump(url: string, token: string): Promise<string> {
    try {
      // Extract base URL (remove https:// if present)
      const baseUrl = url.replace(/^https?:\/\//, '')
      const apiUrl = `https://${baseUrl}/dump`
      logger.info(`Downloading database dump from ${apiUrl}`)
      
      const response = await fetch(apiUrl, {
        method: 'GET',
        headers: {
          'Authorization': `Bearer ${token}`,
        },
      })

      if (!response.ok) {
        const errorText = await response.text()
        logger.error(`Failed to download database dump: ${errorText}`)
        throw new Error(`Failed to download database dump: ${errorText}`)
      }

      const dumpData = await response.text()
      logger.info(`Successfully downloaded database dump: ${Math.round(dumpData.length / 1024)} KB`)
      return dumpData
    } catch (error) {
      logger.error(`Error downloading database dump: ${error}`)
      throw error
    }
  }

  async createDatabaseFromDump(orgId: string, suffix: string, dumpContent: string): Promise<{url: string, token: string}> {
    const dbName = `org-${orgId}-${suffix}`;

    try {
      logger.info(`Creating new database: ${dbName}`);
      
      // Step 1: Create the database with a unique name
      const createDbResponse = await fetch(
        `${TURSO_CONFIG.API_URL}/organizations/${TURSO_CONFIG.ORG_SLUG}/databases`, 
        {
          method: 'POST',
          headers: {
            'Authorization': `Bearer ${this.apiToken}`,
            'Content-Type': 'application/json'
          },
          body: JSON.stringify({
            name: dbName,
            group: TURSO_CONFIG.GROUP_NAME
          })
        }
      );

      if (!createDbResponse.ok) {
        const errorText = await createDbResponse.text();
        throw new Error(`Failed to create database: ${errorText}`);
      }

      const dbData = await createDbResponse.json();
      logger.info(`Database created: ${JSON.stringify(dbData.database || {})}`);
      
      // Step 2: Generate an auth token for the database
      logger.info(`Generating auth token for database ${dbName}`);
      const tokenResponse = await fetch(
        `${TURSO_CONFIG.API_URL}/organizations/${TURSO_CONFIG.ORG_SLUG}/databases/${dbName}/auth/tokens`,
        {
          method: 'POST',
          headers: {
            'Authorization': `Bearer ${this.apiToken}`,
            'Content-Type': 'application/json'
          }
        }
      );

      if (!tokenResponse.ok) {
        const errorText = await tokenResponse.text();
        // Try to clean up the created database
        try {
          await this.deleteOrganizationDatabase(dbName);
        } catch (cleanupError) {
          logger.warn(`Failed to delete database after token error: ${cleanupError}`);
        }
        throw new Error(`Failed to create auth token: ${errorText}`);
      }

      const tokenData = await tokenResponse.json();
      const url = `https://${dbData.database.Hostname}`;
      const token = tokenData.jwt;
      
      logger.info(`Successfully created database ${dbName} with auth token`);
      
      // Step 3: Import the SQL dump using the libSQL client
      try {
        logger.info(`Uploading SQL dump (${Math.round(dumpContent.length / 1024)} KB) to new database`);
        
        // Create client for the new database
        const client = createClient({
          url,
          authToken: token
        });
        
        // Split dump into logical statement groups for more reliable execution
        const statements = dumpContent
          .split(';')
          .map(stmt => stmt.trim())
          .filter(stmt => stmt.length > 0);
        
        logger.info(`Split SQL dump into ${statements.length} statements`);
        
        // Execute statements in phases - first schema statements, then data statements
        // Parse and categorize statements
        const schemaStatements = statements.filter(stmt => 
          stmt.toUpperCase().startsWith('CREATE TABLE')
        );
        
        const indexStatements = statements.filter(stmt => 
          stmt.toUpperCase().startsWith('CREATE INDEX') || 
          stmt.toUpperCase().startsWith('CREATE UNIQUE INDEX')
        );
        
        const insertStatements = statements.filter(stmt => 
          stmt.toUpperCase().startsWith('INSERT')
        );
        
        const otherStatements = statements.filter(stmt => 
          !schemaStatements.includes(stmt) && 
          !indexStatements.includes(stmt) && 
          !insertStatements.includes(stmt)
        );
        
        logger.info(`Processing ${schemaStatements.length} tables, ${indexStatements.length} indexes, ${insertStatements.length} data inserts, and ${otherStatements.length} other statements`);
        
        // Phase 1: Create tables
        for (const tableStatement of schemaStatements) {
          try {
            logger.info(`Creating table: ${tableStatement.substring(0, 60)}...`);
            await client.execute(tableStatement);
          } catch (error) {
            // If there's an error with the contacts table missing updated_at, add it
            if (tableStatement.includes('contacts') && 
                error.toString().includes('no column named updated_at')) {
              logger.warn('Error creating contacts table, attempting to fix missing updated_at column');
              
              // Add the updated_at column if it's missing
              try {
                // First create the table without the updated_at column
                await client.execute(tableStatement);
                
                // Then add the updated_at column
                await client.execute(`
                  ALTER TABLE contacts
                  ADD COLUMN updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
                `);
                
                logger.info('Successfully added missing updated_at column to contacts table');
              } catch (altError) {
                logger.error(`Failed to fix contacts table: ${altError}`);
                throw altError;
              }
            } else {
              // For other errors, just throw them
              throw error;
            }
          }
        }
        
        // Phase 2: Execute other statements (drops, alters, etc)
        for (const statement of otherStatements) {
          await client.execute(statement);
        }
        
        // Phase 3: Insert data
        logger.info(`Inserting data (${insertStatements.length} statements)...`);
        let successCount = 0;
        let errorCount = 0;
        
        // Check if we've already detected updated_at missing
        let updatedAtMissing = false;
        let checkedUpdatedAt = false;
        
        for (const insertStatement of insertStatements) {
          try {
            // Skip if we know the statement will fail due to updated_at missing
            if (updatedAtMissing && insertStatement.includes('updated_at')) {
              // Try to fix the insert statement by removing the updated_at column
              const fixedInsert = insertStatement
                .replace(/updated_at\s*,/i, '')  // Remove updated_at from column list
                .replace(/(\w+\s*,\s*)CURRENT_TIMESTAMP(\s*\))/gi, '$1$2')  // Remove corresponding value in VALUES
                .replace(/,\s*\)/g, ')');  // Fix any trailing commas
              
              try {
                await client.execute(fixedInsert);
                successCount++;
                
                // Log progress periodically
                if (successCount % 50 === 0) {
                  logger.info(`Imported ${successCount}/${insertStatements.length} data statements (with fixes)`);
                }
              } catch (fixError) {
                errorCount++;
                logger.error(`Error executing fixed INSERT: ${fixError}`);
                // Continue with next statement
              }
              continue;
            }
            
            // Try the original statement
            await client.execute(insertStatement);
            successCount++;
            
            // Log progress periodically
            if (successCount % 50 === 0) {
              logger.info(`Imported ${successCount}/${insertStatements.length} data statements`);
            }
          } catch (error) {
            // Check if this is the updated_at missing error
            if (!checkedUpdatedAt && error.toString().includes('no column named updated_at')) {
              updatedAtMissing = true;
              checkedUpdatedAt = true;
              logger.warn('Detected missing updated_at column in contacts table, will attempt to fix INSERT statements');
              
              // Try to add the column if it's missing
              try {
                await client.execute(`
                  ALTER TABLE contacts
                  ADD COLUMN updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
                `);
                logger.info('Added missing updated_at column to contacts table');
                
                // Now we have the column, try the statement again
                try {
                  await client.execute(insertStatement);
                  successCount++;
                  logger.info('Successfully executed INSERT after adding updated_at column');
                  
                  // We fixed the table, so we don't need to modify statements anymore
                  updatedAtMissing = false;
                } catch (retryError) {
                  errorCount++;
                  logger.error(`Error retrying INSERT after adding column: ${retryError}`);
                }
              } catch (alterError) {
                logger.warn(`Could not add updated_at column: ${alterError}`);
                // Still mark as missing so we can fix the statements
              }
            } else {
              errorCount++;
              logger.error(`Error executing INSERT: ${error}`);
              // Continue with next statement - don't fail everything for one bad insert
            }
          }
        }
        
        // Phase 4: Create indexes (do this last for better performance)
        logger.info(`Creating ${indexStatements.length} indexes...`);
        for (const indexStatement of indexStatements) {
          try {
            await client.execute(indexStatement);
          } catch (error) {
            logger.warn(`Error creating index: ${error}`);
            // Continue with next index - not fatal
          }
        }
        
        // Log summary
        logger.info(`Database import complete: ${successCount} successful inserts, ${errorCount} errors`);
        
        // Verify the database
        try {
          const result = await client.execute('SELECT COUNT(*) as count FROM contacts');
          const count = result.rows?.[0]?.[0];
          logger.info(`Database verification: ${count} contacts found`);
        } catch (error) {
          logger.warn(`Error verifying database: ${error}`);
        }
        
        return { url, token };
      } catch (importError) {
        logger.error(`Error importing SQL dump: ${importError}`);
        
        // Try to clean up the database
        try {
          await this.deleteOrganizationDatabase(dbName);
          logger.info(`Cleaned up database ${dbName} after import error`);
        } catch (cleanupError) {
          logger.warn(`Failed to clean up database: ${cleanupError}`);
        }
        
        throw importError;
      }
    } catch (error) {
      logger.error(`Error creating database from dump: ${error}`);
      throw error;
    }
  }

  private handleError(error: unknown): never {
    logger.error(`Turso service error: ${error instanceof Error ? error.message : String(error)}`);
    throw error instanceof Error ? error : new Error(String(error));
  }

  private validateResponse(data: unknown): TursoResponse {
    if (!data || typeof data !== 'object') {
      throw new Error('Invalid response from Turso API');
    }
    return data as TursoResponse;
  }
}