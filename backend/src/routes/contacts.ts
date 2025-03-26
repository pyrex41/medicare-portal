import { Elysia, t } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import type { ContactCreate } from '../types';
import fs from 'fs/promises';
import path from 'path';
import os from 'os';

type User = {
  id: number;
  organization_id: number;
  is_admin: boolean;
};

type Store = {
  user: User;
};

type Context = {
  request: Request;
  store: Store;
  set: { status: number };
  body: { overwriteExisting?: boolean };
  params: { orgId: string };
  query: Record<string, string | undefined>;
};

/**
 * Contacts API endpoints
 */
export const contactsRoutes = new Elysia({ prefix: '/api/contacts' })
  .guard({
    beforeHandle: [
      async ({ request, store }) => {
        const user = await Database.getUserFromSession(request);
        if (user) {
          store.user = user;
        }
      }
    ]
  }, app => app
    
    // Get paginated contacts with filtering
    .get('/', async ({ query, store, set }: Context) => {
      const user = store.user;
      if (!user || !user.organization_id) {
        set.status = 401;
        return { error: 'Not authorized' };
      }

      try {
        // Get parameters from query
        const page = parseInt(query?.page as string) || 1;
        const limit = parseInt(query?.limit as string) || 50;
        
        // Handle filters - either from a JSON string or from individual parameters
        let filters = {};
        if (query?.filters) {
          try {
            filters = JSON.parse(query.filters as string);
          } catch (e) {
            logger.warn(`Failed to parse filters JSON: ${query.filters}`);
          }
        }
        
        // Also handle individual filter parameters
        const search = query?.search;
        const statesParam = query?.states;
        const carriersParam = query?.carriers;
        const agentsParam = query?.agents;
        
        if (search) filters.name = search;
        if (statesParam) filters.state = statesParam;
        if (carriersParam) filters.carrier = carriersParam;
        
        logger.info(`GET /api/contacts - Fetching page ${page}, limit ${limit}, filters: ${JSON.stringify(filters)}, search: ${search}, states: ${statesParam}, carriers: ${carriersParam}, agents: ${agentsParam}`);
        
        // Get organization database
        const orgDb = await Database.getOrgDb(user.organization_id.toString());
        logger.info(`GET /api/contacts - Connected to organization database for org ${user.organization_id}`);
        
        // Implement pagination directly with SQL
        const offsetValue = (page - 1) * limit;
        
        // Build the WHERE clause based on provided filters
        let whereClause = '1=1'; // Default condition (all records)
        const queryParams: any[] = [];
        
        if (filters) {
          // Handle search/name filter
          if (filters.name) {
            whereClause += ' AND (first_name LIKE ? OR last_name LIKE ?)';
            queryParams.push(`%${filters.name}%`, `%${filters.name}%`);
          }
          
          // Handle email filter
          if (filters.email) {
            whereClause += ' AND email LIKE ?';
            queryParams.push(`%${filters.email}%`);
          }
          
          // Handle state filter (could be a single state or an array)
          if (filters.state) {
            if (Array.isArray(filters.state) && filters.state.length > 0) {
              // For arrays of states, create an IN clause
              const placeholders = filters.state.map(() => '?').join(', ');
              whereClause += ` AND state IN (${placeholders})`;
              queryParams.push(...filters.state);
            } else if (typeof filters.state === 'string' && filters.state.includes(',')) {
              // Handle comma-separated string of states
              const states = filters.state.split(',').map(s => s.trim()).filter(Boolean);
              if (states.length > 0) {
                const placeholders = states.map(() => '?').join(', ');
                whereClause += ` AND state IN (${placeholders})`;
                queryParams.push(...states);
              }
            } else if (typeof filters.state === 'string' && filters.state.trim() !== '') {
              // Handle single state
              whereClause += ' AND state = ?';
              queryParams.push(filters.state);
            }
          }
          
          // Handle carrier filter (could be a single carrier or an array)
          if (filters.carrier) {
            if (Array.isArray(filters.carrier) && filters.carrier.length > 0) {
              // For arrays of carriers, we need multiple LIKE conditions
              const likeConditions = filters.carrier.map(() => 'current_carrier LIKE ?').join(' OR ');
              whereClause += ` AND (${likeConditions})`;
              queryParams.push(...filters.carrier.map(c => `%${c}%`));
            } else if (typeof filters.carrier === 'string' && filters.carrier.includes(',')) {
              // Handle comma-separated string of carriers
              const carriers = filters.carrier.split(',').map(c => c.trim()).filter(Boolean);
              if (carriers.length > 0) {
                const likeConditions = carriers.map(() => 'current_carrier LIKE ?').join(' OR ');
                whereClause += ` AND (${likeConditions})`;
                queryParams.push(...carriers.map(c => `%${c}%`));
              }
            } else if (typeof filters.carrier === 'string' && filters.carrier.trim() !== '') {
              // Handle single carrier
              whereClause += ' AND current_carrier LIKE ?';
              queryParams.push(`%${filters.carrier}%`);
            }
          }
          
          // Handle agent filter
          if (filters.agents) {
            if (Array.isArray(filters.agents) && filters.agents.length > 0) {
              // For arrays of agent IDs
              const placeholders = filters.agents.map(() => '?').join(', ');
              whereClause += ` AND agent_id IN (${placeholders})`;
              queryParams.push(...filters.agents);
            } else if (typeof filters.agents === 'string' && filters.agents.includes(',')) {
              // Handle comma-separated string of agent IDs
              const agentIds = filters.agents.split(',').map(a => parseInt(a.trim())).filter(a => !isNaN(a));
              if (agentIds.length > 0) {
                const placeholders = agentIds.map(() => '?').join(', ');
                whereClause += ` AND agent_id IN (${placeholders})`;
                queryParams.push(...agentIds);
              }
            } else if (typeof filters.agents === 'string' || typeof filters.agents === 'number') {
              // Handle single agent ID
              whereClause += ' AND agent_id = ?';
              queryParams.push(parseInt(filters.agents as string));
            }
          }
        }
        
        // Log the final SQL query
        logger.info(`GET /api/contacts - WHERE clause: ${whereClause}, params: ${JSON.stringify(queryParams)}`);
        
        
        // Get total count
        const countResult = await orgDb.fetchOne(
          `SELECT COUNT(*) as total FROM contacts WHERE ${whereClause}`,
          queryParams
        );
        const total = countResult?.total || 0;
        logger.info(`GET /api/contacts - Found total of ${total} contacts`);
        
        // Get paginated contacts
        const contacts = await orgDb.fetchAll(
          `SELECT * FROM contacts WHERE ${whereClause} ORDER BY created_at DESC LIMIT ? OFFSET ?`,
          [...queryParams, limit, offsetValue]
        );
        logger.info(`GET /api/contacts - Retrieved ${contacts.length} contacts for this page`);
        
        // Map contacts to the expected format
        const mappedContacts = contacts.map(contact => ({
          id: contact.id,
          first_name: contact.first_name,
          last_name: contact.last_name,
          email: contact.email,
          current_carrier: contact.current_carrier,
          plan_type: contact.plan_type,
          effective_date: contact.effective_date,
          birth_date: contact.birth_date,
          tobacco_user: Boolean(contact.tobacco_user),
          gender: contact.gender,
          state: contact.state,
          zip_code: contact.zip_code,
          agent_id: contact.agent_id,
          last_emailed: contact.last_emailed,
          phone_number: contact.phone_number || ''
        }));

        // Create filter options from the available data
        const filterOptions = {
          carriers: [...new Set(contacts.map(c => c.current_carrier).filter(Boolean))],
          states: [...new Set(contacts.map(c => c.state).filter(Boolean))]
        };

        // Format the response to match what the frontend expects
        const response = {
          contacts: mappedContacts,
          filterOptions,
          total,
          page,
          limit
        };
        
        logger.info(`GET /api/contacts - Sending response with ${mappedContacts.length} contacts, total: ${total}`);
        return response;
      } catch (error) {
        logger.error(`Error in GET /api/contacts: ${error}`);
        set.status = 500;
        return { error: 'Failed to fetch contacts' };
      }
    })
    
    // Bulk import contacts from CSV
    .post('/bulk-import',
      {
        body: t.Object({
          overwriteExisting: t.Optional(t.Boolean())
        })
      },
      async ({ request, store, set, body }: Context) => {
        const user = store.user;
        if (!user || !user.organization_id || !user.is_admin) {
          set.status = 401;
          return { error: 'Not authorized for bulk import' };
        }

        try {
          const formData = await request.formData();
          const file = formData.get('file');
          if (!file || !(file instanceof File) || path.extname(file.name).toLowerCase() !== '.csv') {
            set.status = 400;
            return { error: 'File must be a CSV' };
          }

          const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'import-'));
          const tempFilePath = path.join(tempDir, 'contacts.csv');
          await fs.writeFile(tempFilePath, Buffer.from(await file.arrayBuffer()));

          logger.info(`CSV file saved to ${tempFilePath}, starting import`);

          // Start the import process in the background with high priority
          const importPromise = Database.bulkImportContacts(
            user.organization_id.toString(), 
            tempFilePath, 
            body.overwriteExisting || false
          );

          // Handle promise completion to clean up temp files and log results
          importPromise
            .then(() => {
              logger.info(`Import completed successfully for organization ${user.organization_id}`);
              return fs.rm(tempDir, { recursive: true, force: true });
            })
            .catch((error: Error) => {
              logger.error(`Background import failed for organization ${user.organization_id}: ${error}`);
              return fs.rm(tempDir, { recursive: true, force: true }).catch(() => {
                logger.error(`Failed to clean up temp directory ${tempDir}`);
              });
            });

          return { 
            success: true, 
            message: 'Started import of contacts',
            organizationId: user.organization_id
          };
        } catch (error) {
          const err = error as Error;
          logger.error(`Error in bulk import: ${err}`);
          set.status = 500;
          return { error: 'Failed to process import: ' + err.message };
        }
      }
    )
    
    // Add route to check import status
    .get('/import-status/:orgId', async ({ params, store, set }: Context) => {
      const user = store.user;
      
      if (!user || !user.organization_id) {
        set.status = 401;
        return { error: 'Not authorized' };
      }
      
      // Only admins can check import status for any org
      // Regular users can only check their own org's status
      if (Number(params.orgId) !== user.organization_id && !user.is_admin) {
        set.status = 403;
        return { error: 'Not authorized to view this organization' };
      }
      
      try {
        // Look for a backup file to indicate an import is in progress
        const backupDir = path.join(process.cwd(), 'backups');
        const files = await fs.readdir(backupDir);
        
        // Filter files for this organization
        const orgBackups = files.filter(file => file.startsWith(`org-${params.orgId}-`));
        
        if (orgBackups.length === 0) {
          return { 
            status: 'none',
            message: 'No import in progress or recently completed' 
          };
        }
        
        // Sort by creation time (most recent first)
        const statPromises = orgBackups.map(async (file) => {
          const stats = await fs.stat(path.join(backupDir, file));
          return { file, stats };
        });
        
        const fileStats = await Promise.all(statPromises);
        fileStats.sort((a, b) => b.stats.mtimeMs - a.stats.mtimeMs);
        
        const latestBackup = fileStats[0];
        const ageInMinutes = (Date.now() - latestBackup.stats.mtimeMs) / 60000;
        
        // If backup file is less than 30 minutes old, consider it active
        if (ageInMinutes < 30) {
          const timestamp = latestBackup.file.split('-').pop()?.split('.')[0] || '';
          
          // Check logs for progress info
          let progress = "unknown";
          try {
            const logs = await fs.readFile(path.join(process.cwd(), 'logs', 'ai-interactions.log'), 'utf-8');
            const progressLines = logs.split('\n').filter(line => 
              line.includes(`org-${params.orgId}-${timestamp}`) && line.includes('contacts')
            );
            
            if (progressLines.length > 0) {
              const lastLine = progressLines[progressLines.length - 1];
              if (lastLine.includes('completed successfully')) {
                progress = "completed";
              } else {
                progress = "in progress";
              }
            }
          } catch (error) {
            // Can't read logs, continue with unknown status
          }
          
          return {
            status: progress,
            message: `Import ${progress === "completed" ? "completed" : "in progress"}`,
            timestamp: new Date(parseInt(timestamp)).toISOString(),
            age: Math.round(ageInMinutes)
          };
        } else {
          return {
            status: 'completed',
            message: 'Import completed',
            timestamp: new Date(latestBackup.stats.mtimeMs).toISOString(),
            age: Math.round(ageInMinutes)
          };
        }
      } catch (error) {
        const err = error as Error;
        logger.error(`Error checking import status: ${err}`);
        return { 
          status: 'error',
          message: `Error checking import status: ${err.message}`
        };
      }
    })
  );