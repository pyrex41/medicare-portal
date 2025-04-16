import { Elysia, t } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import { validateSession, getUserFromSession } from '../services/auth';
import fs from 'fs';
import path from 'path';
import { stringify } from 'csv-stringify/sync';
import { nanoid } from 'nanoid';
import { ZIP_DATA } from '../index';
import { TursoService } from '../services/turso';
import { TURSO_CONFIG } from '../config/turso';
import fetch from 'node-fetch';
import { 
  trackContact, 
  trackContactBatch, 
  getContactUsageStats, 
  getUniqueContactCount,
  resetContactCount
} from '../services/contactTracking';

type User = {
  id: number;
  organization_id: number;
  is_admin: boolean;
};

interface ContactImport {
  first_name: string;
  last_name: string;
  email: string;
  phone_number: string;
  state: string;
  current_carrier: string;
  effective_date: string;
  birth_date: string;
  tobacco_user: boolean;
  gender: string;
  zip_code: string;
  plan_type: string;
}

type BulkImportRequest = {
  contacts: ContactImport[];
  overwriteExisting: boolean;
  agentId?: number | null;
};

interface Contact {
  id: number;
  first_name: string;
  last_name: string;
  email: string;
  phone_number?: string;
  state: string;
  current_carrier?: string;
  effective_date: string;
  birth_date: string;
  tobacco_user: number;
  gender: string;
  zip_code: string;
  plan_type?: string;
  agent_id?: number;
  last_emailed?: string;
  created_at: string;
  updated_at: string;
  status?: string;
}

type Context = {
  request: Request;
  user: User;
  set: { status: number };
};

/**
 * Contacts API endpoints
 */
export const contactsRoutes = new Elysia({ prefix: '/api/contacts' })
  .use(app => app
    .derive(async ({ request, set }) => {
      const sessionCookie = request.headers.get('cookie')?.split(';')
        .find(c => c.trim().startsWith('session='))
        ?.split('=')[1];

      if (!sessionCookie) {
        set.status = 401;
        return { error: 'Not authorized' };
      }

      const user = await validateSession(sessionCookie);
      if (!user) {
        set.status = 401;
        return { error: 'Not authorized' };
      }

      return { user };
    })
  )
  .get('/', async ({ request, user, set }: Context) => {
    if (!user || !user.organization_id) {
      set.status = 401;
      return { error: 'Not authorized' };
    }

    try {
      // Parse query parameters
      const url = new URL(request.url);
      const page = parseInt(url.searchParams.get('page') || '1');
      const limit = parseInt(url.searchParams.get('limit') || '100');
      const search = url.searchParams.get('search') || '';
      const states = url.searchParams.get('states')?.split(',').filter(Boolean) || [];
      const carriers = url.searchParams.get('carriers')?.split(',').filter(Boolean) || [];
      const agents = url.searchParams.get('agents')?.split(',').map(Number).filter(Boolean) || [];

      logger.info(`Fetching contacts for org ${user.organization_id} - page: ${page}, limit: ${limit}, search: ${search || 'none'}, states: ${states.length ? states.join(',') : 'none'}, carriers: ${carriers.length ? carriers.join(',') : 'none'}, agents: ${agents.length ? agents.join(',') : 'none'}`);

      // Build base query parts
      let whereConditions = ['1=1'];
      let params: any[] = [];

      // Add search condition if present
      if (search) {
        const searchTerms = search.trim().split(/\s+/);
        
        if (searchTerms.length === 1) {
          // Single word search - check each column individually
          whereConditions.push('(first_name LIKE ? OR last_name LIKE ? OR email LIKE ? OR phone_number LIKE ?)');
          params.push(`%${search}%`, `%${search}%`, `%${search}%`, `%${search}%`);
        } else {
          // Multi-word search - treat first word as first name and remaining words as last name
          const firstName = searchTerms[0];
          const lastName = searchTerms.slice(1).join(' ');
          
          whereConditions.push('((first_name LIKE ? AND last_name LIKE ?) OR first_name LIKE ? OR last_name LIKE ? OR email LIKE ? OR phone_number LIKE ?)');
          params.push(
            `%${firstName}%`, `%${lastName}%`, // Combined name search
            `%${search}%`, `%${search}%`, // Full search term in either name field
            `%${search}%`, `%${search}%` // Email and phone
          );
        }
      }

      // Add state filter
      if (states.length > 0) {
        const zipCodesForStates = Object.entries(ZIP_DATA)
          .filter(([_, info]) => states.includes(info.state))
          .map(([zipCode]) => zipCode);
        whereConditions.push(`zip_code IN (${zipCodesForStates.map(() => '?').join(',')})`);
        params.push(...zipCodesForStates);
      }

      // Add carrier filter
      if (carriers.length > 0) {
        whereConditions.push(`(${carriers.map(() => 'current_carrier LIKE ?').join(' OR ')})`);
        params.push(...carriers.map(c => `%${c}%`));
      }

      // Add agent filter
      if (agents.length > 0) {
        whereConditions.push(`agent_id IN (${agents.map(() => '?').join(',')})`);
        params.push(...agents);
      }

      // Combine conditions
      const whereClause = whereConditions.join(' AND ');

      // Get organization database with retry logic
      let orgDb;
      try {
        orgDb = await Database.getOrInitOrgDb(user.organization_id.toString());
      } catch (error) {
        // If database not found, check if it exists in the list
        if (error instanceof Error && error.message.includes('database not configured')) {
          logger.info(`Database not found for org ${user.organization_id}, checking database list...`);
          
          const tursoService = new TursoService();
          const mainDb = new Database();
          
          // Get the org's database URL
          const orgData = await mainDb.fetchOne<{ turso_db_url: string }>(
            'SELECT turso_db_url FROM organizations WHERE id = ?',
            [user.organization_id]
          );

          if (!orgData?.turso_db_url) {
            logger.error(`No database URL found for org ${user.organization_id}`);
            set.status = 500;
            return { error: 'Organization database not configured' };
          }

          // Extract database name from URL
          const dbName = orgData.turso_db_url.split('/').pop()?.split('.')[0]?.replace(/-[^-]*$/, '');
          if (!dbName) {
            logger.error(`Could not extract database name from URL: ${orgData.turso_db_url}`);
            set.status = 500;
            return { error: 'Invalid database configuration' };
          }

          // Check if database exists in Turso
          const response = await fetch(`https://api.turso.tech/v1/organizations/${TURSO_CONFIG.ORG_SLUG}/databases`, {
            method: 'GET',
            headers: {
              'Authorization': `Bearer ${TURSO_CONFIG.API_TOKEN}`,
            }
          });

          if (!response.ok) {
            logger.error(`Failed to list databases, status: ${response.status}`);
            set.status = 500;
            return { error: 'Failed to verify database status' };
          }

          const data = await response.json();
          const dbExists = data.databases.some((db: any) => db.Name === dbName);

          if (dbExists) {
            logger.info(`Database ${dbName} found in list, retrying connection...`);
            // Retry getting the database
            try {
              orgDb = await Database.getOrgDb(user.organization_id.toString());
            } catch (retryError) {
              logger.error(`Failed to connect to database after verification: ${retryError}`);
              set.status = 500;
              return { error: 'Database exists but connection failed' };
            }
          } else {
            logger.error(`Database ${dbName} not found in Turso organization`);
            set.status = 500;
            return { error: 'Database not found in organization' };
          }
        } else {
          // For other errors, throw them
          throw error;
        }
      }

      logger.info(`Connected to org database for ${user.organization_id}`);

      // First get total count with a simpler query
      const countQuery = `SELECT COUNT(*) as total FROM contacts WHERE ${whereClause}`;
      logger.info(`Executing count query: ${countQuery} with params: ${JSON.stringify(params)}`);
      
      const countResult = await orgDb.fetchOne<{ total: number }>(countQuery, params);
      let total = countResult?.total || 0;
      logger.info(`Found total of ${total} contacts matching criteria`);

      // If no contacts found, verify database is properly synced
      if (total === 0) {
        logger.info(`No contacts found for org ${user.organization_id}, verifying database sync status...`);
        
        const mainDb = new Database();
        
        // Get the org's database URL
        const orgData = await mainDb.fetchOne<{ turso_db_url: string }>(
          'SELECT turso_db_url FROM organizations WHERE id = ?',
          [user.organization_id]
        );

        if (!orgData?.turso_db_url) {
          logger.error(`No database URL found for org ${user.organization_id}`);
          set.status = 500;
          return { error: 'Organization database not configured' };
        }

        // Extract database name from URL
        const dbName = orgData.turso_db_url.split('/').pop()?.split('.')[0]?.replace(/-[^-]*$/, '');
        if (!dbName) {
          logger.error(`Could not extract database name from URL: ${orgData.turso_db_url}`);
          set.status = 500;
          return { error: 'Invalid database configuration' };
        }

        // Check if database exists in Turso
        const response = await fetch(`https://api.turso.tech/v1/organizations/${TURSO_CONFIG.ORG_SLUG}/databases`, {
          method: 'GET',
          headers: {
            'Authorization': `Bearer ${TURSO_CONFIG.API_TOKEN}`,
          }
        });

        if (!response.ok) {
          logger.error(`Failed to list databases, status: ${response.status}`);
          set.status = 500;
          return { error: 'Failed to verify database status' };
        }

        const data = await response.json();
        const dbExists = data.databases.some((db: any) => db.Name === dbName);

        if (dbExists) {
          logger.info(`Database ${dbName} found in list, retrying connection and count...`);
          // Retry getting the database and count
          try {
            orgDb = await Database.getOrgDb(user.organization_id.toString());
            const retryCountResult = await orgDb.fetchOne<{ total: number }>(countQuery, params);
            const retryTotal = retryCountResult?.total || 0;
            logger.info(`Retry count found ${retryTotal} contacts`);
            if (retryTotal > 0) {
              total = retryTotal; // Update total if we found contacts on retry
            }
          } catch (retryError) {
            logger.error(`Failed to reconnect to database after verification: ${retryError}`);
            set.status = 500;
            return { error: 'Database exists but connection failed' };
          }
        } else {
          logger.error(`Database ${dbName} not found in Turso organization`);
          set.status = 500;
          return { error: 'Database not found in organization' };
        }
      }

      // Then get paginated results
      const offset = (page - 1) * limit;
      const selectQuery = `
        SELECT 
          COALESCE(id, rowid) as id,
          first_name, last_name, email, phone_number, state,
          current_carrier, effective_date, birth_date, tobacco_user,
          gender, zip_code, plan_type, agent_id, last_emailed,
          created_at, updated_at, status
        FROM contacts 
        WHERE ${whereClause}
        ORDER BY created_at DESC 
        LIMIT ? OFFSET ?`;
      
      logger.info(`Executing select query: ${selectQuery} with params: ${JSON.stringify([...params, limit, offset])}`);
      
      const contacts = await orgDb.query<Contact>(selectQuery, [...params, limit, offset]);
      logger.info(`Retrieved ${contacts.length} contacts for current page`);

      // Log first contact for debugging
      if (contacts.length > 0) {
        logger.info(`First contact: ${JSON.stringify(contacts[0])}`);
      }

      // Get filter options using separate queries for unique values
      const carrierQuery = `SELECT DISTINCT current_carrier FROM contacts WHERE ${whereClause} AND current_carrier IS NOT NULL ORDER BY current_carrier`;
      const zipQuery = `SELECT DISTINCT zip_code FROM contacts WHERE ${whereClause} AND zip_code IS NOT NULL ORDER BY zip_code`;
      
      const [carrierRows, zipRows] = await Promise.all([
        orgDb.query<{current_carrier: string}>(carrierQuery, params),
        orgDb.query<{zip_code: string}>(zipQuery, params)
      ]);

      // Get unique states from zip codes using ZIP_DATA
      const uniqueStates = zipRows
        .map(row => {
          const zipInfo = ZIP_DATA[row.zip_code];
          return zipInfo?.state;
        })
        .filter((state): state is string => state !== undefined)
        .filter((value, index, self) => self.indexOf(value) === index)
        .sort();
      
      const filterOptions = {
        carriers: carrierRows.map(row => row.current_carrier).filter(Boolean),
        states: uniqueStates
      };
      
      logger.info(`Filter options - carriers: ${filterOptions.carriers.join(',')}, states: ${filterOptions.states.join(',')}`);

      // Map contacts to expected format using snake_case
      const mappedContacts = contacts.map(contact => {
        // Get state from ZIP code
        const zipInfo = ZIP_DATA[contact.zip_code];
        const state = zipInfo?.state || contact.state; // Fallback to stored state if ZIP lookup fails

        // Ensure we have a valid ID
        if (!contact.id) {
          logger.error(`Contact missing ID: ${JSON.stringify(contact)}`);
        }

        return {
          id: contact.id || 0, // Fallback to 0 if null (shouldn't happen with COALESCE)
          first_name: contact.first_name,
          last_name: contact.last_name,
          email: contact.email,
          phone_number: contact.phone_number || '',
          state: state,
          current_carrier: contact.current_carrier,
          effective_date: contact.effective_date,
          birth_date: contact.birth_date,
          tobacco_user: Boolean(contact.tobacco_user),
          gender: contact.gender,
          zip_code: contact.zip_code,
          plan_type: contact.plan_type,
          agent_id: contact.agent_id,
          last_emailed: contact.last_emailed,
          status: contact.status || 'New'
        };
      });

      const response = {
        contacts: mappedContacts,
        filterOptions,
        total,
        page,
        limit
      };

      logger.info(`Returning response with ${mappedContacts.length} contacts, total: ${total}`);
      return response;

    } catch (error) {
      logger.error(`Error fetching contacts: ${error}`);
      set.status = 500;
      return { error: 'Failed to fetch contacts' };
    }
  })
  .post('/bulk-import', async ({ body, user, set }: { body: BulkImportRequest; user: User; set: { status: number } }) => {
    if (!user || !user.organization_id || !user.is_admin) {
      set.status = 401;
      return { success: false, message: 'Not authorized for bulk import', totalRows: 0 };
    }

    try {
      // For non-admin agents, force overwriteExisting to false
      const overwriteExisting = user.is_admin ? body.overwriteExisting : false;

      // Validate contacts array
      if (!Array.isArray(body.contacts) || body.contacts.length === 0) {
        set.status = 400;
        return { success: false, message: 'No contacts provided', totalRows: 0 };
      }

      // Create temp directory if it doesn't exist
      const tempDir = path.join(process.cwd(), 'tmp');
      if (!fs.existsSync(tempDir)) {
        fs.mkdirSync(tempDir, { recursive: true });
      }

      // Generate temp file name with random ID to avoid conflicts
      const tempFile = path.join(tempDir, `contacts-${nanoid()}.csv`);
      
      try {
        // Convert contacts array to CSV string
        const csvData = stringify(body.contacts, {
          header: true,
          columns: [
            'first_name', 'last_name', 'email', 'phone_number', 'state',
            'current_carrier', 'effective_date', 'birth_date', 'tobacco_user',
            'gender', 'zip_code', 'plan_type'
          ]
        });
        
        // Write CSV data to temp file
        fs.writeFileSync(tempFile, csvData);
        logger.info(`Created temporary CSV file: ${tempFile} with ${body.contacts.length} contacts`);
        
        // Use the bulk import function with the temp CSV file
        const result = await Database.bulkImportContacts(
          user.organization_id.toString(),
          tempFile,
          overwriteExisting,
          undefined, // columnMapping
          undefined, // carrierMapping
          body.agentId // Pass the agentId
        );
        
        logger.info(`Bulk import completed: ${result}`);
        
        return {
          success: true,
          message: 'Contacts imported successfully',
          totalRows: body.contacts.length
        };
      } finally {
        // Clean up temp file
        if (fs.existsSync(tempFile)) {
          fs.unlinkSync(tempFile);
          logger.info(`Removed temporary CSV file: ${tempFile}`);
        }
      }
    } catch (error) {
      logger.error(`Error in bulk import: ${error}`);
      set.status = 500;
      return { 
        success: false,
        message: error instanceof Error ? error.message : 'Failed to process import',
        totalRows: 0
      };
    }
  })
  .post('/track', 
    async ({ request, body, set }) => {
      try {
        const user = await getUserFromSession(request);
        
        // Check if request includes required fields
        if (!user || !body || 'skip_auth' in user) {
          set.status = 400;
          return { 
            success: false, 
            error: 'Invalid request' 
          };
        }
        
        const { email, firstName, lastName } = body as {
          email: string;
          firstName?: string;
          lastName?: string;
        };
        
        if (!email) {
          set.status = 400;
          return { 
            success: false, 
            error: 'Email is required' 
          };
        }
        
        // Track the contact
        const result = await trackContact(
          user.organization_id.toString(),
          user.id.toString(),
          email,
          firstName,
          lastName
        );
        
        // Get updated usage stats
        const stats = await getContactUsageStats(user.organization_id.toString());
        
        return {
          success: true,
          isNew: result.isNew,
          contactId: result.contactId,
          stats
        };
      } catch (error) {
        logger.error(`Error tracking contact: ${error}`);
        set.status = 500;
        return { 
          success: false, 
          error: 'Failed to track contact' 
        };
      }
    },
    {
      body: t.Object({
        email: t.String(),
        firstName: t.Optional(t.String()),
        lastName: t.Optional(t.String())
      })
    }
  )
  
  // Track a batch of contacts
  .post('/batch', 
    async ({ request, body, set }) => {
      try {
        const user = await getUserFromSession(request);
        
        // Check if request includes required fields
        if (!user || !body || !body.contacts || !Array.isArray(body.contacts) || 'skip_auth' in user) {
          set.status = 400;
          return { 
            success: false, 
            error: 'Invalid request' 
          };
        }
        
        const { contacts } = body as {
          contacts: Array<{
            email: string;
            firstName?: string;
            lastName?: string;
          }>
        };
        
        // Track the contacts in batch
        const result = await trackContactBatch(
          user.organization_id.toString(),
          user.id.toString(),
          contacts
        );
        
        // Get updated usage stats
        const stats = await getContactUsageStats(user.organization_id.toString());
        
        return {
          success: true,
          newCount: result.newCount,
          totalProcessed: result.totalProcessed,
          stats
        };
      } catch (error) {
        logger.error(`Error tracking contact batch: ${error}`);
        set.status = 500;
        return { 
          success: false, 
          error: 'Failed to track contacts' 
        };
      }
    },
    {
      body: t.Object({
        contacts: t.Array(
          t.Object({
            email: t.String(),
            firstName: t.Optional(t.String()),
            lastName: t.Optional(t.String())
          })
        )
      })
    }
  )
  
  // Get contact usage stats
  .get('/usage-stats', async ({ request, set }) => {
    try {
      const user = await getUserFromSession(request);
      
      if (!user || 'skip_auth' in user) {
        set.status = 401;
        return { 
          success: false, 
          error: 'Unauthorized' 
        };
      }
      
      const stats = await getContactUsageStats(user.organization_id.toString());
      
      return {
        success: true,
        stats
      };
    } catch (error) {
      logger.error(`Error getting contact usage stats: ${error}`);
      set.status = 500;
      return { 
        success: false, 
        error: 'Failed to get contact usage stats' 
      };
    }
  })
  
  // Reset contact count (admin/support only)
  .post('/reset', 
    async ({ request, body, set }) => {
      try {
        const user = await getUserFromSession(request);
        
        // Check if user is admin
        if (!user || 'skip_auth' in user || !user.is_admin) {
          set.status = 403;
          return { 
            success: false, 
            error: 'You do not have permission to perform this action' 
          };
        }
        
        const { email, reason } = body as {
          email: string;
          reason: string;
        };
        
        if (!email || !reason) {
          set.status = 400;
          return { 
            success: false, 
            error: 'Email and reason are required' 
          };
        }
        
        // Reset the contact
        const success = await resetContactCount(
          user.organization_id.toString(),
          email,
          reason
        );
        
        if (!success) {
          set.status = 404;
          return { 
            success: false, 
            error: 'Contact not found' 
          };
        }
        
        // Get updated stats
        const stats = await getContactUsageStats(user.organization_id.toString());
        
        return {
          success: true,
          stats
        };
      } catch (error) {
        logger.error(`Error resetting contact: ${error}`);
        set.status = 500;
        return { 
          success: false, 
          error: 'Failed to reset contact' 
        };
      }
    },
    {
      body: t.Object({
        email: t.String(),
        reason: t.String()
      })
    }
  )
  
  // Check if an email exists
  .get('/check-email/:email', async ({ params, request, set }) => {
    try {
      const user = await getUserFromSession(request);
      
      if (!user || 'skip_auth' in user) {
        set.status = 401;
        return { 
          success: false, 
          error: 'Unauthorized' 
        };
      }
      
      const { email } = params;
      
      if (!email) {
        set.status = 400;
        return { 
          success: false, 
          error: 'Email is required' 
        };
      }
      
      try {
        // Get the organization database
        const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString());
        
        // Check if the email exists
        const contact = await orgDb.fetchOne(
          'SELECT id FROM contacts WHERE email = ?',
          [email]
        );
        
        logger.info(`Email check for "${email}" - exists: ${!!contact}`);
        
        return {
          exists: !!contact
        };
      } catch (dbError) {
        logger.error(`Database error checking email ${email}: ${dbError}`);
        set.status = 500;
        return { 
          success: false, 
          error: 'Database error checking email',
          message: dbError instanceof Error ? dbError.message : 'Unknown database error'
        };
      }
    } catch (error) {
      logger.error(`Error checking email: ${error}`);
      set.status = 500;
      return { 
        success: false, 
        error: 'Failed to check email',
        message: error instanceof Error ? error.message : 'Unknown error'
      };
    }
  })
  
  // Add DELETE endpoint for contacts
  .delete('/', 
    async ({ body, set, request }) => {
      try {
        const user = await getUserFromSession(request);
        if (!user || 'skip_auth' in user || !user.organization_id) {
          set.status = 401;
          return { error: 'Not authorized' };
        }

        const contactIds = body;
        if (!Array.isArray(contactIds) || contactIds.length === 0) {
          throw new Error('No contact IDs provided');
        }
        
        logger.info(`DELETE /api/contacts - Attempting to delete ${contactIds.length} contacts for org ${user.organization_id}`);
        
        // Get org-specific database
        const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString());

        // Create placeholders for SQL IN clause
        const placeholders = contactIds.map(() => '?').join(',');
        
        // First delete related records from email_send_tracking
        const deleteEmailTrackingQuery = `
          DELETE FROM email_send_tracking 
          WHERE contact_id IN (${placeholders})
        `;
        await orgDb.execute(deleteEmailTrackingQuery, contactIds);

        // Then delete the contacts
        const deleteContactsQuery = `
          DELETE FROM contacts 
          WHERE id IN (${placeholders})
          RETURNING id
        `;

        const result = await orgDb.execute(deleteContactsQuery, contactIds);
        const deletedIds = result.rows?.map((row: { id: number }) => row.id) || [];

        logger.info(`DELETE /api/contacts - Successfully deleted ${deletedIds.length} contacts from org ${user.organization_id}`);

        return {
          success: true,
          deleted_ids: deletedIds,
          message: `Successfully deleted ${deletedIds.length} contacts`
        };
      } catch (e) {
        logger.error(`Error deleting contacts: ${e}`);
        throw new Error(String(e));
      }
    },
    {
      body: t.Array(t.Number())
    }
  );