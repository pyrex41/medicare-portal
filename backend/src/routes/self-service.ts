import { Elysia, t } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import crypto from 'crypto';
import { config } from '../config';

// Import generateHash function - reimplementing it since it's not exported from email.ts
function generateHash(orgId: string, email: string): string {
  const SECRET = process.env.HASH_SECRET || 'default-hash-secret';
  return crypto.createHash('sha256').update(`${orgId}${email}${SECRET}`).digest('hex').slice(0, 16);
}

export function createSelfServiceRoutes() {
  return new Elysia()
    .get('/api/self-service/init', async ({ query, set }) => {
      // Extract query parameters
      const { orgId, email, hash } = query as { 
        orgId?: string; 
        email?: string; 
        hash?: string;
      };

      // Validate required parameters
      if (!orgId) {
        set.status = 400;
        return { error: 'orgId is required' };
      }

      // Validate hash if email is provided
      if (email && hash) {
        const expectedHash = generateHash(orgId, email);
        if (hash !== expectedHash) {
          logger.warn(`Invalid hash for email ${email} and orgId ${orgId}`);
          set.status = 403;
          return { error: 'Invalid email or hash' };
        }
      }

      try {
        // Get organization database
        const orgDb = await Database.getOrgDb(orgId);
        
        // If email is provided, check for existing contact
        if (email) {
          const client = orgDb.getClient();
          const result = await client.execute({
            sql: `SELECT 
                  email, 
                  first_name AS firstName, 
                  last_name AS lastName, 
                  opt_in_quarterly_updates AS optInQuarterlyUpdates 
                FROM contacts 
                WHERE email = ?`,
            args: [email]
          });

          // If contact exists, return contact details
          if (result.rows.length > 0) {
            const contact = result.rows[0];
            logger.info(`Contact found for email ${email} in organization ${orgId}`);
            return { 
              contact, 
              email, 
              emailReadOnly: true 
            };
          }

          // If no contact exists but email is provided
          logger.info(`No contact found for email ${email} in organization ${orgId}`);
          return { 
            email, 
            emailReadOnly: true 
          };
        }

        // If no email is provided
        return { emailReadOnly: false };

      } catch (error) {
        logger.error(`Error in self-service init endpoint: ${error}`);
        set.status = 500;
        return { error: 'Internal server error' };
      }
    })
    .post('/api/self-service/signup', async ({ body, set }) => {
      const { orgId, email, firstName, lastName, optInQuarterlyUpdates } = body as {
        orgId: string;
        email: string;
        firstName: string;
        lastName: string;
        optInQuarterlyUpdates: boolean;
      };

      // Validate required parameters
      if (!orgId || !email || !firstName || !lastName) {
        set.status = 400;
        return { error: 'Missing required fields' };
      }

      try {
        // Get organization database
        const orgDb = await Database.getOrgDb(orgId);
        const client = orgDb.getClient();
        
        // Check if contact already exists
        const existingContact = await client.execute({
          sql: 'SELECT id FROM contacts WHERE email = ?',
          args: [email]
        });

        const timestamp = new Date().toISOString();

        if (existingContact.rows.length > 0) {
          // Update existing contact
          const contactId = existingContact.rows[0].id;
          await client.execute({
            sql: `UPDATE contacts SET 
                  first_name = ?, 
                  last_name = ?, 
                  opt_in_quarterly_updates = ?,
                  updated_at = ?
                  WHERE id = ?`,
            args: [firstName, lastName, optInQuarterlyUpdates ? 1 : 0, timestamp, contactId]
          });
          
          logger.info(`Updated contact ${contactId} for email ${email} in organization ${orgId}`);
        } else {
          // Create new contact
          const result = await client.execute({
            sql: `INSERT INTO contacts (
                  email, 
                  first_name, 
                  last_name, 
                  opt_in_quarterly_updates,
                  created_at,
                  updated_at
                ) VALUES (?, ?, ?, ?, ?, ?)`,
            args: [email, firstName, lastName, optInQuarterlyUpdates ? 1 : 0, timestamp, timestamp]
          });
          
          logger.info(`Created new contact for email ${email} in organization ${orgId}`);
        }

        return { success: true };
      } catch (error) {
        logger.error(`Error in self-service signup endpoint: ${error}`);
        set.status = 500;
        return { error: 'Internal server error' };
      }
    })
    .get('/api/self-service/:orgSlug', async ({ params, set, request }) => {
      const { orgSlug } = params;
      
      try {
        // For the 'latest' slug, get the current user's organization
        if (orgSlug === 'latest') {
          const userFromSession = await Database.getUserFromSession(request);
          if (!userFromSession) {
            set.status = 401;
            return { success: false, error: 'Unauthorized' };
          }
          
          const orgResult = await Database.getOrganizationById(userFromSession.organization_id);
          if (!orgResult) {
            set.status = 404;
            return { success: false, message: 'Organization not found' };
          }
          
          return {
            success: true,
            orgId: userFromSession.organization_id.toString(),
            orgSlug: orgResult.slug,
            selfOnboardingUrl: `${config.PUBLIC_URL}/self-onboarding/${orgResult.slug}`
          };
        }
        
        // Regular slug lookup
        const db = new Database();
        const result = await db.query(
          'SELECT id FROM organizations WHERE slug = ?',
          [orgSlug]
        );

        if (!result || result.length === 0) {
          set.status = 404;
          return { success: false, message: 'Organization not found' };
        }

        const orgId = result[0].id;
        
        // Return basic organization info for the frontend
        return {
          success: true,
          orgId: orgId.toString(),
          orgSlug,
          selfOnboardingUrl: `${config.PUBLIC_URL}/self-onboarding/${orgSlug}`
        };
      } catch (error) {
        logger.error(`Error in self-service org slug endpoint: ${error}`);
        set.status = 500;
        return { error: 'Internal server error' };
      }
    });
} 