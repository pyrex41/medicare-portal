import { Elysia, t } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import crypto from 'crypto';
import { config } from '../config';
import { generateQuoteId } from '../utils/quoteId';
import { getUserFromSession } from '../services/auth';
import { readFileSync } from 'fs';

// Import ZIP_DATA
interface ZipInfo {
  state: string;
  counties: string[];
}

// Load ZIP data
let ZIP_DATA: Record<string, ZipInfo> = {};
try {
  ZIP_DATA = JSON.parse(readFileSync('../zipData.json', 'utf-8'));
} catch (e) {
  logger.error(`Error loading ZIP data: ${e}`);
}

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
                  last_name AS lastName
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
      const { orgId, email, firstName, lastName, optInQuarterlyUpdates, zipCode, dateOfBirth, gender, tobacco, phoneNumber, currentPremium, currentCarrier, state, county } = body as {
        orgId: string;
        email: string;
        firstName: string;
        lastName: string;
        optInQuarterlyUpdates: boolean;
        zipCode: string;
        dateOfBirth: string;
        gender: string;
        tobacco: boolean;
        phoneNumber: string;
        currentPremium: string;
        currentCarrier: string;
        state: string;
        county: string;
      };

      // Validate required parameters
      if (!orgId || !email || !firstName || !lastName || !zipCode || !dateOfBirth || !gender || !state) {
        set.status = 400;
        return { error: 'Missing required fields' };
      }
      
      // Only proceed if the user has agreed to receive updates
      if (!optInQuarterlyUpdates) {
        set.status = 400;
        return { error: 'You must agree to receive updates to continue' };
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

        if (existingContact.rows.length > 0) {
          // Update existing contact
          const contactId = existingContact.rows[0].id;
          await client.execute({
            sql: `UPDATE contacts SET 
                  first_name = ?, 
                  last_name = ?,
                  phone_number = ?,
                  zip_code = ?,
                  state = ?,
                  gender = ?,
                  birth_date = ?,
                  tobacco_user = ?,
                  current_carrier = ?
                  WHERE id = ?`,
            args: [
              firstName, 
              lastName, 
              phoneNumber || '', 
              zipCode, 
              state,
              gender,
              dateOfBirth,
              tobacco ? 1 : 0,
              currentCarrier || '', 
              contactId
            ]
          });
          
          logger.info(`Updated contact ${contactId} for email ${email} in organization ${orgId}`);
          
          return { 
            success: true,
            contactId,
            email
          };
        } else {
          // Create new contact with required fields
          const result = await client.execute({
            sql: `INSERT INTO contacts (
                  email, 
                  first_name, 
                  last_name,
                  phone_number,
                  zip_code,
                  state,
                  gender,
                  birth_date,
                  tobacco_user,
                  current_carrier,
                  effective_date
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
            args: [
              email, 
              firstName, 
              lastName, 
              phoneNumber || '', 
              zipCode, 
              state,
              gender,
              dateOfBirth,
              tobacco ? 1 : 0,
              currentCarrier || null, // Default plan type
              new Date().toISOString().split('T')[0] // Today as effective date
            ]
          });
          
          // Get the ID of the newly created contact
          const newContactResult = await client.execute({
            sql: 'SELECT id FROM contacts WHERE email = ?',
            args: [email]
          });
          
          const contactId = newContactResult.rows[0]?.id;
          
          logger.info(`Created new contact ${contactId} for email ${email} in organization ${orgId}`);
          
          return { 
            success: true,
            contactId,
            email
          };
        }
      } catch (error) {
        logger.error(`Error in self-service signup endpoint: ${error}`);
        set.status = 500;
        return { error: 'Internal server error' };
      }
    })
    .post('/api/self-service/update-location', async ({ body, set }) => {
      const { orgSlug, contactId, zipCode } = body as {
        orgSlug: string;
        contactId: string;
        zipCode: string;
      };

      // Validate required parameters
      if (!orgSlug || !contactId || !zipCode) {
        set.status = 400;
        return { error: 'Missing required fields' };
      }

      try {
        // Get state and county from ZIP_DATA
        const zipInfo = ZIP_DATA[zipCode];
        if (!zipInfo) {
          set.status = 400;
          return { error: 'Invalid zip code' };
        }
        logger.info(`Zip info: ${JSON.stringify(zipInfo)}`);

        // Get organization ID from slug
        const db = new Database();
        const orgResult = await db.fetchOne<{ id: number }>(
          'SELECT id FROM organizations WHERE slug = ?',
          [orgSlug]
        );

        if (!orgResult) {
          set.status = 404;
          return { error: 'Organization not found' };
        }

        // Get organization database
        const orgDb = await Database.getOrgDb(orgResult.id.toString());
        const client = orgDb.getClient();

  

        // Use provided county or first county if only one available

        // Update contact's zip code, state and county
        await client.execute({
          sql: `UPDATE contacts SET 
                zip_code = ?
                WHERE id = ?`,
          args: [
            zipCode,
            contactId
          ]
        });

        logger.info(`Updated location for contact ${contactId} in organization ${orgSlug} to ${zipCode}, ${zipInfo.state}`);

        let output = {
          success: true,
          zipCode,
          state: zipInfo.state,
          counties: zipInfo.counties
        };

        logger.info(`Output: ${JSON.stringify(output)}`);
        return output;

      } catch (error) {
        logger.error(`Error updating location: ${error}`);
        set.status = 500;
        return { error: 'Internal server error' };
      }
    })
    // Add a new endpoint to generate a quote for a contact
    .post('/api/self-service/generate-quote', async ({ body, set }) => {
      const { orgId, contactEmail } = body as {
        orgId: string;
        contactEmail: string;
      };

      logger.info(`Generate quote request received: orgId=${orgId}, contactEmail=${contactEmail}`);

      // Validate required parameters
      if (!orgId || !contactEmail) {
        logger.error(`Missing required fields: orgId=${orgId}, contactEmail=${contactEmail}`);
        set.status = 400;
        return { error: 'Missing required fields (orgId and contactEmail)' };
      }

      try {
        // Get organization database
        const orgDb = await Database.getOrgDb(orgId);
        const client = orgDb.getClient();
        
        // Lookup contact by email
        const contactResult = await client.execute({
          sql: 'SELECT id FROM contacts WHERE email = ?',
          args: [contactEmail]
        });

        if (contactResult.rows.length === 0) {
          logger.error(`Contact not found for email ${contactEmail} in organization ${orgId}`);
          set.status = 404;
          return { error: 'Contact not found' };
        }

        const contactId = contactResult.rows[0].id;
        logger.info(`Found contact ID ${contactId} for email ${contactEmail} in organization ${orgId}`);
        
        // Generate quote ID using the proper utility function (with base36 encoding)
        const quoteId = generateQuoteId(parseInt(orgId), contactId);
        
        // Get plan type from contact
        let planType = 'MedSupp';
        try {
          const planTypeResult = await client.execute({
            sql: 'SELECT plan_type FROM contacts WHERE id = ?',
            args: [contactId]
          });
          
          if (planTypeResult.rows.length > 0 && planTypeResult.rows[0].plan_type) {
            planType = planTypeResult.rows[0].plan_type;
          }
          logger.info(`Found plan type ${planType} for contact ${contactId}`);
        } catch (error) {
          logger.warn(`Could not get plan type for contact ${contactId}: ${error}`);
          // Continue with default plan type
        }
        
        // Build redirect URL with just the quote ID
        const redirectUrl = `${config.PUBLIC_URL || 'http://localhost:5173'}/quote?id=${quoteId}`;
        
        // Log response details
        logger.info(`Generated quote ID: ${quoteId}`);
        logger.info(`Redirect URL: ${redirectUrl}`);

        // Return successful response with quote information
        return {
          success: true,
          contactId,
          quoteId,
          redirectUrl
        };
      } catch (error) {
        logger.error(`Error generating quote ID: ${error}`);
        set.status = 500;
        return { error: 'Internal server error' };
      }
    })
    .get('/api/self-service/:orgSlug', async ({ params, query, set, request }) => {
      const { orgSlug } = params;
      const { email, id } = query as { email?: string; id?: string };
      
      logger.info(`Self-service request for orgSlug=${orgSlug}, email=${email || 'none'}, quoteId=${id || 'none'}`);
      
      try {
        // For the 'latest' slug, get the current user's organization
        if (orgSlug === 'latest') {
          const userFromSession = await getUserFromSession(request);
          if (!userFromSession || !userFromSession.organization_id) {
            set.status = 401;
            return { success: false, error: 'Unauthorized' };
          }
          
          // Get organization from central database
          const db = new Database();
          const orgResult = await db.fetchOne<{ id: number, slug: string, logo_data: string | null }>(
            'SELECT id, slug, logo_data FROM organizations WHERE id = ?',
            [userFromSession.organization_id]
          );
          
          if (!orgResult) {
            set.status = 404;
            return { success: false, message: 'Organization not found' };
          }
          
          return {
            success: true,
            orgId: userFromSession.organization_id.toString(),
            orgSlug: orgResult.slug,
            selfOnboardingUrl: `${config.PUBLIC_URL}/self-onboarding/${orgResult.slug}`,
            logo: orgResult.logo_data
          };
        }
        
        // Regular slug lookup
        const db = new Database();
        const result = await db.query(
          'SELECT id, logo_data FROM organizations WHERE slug = ?',
          [orgSlug]
        );

        if (!result || result.length === 0) {
          set.status = 404;
          return { success: false, message: 'Organization not found' };
        }

        const orgId = result[0].id;
        const orgIdStr = orgId.toString();
        const logo = result[0].logo_data;
        
        // Create response object with organization info
        const response = {
          success: true,
          orgId: orgIdStr,
          orgSlug,
          selfOnboardingUrl: `${config.PUBLIC_URL}/self-onboarding/${orgSlug}`,
          logo
        };
        
        // Try to find contact information if email or quoteId provided
        if (email || id) {
          try {
            const orgDb = await Database.getOrgDb(orgIdStr);
            const client = orgDb.getClient();
            let contactResult;
            
            // First try by email if provided
            if (email) {
              logger.info(`Looking up contact by email: ${email}`);
              contactResult = await client.execute({
                sql: `SELECT 
                  id,
                  email, 
                  first_name AS firstName, 
                  last_name AS lastName,
                  phone_number AS phone,
                  zip_code AS zipCode,
                  state,
                  gender,
                  birth_date AS dateOfBirth,
                  tobacco_user AS tobacco,
                  current_carrier AS currentCarrier,
                  plan_type AS planType
                FROM contacts 
                WHERE email = ?`,
                args: [email]
              });
            }
            
            // If no results and we have a quoteId, try that
            if ((!contactResult || contactResult.rows.length === 0) && id) {
              // Extract contactId from quoteId (base36 encoded)
              try {
                logger.info(`Looking up contact by quoteId: ${id}`);
                // Decode the quoteId to get contactId
                const decoded = id.split('-');
                if (decoded.length >= 2) {
                  const contactId = parseInt(decoded[1], 36);
                  
                  contactResult = await client.execute({
                    sql: `SELECT 
                      id,
                      email, 
                      first_name AS firstName, 
                      last_name AS lastName,
                      phone_number AS phone,
                      zip_code AS zipCode,
                      state,
                      gender,
                      birth_date AS dateOfBirth,
                      tobacco_user AS tobacco,
                      current_carrier AS currentCarrier,
                      plan_type AS planType
                    FROM contacts 
                    WHERE id = ?`,
                    args: [contactId]
                  });
                }
              } catch (err) {
                logger.warn(`Error decoding quoteId ${id}: ${err}`);
              }
            }
            
            // If we found a contact, add it to the response
            if (contactResult && contactResult.rows.length > 0) {
              const contact = contactResult.rows[0];
              // Convert tobacco_user from number to boolean
              contact.tobacco = contact.tobacco === 1;
              
              logger.info(`Found contact for ${email || id}`);
              let output = {
                ...response,
                contact: {
                  ...contact,
                  id: contactResult.rows[0].id
                }
              };
              logger.info(`Output: ${JSON.stringify(output)}`);
              return output;
            }
          } catch (contactError) {
            logger.error(`Error looking up contact: ${contactError}`);
            // Continue without contact info
          }
        }
        
        // Return basic organization info for the frontend if no contact was found
        return response;
      } catch (error) {
        logger.error(`Error in self-service org slug endpoint: ${error}`);
        set.status = 500;
        return { error: 'Internal server error' };
      }
    });
} 