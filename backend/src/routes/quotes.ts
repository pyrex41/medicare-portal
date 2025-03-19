import { Elysia } from 'elysia';
import axios from 'axios';
import { config } from '../config';
import { logger } from '../logger';
import { Database } from '../database';
import { generateQuoteId, decodeQuoteId } from '../utils/quoteId';
import { getUserFromSession } from '../services/auth';
import { readFile } from 'fs/promises';
import { join } from 'path';
import { readFileSync } from 'fs';


interface ZipInfo {
  state: string;
  // Add other ZIP info properties as needed
}


// Update ZIP_DATA declaration
let ZIP_DATA: Record<string, ZipInfo> = {}
try {
  ZIP_DATA = JSON.parse(readFileSync('../zipData.json', 'utf-8'))
} catch (e) {
  logger.error(`Error loading ZIP data: ${e}`)
}


interface Quote {
    age: number;
    gender: string;
    plan: string;
    tobacco: number;
    rate: number;
    discount_rate: number;
    discount_category: string;
}

interface QuoteResponse {
    naic: string;
    group: number;
    company_name: string;
    quotes: Quote[];
}

interface QuoteRequestBody {
    zip_code: string;
    state: string;
    age: string | number;
    tobacco: string | boolean;
    gender: string;
    county?: string;
}

interface QuoteRequest {
    zip_code: string;
    state: string;
    age: number;
    tobacco: boolean;
    gender: string;
    plans: string[];
    carriers: string;
    county?: string;
}

interface ContactQuoteInfo {
    zip_code: string;
    birth_date: string;
    tobacco_user: number;
    gender: string;
    email: string;
    first_name: string;
    last_name: string;
    current_carrier: string;
    phone_number: string;
}

// Add new interface for org info response
interface OrgRedirectInfo {
    redirect_url: string | null;
    agent_name: string;
}

export const quotesRoutes = (app: Elysia) => {
    app
    .get('/api/quotes/generate/:contactId', async ({ params, request }) => {
        try {
            const user = await getUserFromSession(request);
            if (!user?.organization_id) {
                throw new Error('No organization ID found in session');
            }

            const contactId = parseInt(params.contactId);
            if (isNaN(contactId)) {
                throw new Error('Invalid contact ID');
            }

            // Get org-specific database
            const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString());
            
            // Verify contact exists and belongs to this org
            const contact = await orgDb.fetchOne(
                'SELECT id FROM contacts WHERE id = ?',
                [contactId]
            );

            if (!contact) {
                throw new Error('Contact not found');
            }

            // Generate quote ID
            const quoteId = generateQuoteId(user.organization_id, contactId);

            return {
                success: true,
                quoteId,
                redirectUrl: `${process.env.PUBLIC_URL || 'http://localhost:5173'}/quote?id=${quoteId}`
            };
        } catch (e) {
            logger.error(`Error generating quote ID: ${e}`);
            throw new Error(String(e));
        }
    })
    .get('/api/quotes/decode/:quoteId', async ({ params, set }) => {
        try {
            logger.info(`Decoding quote ID: ${params.quoteId}`);
            
            const decoded = decodeQuoteId(params.quoteId);
            if (!decoded) {
                logger.error(`Invalid quote ID format: ${params.quoteId}`);
                set.status = 400;
                return {
                    success: false,
                    error: 'Invalid quote ID format'
                };
            }
            
            logger.info(`Decoded quote ID: orgId=${decoded.orgId}, contactId=${decoded.contactId}`);

            // Get org-specific database
            logger.info(`Getting database for org: ${decoded.orgId}`);
            const orgDb = await Database.getOrInitOrgDb(decoded.orgId.toString());
            
            const mainDb = new Database();
            logger.info(`Fetching organization details for orgId: ${decoded.orgId}`);
            const result = await mainDb.fetchOne<{ slug: string, org_settings: string }>(
                'SELECT slug, org_settings FROM organizations WHERE id = ?',
                [decoded.orgId]
            );

            if (!result) {
                logger.error(`Organization not found: ${decoded.orgId}`);
                set.status = 404;
                return {
                    success: false,
                    error: 'Organization not found'
                };
            }

            const orgSlug = result.slug;
            logger.info(`Found organization: ${orgSlug} (ID: ${decoded.orgId})`);
            
            let orgSettings;
            try {
                orgSettings = JSON.parse(result.org_settings || '{}');
            } catch (e) {
                logger.warn(`Error parsing org settings for ${decoded.orgId}: ${e}`);
                orgSettings = {};
            }
            
            const carrierContracts = orgSettings?.carrierContracts || [];
            
            // Fetch contact details with detailed logging
            const contactQuery = 'SELECT zip_code, birth_date, tobacco_user, gender, email, first_name, last_name, current_carrier, phone_number, plan_type FROM contacts WHERE id = ?';
            const contactParams = [decoded.contactId];
            
            logger.info(`Executing contact query: ${contactQuery} with params: [${contactParams}]`);
            
            const contact = await orgDb.fetchOne<ContactQuoteInfo>(contactQuery, contactParams);

            if (!contact) {
                logger.error(`Contact not found: contactId=${decoded.contactId} in orgId=${decoded.orgId}`);
                
                // Additional debugging: List all contacts in this org
                try {
                    const allContacts = await orgDb.query('SELECT id, email, first_name, last_name FROM contacts LIMIT 5');
                    logger.info(`First 5 contacts in org ${decoded.orgId}: ${JSON.stringify(allContacts)}`);
                } catch (e) {
                    logger.error(`Error listing contacts: ${e}`);
                }
                
                set.status = 404;
                return {
                    success: false,
                    error: `Contact not found: ID=${decoded.contactId}`
                };
            }

            logger.info(`Found contact: ${contact.first_name} ${contact.last_name} (ID: ${decoded.contactId})`);
            
            const zipInfo = ZIP_DATA[contact.zip_code];
            const contactState = zipInfo?.state;
            
            if (!contactState) {
                logger.warn(`No state found for zip code: ${contact.zip_code}`);
            }

            const output = {
                success: true,
                orgId: decoded.orgId.toString(),
                orgSlug: orgSlug || null,
                carrierContracts: carrierContracts || null,
                contact: {
                    zipCode: contact.zip_code,
                    state: contactState,
                    dateOfBirth: contact.birth_date,
                    tobacco: Boolean(contact.tobacco_user),
                    gender: contact.gender,
                    email: contact.email,
                    firstName: contact.first_name,
                    lastName: contact.last_name,
                    currentCarrier: contact.current_carrier,
                    planType: contact.plan_type,
                    phoneNumber: contact.phone_number
                }
            };

            logger.info(`Returning success response for quote ID: ${params.quoteId}`);
            return output;
        } catch (e) {
            logger.error(`Error decoding quote ID: ${e}`);
            set.status = 500;
            return {
                success: false,
                error: String(e)
            };
        }
    })
    .post('/api/quotes', async ({ body }: { body: QuoteRequestBody }) => {
        try {
            // Format request body
            const requestBody: QuoteRequest = {
                zip_code: body.zip_code,
                state: body.state,
                age: Number(body.age),
                tobacco: body.tobacco === 'true' || body.tobacco === true,
                gender: body.gender,
                plans: ['G', 'N'],
                carriers: 'supported',
                county: body.county
            };

            // Log incoming request details
            logger.info(`Incoming quote request body: ${JSON.stringify(requestBody, null, 2)}`);
            
            // Construct request config
            const quoteEngineUrl = 'https://quote-engine.replit.app/quotes/';
            const requestConfig = {
                url: quoteEngineUrl,
                method: 'POST' as const,
                headers: {
                    'X-API-Key': config.quoteApiKey,
                    'Accept': 'application/json',
                    'Content-Type': 'application/json'
                },
                data: requestBody
            };

            // Make request to quote engine API
            const response = await axios(requestConfig);
            
            logger.info(`Quote engine response status: ${response.status}`);
            logger.info(`Quote engine response data: ${JSON.stringify(response.data, null, 2)}`);
            
            // Return quotes from response data
            return response.data;
        } catch (error) {
            logger.error(`Error fetching quotes: ${error}`);
            throw new Error(String(error));
        }
    })
    .get('/api/contact-request/org-info/:orgId', async ({ params }) => {
        try {
            const db = new Database();
            
            // Get organization info including redirect URL
            const orgResult = await db.fetchOne<{ redirect_url: string | null }>(
                'SELECT redirect_url FROM organizations WHERE id = ?',
                [params.orgId]
            );

            if (!orgResult) {
                throw new Error('Organization not found');
            }

            // Get org-specific database
            const orgDb = await Database.getOrInitOrgDb(params.orgId);

            // Try to get default agent (first admin or agent)
            const defaultAgent = await orgDb.fetchOne<{ first_name: string, last_name: string }>(
                `SELECT first_name, last_name 
                FROM users 
                WHERE (is_admin = 1 OR is_agent = 1) 
                AND is_active = 1 
                ORDER BY is_admin DESC, id ASC 
                LIMIT 1`
            );

            if (!defaultAgent) {
                throw new Error('No active agents or admins found');
            }

            return {
                success: true,
                redirect_url: orgResult.redirect_url,
                agent_name: `${defaultAgent.first_name} ${defaultAgent.last_name}`
            };
        } catch (e) {
            logger.error(`Error getting org redirect info: ${e}`);
            throw new Error(String(e));
        }
    })
    .post('/api/contact-request', async ({ body }) => {
        try {
            const { name, email, type, quoteId } = body as { 
                name: string;
                email: string;
                type: 'accept' | 'decline' | 'generic';
                quoteId?: string;
            };

            // Get contact info from quoteId if available
            let orgId: number | undefined;
            let contactId: number | undefined;
            let agentName: string | undefined;

            if (!quoteId) {
                throw new Error('Quote ID is required');
            }

            const decoded = decodeQuoteId(quoteId);
            if (!decoded) {
                throw new Error('Invalid quote ID');
            }

            orgId = decoded.orgId;
            contactId = decoded.contactId;

            // Get org-specific database
            const orgDb = await Database.getOrInitOrgDb(orgId.toString());

            // If we have a contact ID, try to get the assigned agent's name
            if (contactId) {
                const contact = await orgDb.fetchOne<{ first_name: string, last_name: string }>(
                    `SELECT u.first_name, u.last_name 
                    FROM contacts c
                    JOIN users u ON c.agent_id = u.id
                    WHERE c.id = ? AND u.is_active = 1`,
                    [contactId]
                );
                if (contact) {
                    agentName = `${contact.first_name} ${contact.last_name}`;
                }
            }

            // If no agent assigned to contact, get default agent
            if (!agentName) {
                const defaultAgent = await orgDb.fetchOne<{ first_name: string, last_name: string }>(
                    `SELECT first_name, last_name 
                    FROM users 
                    WHERE (is_admin = 1 OR is_agent = 1) 
                    AND is_active = 1 
                    ORDER BY is_admin DESC, id ASC 
                    LIMIT 1`
                );
                if (defaultAgent) {
                    agentName = `${defaultAgent.first_name} ${defaultAgent.last_name}`;
                }
            }

            // Store request in org-specific database
            await orgDb.execute(
                `INSERT INTO contact_requests (
                    name, email, request_type, contact_id, status, agent_name, created_at
                ) VALUES (?, ?, ?, ?, 'new', ?, CURRENT_TIMESTAMP)`,
                [name, email, type, contactId || null, agentName || null]
            );

            // Get org redirect URL from central database
            const db = new Database();
            const orgResult = await db.fetchOne<{ redirect_url: string | null }>(
                'SELECT redirect_url FROM organizations WHERE id = ?',
                [orgId]
            );

            return { 
                success: true,
                redirect: orgResult?.redirect_url ? { redirect_url: orgResult.redirect_url } : null
            };
        } catch (e) {
            logger.error(`Error saving contact request: ${e}`);
            throw new Error(String(e));
        }
    })
    // Add new endpoint for zip code information
    .get('/api/zipinfo/:zipCode', async ({ params }) => {
        try {
            const { zipCode } = params;
            
            // Read the zip data file from the ROOT directory, not the backend directory
            const zipDataPath = join(process.cwd(), '..', 'zipData.json');
            logger.info(`Looking for zip data at: ${zipDataPath}`);
            const zipDataContent = await readFile(zipDataPath, 'utf-8');
            const zipData = JSON.parse(zipDataContent);
            
            // Look up the zip code
            if (zipData[zipCode]) {
                return {
                    success: true,
                    data: zipData[zipCode]
                };
            } else {
                return {
                    success: false,
                    error: 'Zip code not found'
                };
            }
        } catch (error) {
            logger.error(`Error fetching zip code info: ${error}`);
            return {
                success: false,
                error: 'Failed to fetch zip code information'
            };
        }
    })
    .get('/api/quotes/debug-generate/:orgId/:contactId', async ({ params }) => {
        try {
            logger.info(`Debug endpoint - generating quote ID for org: ${params.orgId}, contact: ${params.contactId}`);
            
            const orgId = parseInt(params.orgId);
            const contactId = parseInt(params.contactId);
            
            if (isNaN(orgId) || isNaN(contactId)) {
                return {
                    success: false,
                    error: 'Invalid organization or contact ID'
                };
            }
            
            // Get org-specific database to verify the contact exists
            const orgDb = await Database.getOrInitOrgDb(orgId.toString());
            
            // Verify contact exists
            const contact = await orgDb.fetchOne(
                'SELECT id FROM contacts WHERE id = ?',
                [contactId]
            );

            if (!contact) {
                // List first 5 contacts in this org for debugging
                const contacts = await orgDb.query('SELECT id FROM contacts LIMIT 5');
                logger.info(`Available contacts in org ${orgId}: ${JSON.stringify(contacts)}`);
                
                return {
                    success: false,
                    error: 'Contact not found',
                    availableContacts: contacts
                };
            }
            
            // Generate quote ID
            const quoteId = generateQuoteId(orgId, contactId);
            
            logger.info(`Generated debug quote ID: ${quoteId} for orgId: ${orgId}, contactId: ${contactId}`);

            return {
                success: true,
                quoteId,
                redirectUrl: `${process.env.PUBLIC_URL || 'http://localhost:5173'}/quote?id=${quoteId}`
            };
        } catch (e) {
            logger.error(`Error in debug quote generation: ${e}`);
            return {
                success: false,
                error: String(e)
            };
        }
    });

    return app;
}; 