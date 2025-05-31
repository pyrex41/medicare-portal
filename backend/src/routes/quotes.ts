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
  counties: string[];
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
    original_plan_name?: string;
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
    effective_date?: string;
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
    effective_date?: string;
}

interface ContactQuoteInfo {
    zip_code: string;
    birth_date: string;
    age: number;
    tobacco_user: number;
    gender: string;
    email: string;
    first_name: string;
    last_name: string;
    current_carrier: string;
    phone_number: string;
    plan_type: string;
}

// Add new interface for org info response
interface OrgRedirectInfo {
    redirect_url: string | null;
    agent_name: string;
}

function calculateAgeOnFirstOfNextMonth(birthDate: string, currentDate: string): number {
    const birth = new Date(birthDate);
    const current = new Date(currentDate);
    
    // Get first day of next month
    const nextMonth = new Date(current.getFullYear(), current.getMonth() + 1, 1);
    
    // Calculate age based on year difference
    let age = nextMonth.getFullYear() - birth.getFullYear();
    
    // Adjust age if birthday hasn't occurred yet in the target month
    if (
        nextMonth.getMonth() < birth.getMonth() || 
        (nextMonth.getMonth() === birth.getMonth() && 1 < birth.getDate())
    ) {
        age--;
    }
    
    return age;
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
            const result = await mainDb.fetchOne<{ slug: string, org_settings: string, name: string, logo_data: string, phone: string, redirect_url: string, org_signature: boolean, signature: string | null }>(
                'SELECT slug, org_settings, name, logo_data, phone, redirect_url, org_signature, signature FROM organizations WHERE id = ?',
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

            // Get forceOrgSenderDetails from org settings with backwards compatibility
            const forceOrgSenderDetails = orgSettings?.forceOrgSenderDetails !== undefined 
                ? orgSettings.forceOrgSenderDetails 
                : Boolean(result.org_signature);

            // Get signature from either orgSettings, database column, or fall back to org name
            let signature = orgSettings?.signature || result.signature || result.name;

            // First try to get the assigned agent from the contact
            const contactQuery = 'SELECT zip_code, birth_date, tobacco_user, gender, email, first_name, last_name, current_carrier, phone_number, plan_type, agent_id FROM contacts WHERE id = ?';
            const contactParams = [decoded.contactId];
            
            logger.info(`Executing contact query: ${contactQuery} with params: [${contactParams}]`);
            
            const contact = await orgDb.fetchOne<ContactQuoteInfo & { agent_id: number | null }>(contactQuery, contactParams);

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

            // Get agent info - first try assigned agent, then default agent, then fall back to first user
            let agent = null;
            
            // Try assigned agent first
            if (contact.agent_id) {
                agent = await mainDb.fetchOne<{ first_name: string, last_name: string, email: string, phone: string, booking_link: string, use_org_sender_details: number, signature: string }>(
                    'SELECT first_name, last_name, email, phone, booking_link, use_org_sender_details, signature FROM users WHERE id = ?',
                    [contact.agent_id]
                );
            }
            
            // If no assigned agent, try default agent from org settings
            if (!agent) {
                const defaultAgentResult = await mainDb.fetchOne<{ default_agent_id: number }>(
                    'SELECT default_agent_id FROM organizations WHERE id = ?',
                    [decoded.orgId]
                );
                
                if (defaultAgentResult?.default_agent_id) {
                    agent = await mainDb.fetchOne<{ first_name: string, last_name: string, email: string, phone: string, booking_link: string, use_org_sender_details: number, signature: string }>(
                        'SELECT first_name, last_name, email, phone, booking_link, use_org_sender_details, signature FROM users WHERE id = ?',
                        [defaultAgentResult.default_agent_id]
                    );
                    
                    // If we found the default agent, update the contact's agent_id
                    if (agent) {
                        try {
                            await orgDb.execute(
                                'UPDATE contacts SET agent_id = ? WHERE id = ?',
                                [defaultAgentResult.default_agent_id, decoded.contactId]
                            );
                            logger.info(`Updated contact ${decoded.contactId} with default agent ${defaultAgentResult.default_agent_id}`);
                        } catch (e) {
                            logger.error(`Error updating contact's agent_id: ${e}`);
                        }
                    }
                }
            }
            
            // If still no agent, fall back to first active user
            if (!agent) {
                agent = await mainDb.fetchOne<{ first_name: string, last_name: string, email: string, phone: string, booking_link: string, use_org_sender_details: number, signature: string }>(
                    'SELECT first_name, last_name, email, phone, booking_link, use_org_sender_details, signature FROM users WHERE organization_id = ? AND is_active = 1 ORDER BY id ASC LIMIT 1',
                    [decoded.orgId]
                );
            }

            if (!agent) {
                logger.error(`No users found for organization: ${decoded.orgId}`);
                set.status = 404;
                return {
                    success: false,
                    error: 'No users found for organization'
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
                orgName: result.name,
                orgLogo: result.logo_data || null,
                orgPhone: orgSettings?.phone || result.phone || null,
                orgRedirectUrl: orgSettings?.redirectUrl || result.redirect_url || null,
                orgSignature: Boolean(result.org_signature) || false,
                orgSignatureText: signature,
                carrierContracts: carrierContracts || null,
                forceOrgSenderDetails: forceOrgSenderDetails,
                agent: {
                    firstName: agent.first_name,
                    lastName: agent.last_name,
                    email: agent.email,
                    phone: agent.phone,
                    bookingLink: agent.booking_link || '',
                    useOrgSenderDetails: Boolean(agent.use_org_sender_details),
                    signature: agent.signature || ''
                },
                contact: {
                    id: decoded.contactId,
                    zipCode: contact.zip_code,
                    state: contactState,
                    dateOfBirth: contact.birth_date,
                    age: calculateAgeOnFirstOfNextMonth(contact.birth_date, new Date().toISOString()),
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
            logger.info(`Output: ${JSON.stringify(output, null, 2)}`);
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
    .post('/api/quotes', async ({ body, set }: { body: QuoteRequestBody, set: any }) => {
        try {
            // Check if state uses special plans
            const stateSpecificPlans: Record<string, string[]> = {
                'MN': ['MN_BASIC', 'MN_EXTB'],
                'WI': ['WI_BASE', 'WI_HDED'],
                'MA': ['MA_CORE', 'MA_SUPP1']
            };

            // Determine which plans to request based on state
            const plansToRequest = stateSpecificPlans[body.state] || ['G', 'N'];

            // Format request body
            const requestBody: QuoteRequest = {
                zip_code: body.zip_code,
                state: body.state,
                age: Math.max(65, Math.min(110, Number(body.age))),
                tobacco: body.tobacco === 'true' || body.tobacco === true,
                gender: body.gender,
                plans: plansToRequest,
                carriers: 'supported',
                county: body.county
            };

            if (body.effective_date) {
                requestBody.effective_date = body.effective_date;
            }

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

            logger.info(`Making request to quote engine with config: ${JSON.stringify({
                url: requestConfig.url,
                method: requestConfig.method,
                headers: {
                    ...requestConfig.headers,
                    'X-API-Key': '[REDACTED]'
                }
            }, null, 2)}`);

            // Make request to quote engine API
            const response = await axios(requestConfig);
            
            logger.info(`Quote engine response status: ${response.status}`);
            logger.info(`Quote engine response headers: ${JSON.stringify(response.headers, null, 2)}`);
            logger.info(`Quote engine response data length: ${JSON.stringify(response.data).length} characters`);
            // logger.info(`Quote engine response data preview: ${JSON.stringify(response.data).substring(0, 500)}...`); // Log more for debugging complex structures
            
            if (!response.data || !Array.isArray(response.data)) {
                logger.error('Quote engine returned empty or invalid response data');
                set.status = 500;
                return { error: 'No quote data received from engine or data is not an array' };
            }

            // Define state-specific plan mappings to their standard equivalents
            const planAliasMapping: Record<string, { standard: string, original: string }> = {
                'MN_BASIC': { standard: 'N', original: 'MN_BASIC' },
                'MN_EXTB': { standard: 'G', original: 'MN_EXTB' },
                'WI_BASE': { standard: 'N', original: 'WI_BASE' },
                'WI_HDED': { standard: 'N', original: 'WI_HDED' }, // Assuming WI_HDED maps to N, adjust if needed
                'MA_CORE': { standard: 'N', original: 'MA_CORE' },
                'MA_SUPP1': { standard: 'G', original: 'MA_SUPP1' }
            };

            const processedData = response.data.map((companyQuote: QuoteResponse) => {
                if (companyQuote.quotes && Array.isArray(companyQuote.quotes)) {
                    companyQuote.quotes = companyQuote.quotes.map((quote: Quote) => {
                        const mapping = planAliasMapping[quote.plan.toUpperCase()];
                        if (mapping) {
                            return {
                                ...quote,
                                original_plan_name: mapping.original,
                                plan: mapping.standard
                            };
                        }
                        // If not a state-specific plan that needs aliasing, ensure original_plan_name is null or undefined
                        // if the quote engine might send it for G/N plans directly.
                        // For now, assume if it's not in mapping, it doesn't get an original_plan_name unless already present.
                        return {
                            ...quote,
                            original_plan_name: quote.original_plan_name || undefined // or null if preferred by Elm decoder
                        };
                    });
                }
                return companyQuote;
            });

            logger.info(`Processed quote data preview: ${JSON.stringify(processedData).substring(0, 500)}...`);

            // Return quotes from response data
            set.status = 200; // Explicitly set 200 status
            return processedData;
        } catch (error: any) {
            logger.error(`Error fetching quotes: ${error}`);
            if (axios.isAxiosError(error)) {
                logger.error(`Axios error details: ${JSON.stringify({
                    status: error.response?.status,
                    statusText: error.response?.statusText,
                    headers: error.response?.headers,
                    data: error.response?.data
                }, null, 2)}`);
            }
            set.status = error.response?.status || 500;
            throw new Error(String(error));
        }
    })
    .get('/api/contact-request/org-info/:orgId', async ({ params }) => {
        try {
            const db = new Database();
            
            // Get organization info including redirect URL and signature
            const orgResult = await db.fetchOne<{ redirect_url: string | null, name: string, signature: string | null, org_settings: string | null }>(
                'SELECT redirect_url, name, signature, org_settings FROM organizations WHERE id = ?',
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

            // Parse org settings to try to get signature from there first
            let orgSettings = {};
            try {
                if (orgResult.org_settings) {
                    orgSettings = JSON.parse(orgResult.org_settings);
                }
            } catch (e) {
                logger.warn(`Error parsing org settings: ${e}`);
            }

            // Prioritize signature in this order: orgSettings, signature column, org name
            const signature = (orgSettings as any)?.signature || orgResult.signature || orgResult.name;

            return {
                success: true,
                redirect_url: orgResult.redirect_url,
                agent_name: `${defaultAgent.first_name} ${defaultAgent.last_name}`,
                org_name: orgResult.name,
                signature: signature
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