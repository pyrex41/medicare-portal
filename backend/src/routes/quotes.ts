import { Elysia } from 'elysia';
import axios from 'axios';
import { config } from '../config';
import { logger } from '../logger';
import { Database } from '../database';
import { generateQuoteId, decodeQuoteId } from '../utils/quoteId';
import { getUserFromSession } from '../services/auth';

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
            const orgDb = await Database.getOrgDb(user.organization_id.toString());
            
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
    .get('/api/quotes/decode/:quoteId', async ({ params }) => {
        try {
            const decoded = decodeQuoteId(params.quoteId);
            if (!decoded) {
                throw new Error('Invalid quote ID');
            }

            // Get org-specific database
            const orgDb = await Database.getOrgDb(decoded.orgId.toString());
            
            // Fetch contact details
            const contact = await orgDb.fetchOne<ContactQuoteInfo>(
                'SELECT zip_code, birth_date, tobacco_user, gender, email, first_name, last_name, current_carrier FROM contacts WHERE id = ?',
                [decoded.contactId]
            );

            if (!contact) {
                throw new Error('Contact not found');
            }

            return {
                success: true,
                contact: {
                    zipCode: contact.zip_code,
                    dateOfBirth: contact.birth_date,
                    tobacco: Boolean(contact.tobacco_user),
                    gender: contact.gender,
                    email: contact.email,
                    firstName: contact.first_name,
                    lastName: contact.last_name,
                    currentCarrier: contact.current_carrier
                }
            };
        } catch (e) {
            logger.error(`Error decoding quote ID: ${e}`);
            throw new Error(String(e));
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

            // Log complete request details
            logger.info(`Full request details: ${JSON.stringify({
                url: requestConfig.url,
                method: requestConfig.method,
                headers: requestConfig.headers,
                data: requestConfig.data
            }, null, 2)}`);
            
            const response = await axios.request<QuoteResponse[]>(requestConfig);
            
            // Log response summary without full data
            const quotes = response.data;
            const planTypes = new Set(quotes.flatMap(q => q.quotes.map(quote => quote.plan)));
            logger.info(`Quote response summary: ${JSON.stringify({
                total_quotes: quotes.length,
                plan_types: Array.from(planTypes),
                carriers: quotes.map(q => q.company_name)
            }, null, 2)}`);
            
            return response.data;
        } catch (error) {
            console.error('Error fetching quotes:', error);
            const message = error instanceof Error ? error.message : 'Unknown error';
            throw new Error(`Failed to fetch quotes: ${message}`);
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
            const orgDb = await Database.getOrgDb(params.orgId);

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
            const orgDb = await Database.getOrgDb(orgId.toString());

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
    });

    return app;
}; 