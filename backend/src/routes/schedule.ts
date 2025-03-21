import { Elysia } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import { decodeQuoteId } from '../utils/quoteId';

export const scheduleRoutes = (app: Elysia) => {
    app.get('/api/schedule/info/:quoteId', async ({ params, set }) => {
        try {
            const { quoteId } = params;
            
            // Decode quote ID to get org ID and contact ID
            const decoded = decodeQuoteId(quoteId);
            if (!decoded) {
                set.status = 400;
                return {
                    success: false,
                    error: 'Invalid quote ID'
                };
            }

            const { orgId, contactId } = decoded;
            
            // Get main database instance
            const mainDb = new Database();
            
            // Get organization info
            const orgResult = await mainDb.fetchOne<{ name: string, logo_data: string | null }>(
                'SELECT name, logo_data FROM organizations WHERE id = ?',
                [orgId]
            );

            if (!orgResult) {
                set.status = 404;
                return {
                    success: false,
                    error: 'Organization not found'
                };
            }

            // Get org-specific database
            const orgDb = await Database.getOrInitOrgDb(orgId.toString());

            // Get contact information and agent_id in a single query
            const contact = await orgDb.fetchOne<{ 
                first_name: string, 
                last_name: string, 
                email: string, 
                phone_number: string, 
                agent_id: number | null 
            }>(
                'SELECT first_name, last_name, email, phone_number, agent_id FROM contacts WHERE id = ?',
                [contactId]
            );

            if (!contact) {
                set.status = 404;
                return {
                    success: false,
                    error: 'Contact not found'
                };
            }

            // Get agent info - first try assigned agent, then fall back to first user
            const agent = contact.agent_id 
                ? await mainDb.fetchOne<{ first_name: string, last_name: string, phone: string }>(
                    'SELECT first_name, last_name, phone FROM users WHERE id = ? AND is_active = 1',
                    [contact.agent_id]
                  )
                : await mainDb.fetchOne<{ first_name: string, last_name: string, phone: string }>(
                    'SELECT first_name, last_name, phone FROM users WHERE organization_id = ? AND is_active = 1 ORDER BY id ASC LIMIT 1',
                    [orgId]
                  );

            if (!agent) {
                set.status = 404;
                return {
                    success: false,
                    error: 'No active agents found for organization'
                };
            }

            // Return all information in a single response
            return {
                success: true,
                contact: {
                    firstName: contact.first_name,
                    lastName: contact.last_name,
                    email: contact.email,
                    phoneNumber: contact.phone_number
                },
                organization: {
                    name: orgResult.name,
                    logo: orgResult.logo_data
                },
                agent: {
                    name: `${agent.first_name} ${agent.last_name}`,
                    firstName: agent.first_name,
                    phone: agent.phone || ""
                }
            };

        } catch (error) {
            logger.error(`Error getting schedule info: ${error}`);
            set.status = 500;
            return {
                success: false,
                error: 'Internal server error'
            };
        }
    });

    // Keep the original endpoint for backward compatibility
    app.get('/api/schedule/org-info/:quoteId', async ({ params, set }) => {
        try {
            const { quoteId } = params;
            
            // Decode quote ID to get org ID and contact ID
            const decoded = decodeQuoteId(quoteId);
            if (!decoded) {
                set.status = 400;
                return {
                    success: false,
                    error: 'Invalid quote ID'
                };
            }

            const { orgId, contactId } = decoded;
            
            // Get main database instance
            const mainDb = new Database();
            
            // Get organization info
            const orgResult = await mainDb.fetchOne<{ name: string, logo_data: string | null }>(
                'SELECT name, logo_data FROM organizations WHERE id = ?',
                [orgId]
            );

            if (!orgResult) {
                set.status = 404;
                return {
                    success: false,
                    error: 'Organization not found'
                };
            }

            // Get org-specific database
            const orgDb = await Database.getOrInitOrgDb(orgId.toString());

            // Try to get the assigned agent from the contact
            const contact = await orgDb.fetchOne<{ agent_id: number | null }>(
                'SELECT agent_id FROM contacts WHERE id = ?',
                [contactId]
            );

            // Get agent info - first try assigned agent, then fall back to first user
            const agent = contact?.agent_id 
                ? await mainDb.fetchOne<{ first_name: string, last_name: string, phone: string }>(
                    'SELECT first_name, last_name, phone FROM users WHERE id = ? AND is_active = 1',
                    [contact.agent_id]
                  )
                : await mainDb.fetchOne<{ first_name: string, last_name: string, phone: string }>(
                    'SELECT first_name, last_name, phone FROM users WHERE organization_id = ? AND is_active = 1 ORDER BY id ASC LIMIT 1',
                    [orgId]
                  );

            if (!agent) {
                set.status = 404;
                return {
                    success: false,
                    error: 'No active agents found for organization'
                };
            }

            return {
                success: true,
                orgName: orgResult.name,
                orgLogo: orgResult.logo_data,
                agentName: `${agent.first_name} ${agent.last_name}`,
                agentPhone: agent.phone || ""
            };

        } catch (error) {
            logger.error(`Error getting schedule org info: ${error}`);
            set.status = 500;
            return {
                success: false,
                error: 'Internal server error'
            };
        }
    });

    return app;
}; 