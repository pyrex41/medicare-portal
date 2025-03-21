import { Elysia } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import { decodeQuoteId } from '../utils/quoteId';
import { getUserFromSession } from '../services/auth';

interface EligibilityAnswers {
    quote_id?: string;
    contact_id?: string;
    answers: Record<string, {
        question_text: string;
        question_type: string;
        answer: boolean | string | null;
    }>;
}

export const eligibilityRoutes = (app: Elysia) => {
    app
        // Get organization details
        .get('/api/org/:orgId/details', async ({ params }) => {
            try {
                const orgId = params.orgId;
                if (!orgId) {
                    return {
                        success: false,
                        error: 'Missing organization ID'
                    };
                }

                // Get organization details from main database
                const mainDb = new Database();
                const orgDetails = await mainDb.fetchOne<{ name: string, logo_data: string | null }>(
                    'SELECT name, logo_data FROM organizations WHERE id = ?',
                    [orgId]
                );

                if (!orgDetails) {
                    return {
                        success: false,
                        error: 'Organization not found'
                    };
                }

                return {
                    name: orgDetails.name,
                    logo_data: orgDetails.logo_data
                };
            } catch (error) {
                logger.error(`Error getting organization details: ${error}`);
                throw new Error('Failed to get organization details');
            }
        })

        // Get existing eligibility answers for a contact
        .get('/api/org/:orgId/eligibility-answers/:contactId', async ({ params }) => {
            try {
                const { orgId, contactId } = params;
                if (!orgId || !contactId) {
                    return {
                        success: false,
                        error: 'Missing required parameters'
                    };
                }

                // Get org-specific database
                const orgDb = await Database.getOrInitOrgDb(orgId);

                // Get the most recent answers for this contact
                const result = await orgDb.fetchOne<{ answers: string }>(
                    'SELECT answers FROM eligibility_answers WHERE contact_id = ? ORDER BY created_at DESC LIMIT 1',
                    [contactId]
                );

                if (!result) {
                    return {
                        success: false,
                        error: 'No answers found'
                    };
                }

                // Get organization details from main database
                const mainDb = new Database();
                const orgDetails = await mainDb.fetchOne<{ name: string, logo_data: string | null }>(
                    'SELECT name, logo_data FROM organizations WHERE id = ?',
                    [orgId]
                );

                return {
                    answers: JSON.parse(result.answers),
                    orgName: orgDetails?.name || 'Medicare Max',
                    orgLogo: orgDetails?.logo_data || null
                };
            } catch (error) {
                logger.error(`Error getting eligibility answers: ${error}`);
                throw new Error('Failed to get eligibility answers');
            }
        })

        // Create a temporary contact for collecting eligibility answers
        .post('/api/org/:orgId/temp-contact', async ({ params }) => {
            try {
                const orgId = params.orgId;
                if (!orgId) {
                    return {
                        success: false,
                        error: 'Missing organization ID'
                    };
                }

                // Get org-specific database
                const orgDb = await Database.getOrInitOrgDb(orgId);
                
                // Create a minimal contact record for just eligibility assessment
                const result = await orgDb.execute(
                    `INSERT INTO contacts 
                    (first_name, last_name, email, current_carrier, plan_type, effective_date, birth_date, tobacco_user, gender, state, zip_code, phone_number, status) 
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
                    [
                        'Temporary', 
                        'Contact', 
                        `temp-${Date.now()}@example.com`, 
                        '', 
                        'G', 
                        '', 
                        '1950-01-01', 
                        0, 
                        'M', 
                        '', 
                        '00000', 
                        '', 
                        'temp'
                    ]
                );

                // Get the inserted ID from the result
                const contactId = result.lastInsertRowid?.toString();
                
                if (!contactId) {
                    throw new Error('Failed to create temporary contact');
                }

                // Add a record to contact_events to track this
                await orgDb.execute(
                    `INSERT INTO contact_events 
                    (contact_id, event_type, metadata) 
                    VALUES (?, ?, ?)`,
                    [
                        contactId,
                        'temp_contact_created',
                        JSON.stringify({ source: 'eligibility_assessment' })
                    ]
                );

                return contactId;
            } catch (error) {
                logger.error(`Error creating temporary contact: ${error}`);
                throw new Error('Failed to create temporary contact');
            }
        })

        // Save eligibility answers
        .post('/api/org/:orgId/eligibility-answers', async ({ params, body }) => {
            try {
                const orgId = params.orgId;
                if (!orgId) {
                    return {
                        success: false,
                        error: 'Missing organization ID'
                    };
                }

                const data = body as EligibilityAnswers;
                
                // Get org-specific database
                const orgDb = await Database.getOrInitOrgDb(orgId);
                
                let contactId: string | null = null;
                
                // If quote_id is provided, get the contact_id from it
                if (data.quote_id) {
                    try {
                        const decodedQuoteId = decodeQuoteId(data.quote_id);
                        if (decodedQuoteId) {
                            contactId = decodedQuoteId.contactId.toString();
                            
                            // Verify the org ID matches
                            if (decodedQuoteId.orgId.toString() !== orgId) {
                                return {
                                    success: false,
                                    error: 'Invalid quote ID - organization mismatch'
                                };
                            }
                        }
                    } catch (error) {
                        logger.error(`Error decoding quote ID: ${error}`);
                        return {
                            success: false,
                            error: 'Invalid quote ID'
                        };
                    }
                } else if (data.contact_id) {
                    // If contact_id is provided directly, use it
                    contactId = data.contact_id;
                }
                
                if (!contactId) {
                    return {
                        success: false,
                        error: 'Missing contact ID'
                    };
                }
                
                // Verify contact exists in this org
                const contact = await orgDb.fetchOne(
                    'SELECT id FROM contacts WHERE id = ?',
                    [contactId]
                );
                
                if (!contact) {
                    return {
                        success: false,
                        error: 'Contact not found'
                    };
                }

                // Get organization details from main database
                const mainDb = new Database();
                const orgDetails = await mainDb.fetchOne<{ name: string, logo_data: string | null }>(
                    'SELECT name, logo_data FROM organizations WHERE id = ?',
                    [orgId]
                );
                
                // Store the eligibility answers
                const answersJson = JSON.stringify(data.answers);
                
                // Check if we already have answers for this contact
                const existingAnswers = await orgDb.fetchOne(
                    'SELECT id FROM eligibility_answers WHERE contact_id = ?',
                    [contactId]
                );
                
                if (existingAnswers) {
                    // Update existing answers
                    await orgDb.execute(
                        'UPDATE eligibility_answers SET answers = ?, created_at = CURRENT_TIMESTAMP WHERE contact_id = ?',
                        [answersJson, contactId]
                    );
                } else {
                    // Insert new answers
                    await orgDb.execute(
                        'INSERT INTO eligibility_answers (contact_id, answers) VALUES (?, ?)',
                        [contactId, answersJson]
                    );
                }
                
                // Extract analytics data from enhanced answers
                const analyticsData = {
                    answers_count: Object.keys(data.answers).length,
                    source: data.quote_id ? 'quote' : 'direct',
                    main_questions_yes_count: Object.values(data.answers).filter(a => 
                        a.question_type === 'main' && a.answer === true
                    ).length,
                    has_medical_conditions: Object.values(data.answers).some(a => 
                        a.question_type === 'main' && a.answer === true
                    )
                };
                
                // Add a record to contact_events
                await orgDb.execute(
                    `INSERT INTO contact_events 
                    (contact_id, event_type, metadata) 
                    VALUES (?, ?, ?)`,
                    [
                        contactId,
                        'eligibility_answered',
                        JSON.stringify(analyticsData)
                    ]
                );
                
                return {
                    contactId,
                    orgName: orgDetails?.name || 'Medicare Max',
                    orgLogo: orgDetails?.logo_data || null
                };
            } catch (error) {
                logger.error(`Error saving eligibility answers: ${error}`);
                throw new Error('Failed to save eligibility answers');
            }
        });

    return app;
}; 