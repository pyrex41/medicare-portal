import { Elysia, t } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';

// Types for request body
interface WaitlistEntry {
    name: string;
    email: string;
    phone: string;
    numAgents: string;
    bookSize: string;
}

// Validation functions
function validateEmail(email: string): boolean {
    const emailRegex = /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/;
    return emailRegex.test(email.trim());
}

function standardizePhoneNumber(phone: string): { isValid: boolean; standardized: string } {
    const digits = phone.replace(/\D/g, '').slice(0, 10);
    return {
        isValid: digits.length === 10,
        standardized: digits
    };
}

export const createWaitlistRoutes = () => {
    const app = new Elysia({ prefix: '/api/waitlist' });
    const db = new Database(); // Create a new instance of the main database

    app.post('/', async ({ body }: { body: WaitlistEntry }) => {
        try {
            logger.info(`Received waitlist submission: ${JSON.stringify(body)}`);
            const { name, email, phone, numAgents, bookSize } = body;

            // Validate inputs
            if (!name || typeof name !== 'string' || name.trim().length === 0) {
                logger.warn(`Invalid name: ${name}`);
                return { success: false, message: 'Name is required' };
            }

            if (!email || typeof email !== 'string' || !validateEmail(email)) {
                logger.warn(`Invalid email: ${email}`);
                return { success: false, message: 'Invalid email format' };
            }

            const phoneResult = standardizePhoneNumber(phone);
            if (!phoneResult.isValid) {
                logger.warn(`Invalid phone: ${phone}`);
                return { success: false, message: 'Phone number must be 10 digits' };
            }

            if (!numAgents || isNaN(Number(numAgents)) || Number(numAgents) <= 0) {
                logger.warn(`Invalid numAgents: ${numAgents}`);
                return { success: false, message: 'Number of agents must be a positive number' };
            }

            if (!bookSize || isNaN(Number(bookSize)) || Number(bookSize) <= 0) {
                logger.warn(`Invalid bookSize: ${bookSize}`);
                return { success: false, message: 'Book size must be a positive number' };
            }

            // Check if email already exists
            logger.info(`Checking for existing email: ${email}`);
            const existing = await db.execute(
                'SELECT id FROM waitlist WHERE email = ?',
                [email]
            );

            if (existing.rows.length > 0) {
                logger.warn(`Email already registered: ${email}`);
                return { success: false, message: 'Email already registered' };
            }

            // Insert new waitlist entry
            logger.info('Inserting new waitlist entry');
            const insertResult = await db.execute(`
                INSERT INTO waitlist (name, email, phone, num_agents, book_size)
                VALUES (?, ?, ?, ?, ?)
            `, [
                name.trim(),
                email.trim(),
                phoneResult.standardized,
                Number(numAgents),
                Number(bookSize)
            ]);

            logger.info(`Insert result: ${JSON.stringify(insertResult)}`);

            return { 
                success: true, 
                message: 'Successfully joined waitlist' 
            };
        } catch (error) {
            logger.error(`Waitlist error: ${error instanceof Error ? error.message : String(error)}`);
            return { 
                success: false, 
                message: 'Internal server error' 
            };
        }
    });

    return app;
}; 