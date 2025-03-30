import { Elysia, t } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import { cors } from '@elysiajs/cors';

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
    const app = new Elysia({ prefix: '/api/waitlist' })
    .use(cors({
        origin: process.env.NODE_ENV === 'development' 
            ? 'http://localhost:5173' 
            : false,
        methods: ['POST', 'OPTIONS'],
        allowedHeaders: ['Content-Type'],
        credentials: false  // Don't require credentials for waitlist
    }));
    
    const db = new Database(); // Create a new instance of the main database

    app.post('/', async ({ body, set }: { body: WaitlistEntry, set: any }) => {
        try {
            logger.info(`Received waitlist submission: ${JSON.stringify(body)}`);
            const { name, email, phone, numAgents, bookSize } = body;

            // Validate inputs
            if (!name || typeof name !== 'string' || name.trim().length === 0) {
                logger.warn(`Invalid name: ${name}`);
                set.status = 400;
                return { success: false, message: 'Name is required' };
            }

            if (!email || typeof email !== 'string' || !validateEmail(email)) {
                logger.warn(`Invalid email: ${email}`);
                set.status = 400;
                return { success: false, message: 'Invalid email format' };
            }

            const phoneResult = standardizePhoneNumber(phone);
            if (!phoneResult.isValid) {
                logger.warn(`Invalid phone: ${phone}`);
                set.status = 400;
                return { success: false, message: 'Phone number must be 10 digits' };
            }

            if (!numAgents || isNaN(Number(numAgents)) || Number(numAgents) <= 0) {
                logger.warn(`Invalid numAgents: ${numAgents}`);
                set.status = 400;
                return { success: false, message: 'Number of agents must be a positive number' };
            }

            if (!bookSize || isNaN(Number(bookSize)) || Number(bookSize) <= 0) {
                logger.warn(`Invalid bookSize: ${bookSize}`);
                set.status = 400;
                return { success: false, message: 'Book size must be a positive number' };
            }

            // Check if email exists first to determine if this is an update
            const existing = await db.execute(
                'SELECT id FROM waitlist WHERE email = ?',
                [email.trim()]
            );
            const isUpdate = existing.rows && existing.rows.length > 0;

            // Insert or update waitlist entry
            logger.info('Upserting waitlist entry');
            const upsertResult = await db.execute(`
                INSERT INTO waitlist (name, email, phone, num_agents, book_size)
                VALUES (?, ?, ?, ?, ?)
                ON CONFLICT(email) DO UPDATE SET
                    name = EXCLUDED.name,
                    phone = EXCLUDED.phone,
                    num_agents = EXCLUDED.num_agents,
                    book_size = EXCLUDED.book_size,
                    updated_at = CURRENT_TIMESTAMP
            `, [
                name.trim(),
                email.trim(),
                phoneResult.standardized,
                Number(numAgents),
                Number(bookSize)
            ]);

            logger.info(`Upsert result: ${JSON.stringify(upsertResult)}`);

            // Explicitly set success status
            set.status = 200;
            
            return { 
                success: true, 
                message: isUpdate ? 'Successfully updated your waitlist entry' : 'Successfully joined waitlist'
            };
        } catch (error) {
            logger.error(`Waitlist error: ${error instanceof Error ? error.message : String(error)}`);
            set.status = 500;
            return { 
                success: false, 
                message: 'Internal server error' 
            };
        }
    });

    return app;
}; 