import { Elysia, t } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import { cors } from '@elysiajs/cors';
import sgMail from '@sendgrid/mail';

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

    // Initialize SendGrid
    if (!process.env.SENDGRID_API_KEY) {
        throw new Error('Missing SENDGRID_API_KEY environment variable');
    }
    sgMail.setApiKey(process.env.SENDGRID_API_KEY);

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

            // Send confirmation email
            try {
                await sgMail.send({
                    to: email.trim(),
                    from: {
                        email: process.env.HELLO_EMAIL || 'information@medicaremax.ai',
                        name: 'Medicare Max'
                    },
                    replyTo: process.env.HELLO_EMAIL || 'information@medicaremax.ai',
                    subject: "You're In. Time to Max What's Yours.",
                    text: `Hey ${name.trim()},

You made it on the Medicare Max waitlist—and not a moment too soon. 

You're doing the work, building the book, and watching revenue slip through the cracks. That stops here.

It's time to keep more, earn smarter, and finally get what's yours.

Early access is on the way. Stay ready.

Have questions? Just reply to this email and we'll get back to you.

Cheers,
THe Medicare Max Team`,
                    html: `
                        <div style="font-family: Arial, sans-serif; max-width: 600px; margin: 0 auto; background-color: #ffffff; padding: 40px 20px;">
                            <div style="text-align: center; margin-bottom: 30px;">
                                <a href="https://medicaremax.ai" style="text-decoration: none; display: inline-block;">
                                    <img src="https://medicaremax.ai/images/medicare-max-logo.png" alt="Medicare Max Logo" style="height: 40px; margin: 0 auto;">
                                </a>
                            </div>
                            <div style="background-color: #f0f9ff; border-radius: 12px; padding: 30px; margin: 20px 0;">
                                <h2 style="color: #03045E; font-size: 24px; margin-bottom: 20px;">Hey ${name.trim()},</h2>
                                <p style="color: #1a1f5f; font-size: 16px; line-height: 1.6; margin-bottom: 20px;">
                                    You made it on the Medicare Max waitlist—and not a moment too soon.
                                </p>
                                <p style="color: #1a1f5f; font-size: 16px; line-height: 1.6; margin-bottom: 20px;">
                                    You're doing the work, building the book, and watching revenue slip through the cracks. That stops here.
                                </p>
                                <div style="text-align: center;">
                                    <div style="background-color: #03045E; color: white; padding: 15px 25px; border-radius: 8px; display: inline-block; margin: 20px 0;">
                                        <span style="font-size: 20px; vertical-align: middle; margin-right: 8px;">🎉</span>
                                        <span style="vertical-align: middle;">Time to max what's yours</span>
                                    </div>
                                </div>
                                <p style="color: #1a1f5f; font-size: 16px; line-height: 1.6; margin-bottom: 20px;">
                                    It's time to keep more, earn smarter, and finally get what's yours.
                                </p>
                                <p style="color: #1a1f5f; font-size: 16px; line-height: 1.6; margin-bottom: 20px;">
                                    Early access is on the way. Stay ready.
                                </p>
                                <p style="color: #1a1f5f; font-size: 16px; line-height: 1.6; margin-top: 20px; font-style: italic;">
                                    Have questions? Just reply to this email and we'll get back to you.
                                </p>
                            </div>
                            <div style="margin-top: 30px; padding-top: 20px; border-top: 1px solid #e5e7eb; text-align: center;">
                                <p style="color: #6b7280; font-size: 14px;">
                                    Cheers,<br>
                                    The Medicare Max Team
                                </p>
                            </div>
                        </div>
                    `
                });
                logger.info(`Confirmation email sent to ${email}`);
            } catch (emailError) {
                // Log the error but don't fail the request
                logger.error(`Failed to send confirmation email: ${emailError instanceof Error ? emailError.message : String(emailError)}`);
            }

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