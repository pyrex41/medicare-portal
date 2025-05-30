import { Elysia } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import { config } from '../config';
import { getUserFromSession } from '../services/auth';
import { requireAuth } from '../middleware/auth';
import { checkPaymentStatus } from '../services/stripe';
import crypto from 'crypto';
import { cookie } from '@elysiajs/cookie';  

interface SubscriptionStatus {
  isActive: boolean;
  tier: string;
  currentPeriodEnd?: number;
  cancelAtPeriodEnd?: boolean;
  paymentStatus: string;
}

export const createStripeRoutes = (app: Elysia) => {
  app.use(requireAuth)
    .get('/api/stripe/subscription-status', async ({ user, set }) => {
      try {
        if (!user?.organization_id) {
          logger.info('No organization ID in request');
          set.status = 400;
          return { 
            success: false,
            error: 'No organization ID found' 
          };
        }
        
        const db = new Database();
        const status = await checkPaymentStatus(db, user.organization_id);
        logger.info(`Returning subscription status: ${JSON.stringify(status)}`);
        
        set.status = 200;
        return {
          success: true,
          data: status as SubscriptionStatus
        };
      } catch (error) {
        logger.error(`Error in subscription status route: ${error}`);
        set.status = 500;
        return { 
          success: false,
          error: 'Failed to fetch subscription status',
          details: error instanceof Error ? error.message : 'Unknown error'
        };
      }
    });
    
  // Create separate instance without auth middleware for the payment-complete endpoint
  app.use(cookie())
    .post('/api/stripe/payment-complete', async ({ body, set, setCookie }) => {
      try {
        const { 
          email: encodedEmail, 
          firstName, 
          lastName,
          stripeCustomerId,
          stripeSubscriptionId,
          stripeUsageItemId
        } = body as { 
          email: string;
          firstName: string;
          lastName: string;
          stripeCustomerId?: string;
          stripeSubscriptionId?: string;
          stripeUsageItemId?: string;
        };

        // Decode the email in case it contains URL-encoded characters
        const email = decodeURIComponent(encodedEmail);

        logger.info(`Processing payment completion for email: ${email}`);
        logger.info(`Payment data: StripeCustomer=${stripeCustomerId ? (typeof stripeCustomerId === 'string' ? stripeCustomerId.substring(0, 10) + '...' : 'object') : 'missing'}, StripeSubscription=${stripeSubscriptionId ? (stripeSubscriptionId && typeof stripeSubscriptionId === 'string' ? stripeSubscriptionId.substring(0, 10) + '...' : 'object') : 'missing'}, UsageItem=${stripeUsageItemId ? (typeof stripeUsageItemId === 'string' ? stripeUsageItemId.substring(0, 10) + '...' : 'object') : 'missing'}`);

        const db = new Database();
        const client = db.getClient();

        // First get the user and organization
        logger.info(`Looking up user record for email: ${email}`);
        const userResult = await client.execute({
          sql: 'SELECT users.id, users.organization_id FROM users WHERE email = ? AND is_active = 1',
          args: [email]
        });

        logger.info(`User query returned ${userResult.rows.length} rows`);
        
        if (userResult.rows.length === 0) {
          logger.error(`No active user found for email: ${email}`);
          // Let's try a case-insensitive search as a fallback
          logger.info(`Trying case-insensitive search for email: ${email}`);
          const caseInsensitiveResult = await client.execute({
            sql: 'SELECT users.id, users.organization_id FROM users WHERE LOWER(email) = LOWER(?) AND is_active = 1',
            args: [email]
          });
          
          if (caseInsensitiveResult.rows.length === 0) {
            logger.error(`Still no user found with case-insensitive search for email: ${email}`);
            set.status = 404;
            return { success: false, error: 'User not found' };
          } else {
            logger.info(`Found user with case-insensitive match: ${caseInsensitiveResult.rows[0].id}`);
            // Continue with the found user
            userResult.rows = caseInsensitiveResult.rows;
          }
        }

        const userId = userResult.rows[0].id;
        const organizationId = userResult.rows[0].organization_id;
        logger.info(`Found user ID: ${userId}, organization ID: ${organizationId}`);

        // Construct the SQL update based on available Stripe data
        let sql = `UPDATE organizations 
                  SET payment_completed = 1, 
                      subscription_status = 'active',
                      onboarding_completed = TRUE`;
        
        const args = [];
        
        if (stripeCustomerId) {
          sql += ', stripe_customer_id = ?';
          args.push(stripeCustomerId);
        }
        
        if (stripeSubscriptionId) {
          sql += ', stripe_subscription_id = ?';
          args.push(typeof stripeSubscriptionId === 'object' ? JSON.stringify(stripeSubscriptionId) : stripeSubscriptionId);
        }
        
        if (stripeUsageItemId) {
          sql += ', stripe_usage_item_id = ?';
          args.push(typeof stripeUsageItemId === 'object' ? JSON.stringify(stripeUsageItemId) : stripeUsageItemId);
        }
        
        sql += ' WHERE id = ?';
        args.push(organizationId);

        // Update organization with payment and Stripe info
        logger.info(`Updating organization ${organizationId} with payment data`);
        const updateResult = await client.execute({ sql, args });
        logger.info(`Organization update affected ${updateResult.rowsAffected} rows`);

        // Create a new session
        const sessionId = crypto.randomBytes(32).toString('hex');
        const expiresAt = new Date();
        expiresAt.setDate(expiresAt.getDate() + 7); // 7 days from now

        logger.info(`Creating new session for user ${userId}: ${sessionId.substring(0, 16)}...`);
        const sessionResult = await client.execute({
          sql: 'INSERT INTO sessions (id, user_id, expires_at) VALUES (?, ?, ?)',
          args: [sessionId, userId, expiresAt.toISOString()]
        });
        logger.info(`Session creation affected ${sessionResult.rowsAffected} rows`);

        // Set the session cookie
        logger.info(`Setting session cookie with expiry: ${expiresAt.toISOString()}`);
        setCookie('session', sessionId, {
          path: '/',
          httpOnly: true,
          secure: process.env.NODE_ENV === 'production',
          sameSite: 'lax',
          maxAge: 60 * 60 * 24 * 1 // 1 day -- shorter to force them to login again soon after onboarding
        });
        logger.info(`Session cookie set successfully`);

        // Get the subscription status to return to client
        logger.info(`Fetching subscription status for organization ${organizationId}`);
        const status = await checkPaymentStatus(db, organizationId);
        logger.info(`Subscription status: ${JSON.stringify(status)}`);

        logger.info(`Successfully completed payment setup for user ${userId} in organization ${organizationId}`);
        
        set.status = 200;
        return {
          success: true,
          data: status
        };

      } catch (error) {
        logger.error(`Error in payment completion: ${error}`);
        if (error instanceof Error && error.stack) {
          logger.error(`Stack trace: ${error.stack}`);
        }
        set.status = 500;
        return { 
          success: false,
          error: 'Failed to complete payment setup',
          details: error instanceof Error ? error.message : 'Unknown error'
        };
      }
    });

  return app;
};

