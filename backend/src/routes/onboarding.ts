import { Elysia } from 'elysia'
import { logger } from '../logger'
import { Database } from '../database'
import { config } from '../config'
import { cookie } from '@elysiajs/cookie'
import Stripe from 'stripe'

// Initialize Stripe with secret key from environment variables
const stripe = new Stripe(process.env.STRIPE_SECRET_KEY || 'sk_test_51Qyh7RCBUPXAZKNGFySALjap1pDAtEwPtuY5TAzEuKKDq7cfAmHhmQIn8W1UMf2CuOvQ1umjiUrlpPauOc159fpM00nfohCZH3')
const YOUR_DOMAIN = config.clientUrl || 'http://localhost:3000'

// Helper function for generating unique slugs from names
async function generateUniqueSlug(db: Database, name: string): Promise<string> {
  let slug = name
    .toLowerCase()
    .trim()
    .replace(/[^a-z0-9]+/g, '-') // Replace non-alphanumeric chars with hyphens
    .replace(/^-+|-+$/g, '') // Remove leading/trailing hyphens
    .substring(0, 50); // Limit length

  // Check if slug exists
  let counter = 0;
  let uniqueSlug = slug;
  
  while (true) {
    const existing = await db.query<{ count: number }>(
      'SELECT COUNT(*) as count FROM organizations WHERE slug = ?',
      [uniqueSlug]
    );

    if (existing[0]?.count === 0) {
      break;
    }

    counter++;
    uniqueSlug = `${slug}-${counter}`;
  }

  return uniqueSlug;
}

export function createOnboardingRoutes() {
  const dbInstance = new Database()

  return new Elysia()    
    // Create a Stripe checkout session
    .post('/api/create-checkout-session', async ({ body, set }) => {
      try {
        const { priceId, meteredPriceId } = body as { priceId: string, meteredPriceId?: string }
        
        if (!priceId) {
          set.status = 400
          return {
            success: false,
            message: 'Price ID is required'
          }
        }
        
        logger.info(`Creating checkout session for base price: ${priceId}, metered price: ${meteredPriceId || 'none'}`);
        
        // Build line items array for the checkout session
        const lineItems: any[] = [
          {
            price: priceId,
            quantity: 1, // Required for regular subscription
          }
        ];
        
        // Add metered price if provided, WITHOUT a quantity (Stripe doesn't want quantity for metered prices)
        if (meteredPriceId) {
          lineItems.push({
            price: meteredPriceId,
            // No quantity for metered prices
          });
        }
        
        logger.info(`Creating checkout session with line items: ${JSON.stringify(lineItems)}`);
        
        // Create the session
        const session = await stripe.checkout.sessions.create({
          ui_mode: 'embedded',
          line_items: lineItems,
          mode: 'subscription',
          return_url: `${YOUR_DOMAIN}/return?session_id={CHECKOUT_SESSION_ID}`,
          automatic_tax: { enabled: true },
        })
        
        set.status = 200;
        return {
          success: true,
          clientSecret: session.client_secret
        }
      } catch (error) {
        logger.error(`Error creating checkout session: ${error}`)
        set.status = 500
        return {
          success: false,
          message: 'Failed to create checkout session'
        }
      }
    })
    
    // Get session status
    .get('/api/session-status', async ({ query, set }) => {
      try {
        const { session_id } = query as { session_id: string }
        
        if (!session_id) {
          set.status = 400
          return {
            success: false,
            message: 'Session ID is required'
          }
        }
        
        const session = await stripe.checkout.sessions.retrieve(session_id)
        
        return {
          success: true,
          status: session.status,
          customer_email: session.customer_details?.email
        }
      } catch (error) {
        logger.error(`Error retrieving session: ${error}`)
        set.status = 500
        return {
          success: false,
          message: 'Failed to retrieve session status'
        }
      }
    })
    
    // Simplified checkout endpoint that creates the account
    .post('/api/subscription/checkout', async ({ body, set }) => {
      try {
        logger.info(`Processing account creation request: ${JSON.stringify(body)}`);
        const { firstName, lastName, email, tierId = 'basic' } = body as { 
          firstName: string, 
          lastName: string, 
          email: string,
          tierId?: string
        };
        
        logger.info(`User info: ${firstName} ${lastName} (${email})`);
        
        if (!firstName || !lastName || !email) {
          logger.warn(`Missing required user information. Received: firstName=${!!firstName}, lastName=${!!lastName}, email=${!!email}`);
          set.status = 400;
          return { 
            success: false,
            message: 'Missing required user information'
          };
        }
        
        // Check if email already exists
        const existingUser = await dbInstance.query<{ id: number, organization_id: number, count: number }>(
          'SELECT id, organization_id, COUNT(*) as count FROM users WHERE LOWER(email) = LOWER(?)',
          [email]
        );
        
        let organizationId;
        
        if (existingUser[0]?.count > 0) {
          // User exists, check if their organization has completed onboarding
          const orgInfo = await dbInstance.query<{ onboarding_completed: number }>(
            'SELECT onboarding_completed FROM organizations WHERE id = ?',
            [existingUser[0].organization_id]
          );
          
          if (orgInfo[0]?.onboarding_completed === 1) {
            logger.warn(`Email ${email} is already registered with completed onboarding`);
            set.status = 400;
            return { 
              success: false, 
              message: 'This email address is already registered with completed onboarding'
            };
          }
          
          // Use the existing organization if onboarding is not complete
          logger.info(`Found existing organization ${existingUser[0].organization_id} with incomplete onboarding`);
          organizationId = existingUser[0].organization_id;
        } else {
          // Create new organization for new user
          // Generate a unique slug from the name
          const organizationName = `${firstName}'s Organization`;
          const slug = await generateUniqueSlug(dbInstance, organizationName);
          
          logger.info(`Generated organization name: "${organizationName}" and slug: "${slug}"`);
          
          // Create new organization
          const result = await dbInstance.query<{ id: number }>(
            'INSERT INTO organizations (name, slug) VALUES (?, ?)',
            [organizationName, slug]
          );
          
          if (result.length > 0) {
            organizationId = result[0].id;
          } else {
            throw new Error('Failed to create organization');
          }
        }
        
        // Create new user
        const resultUser = await dbInstance.query<{ id: number }>(
          'INSERT INTO users (firstName, lastName, email, organization_id) VALUES (?, ?, ?, ?)',
          [firstName, lastName, email, organizationId]
        );
        
        if (resultUser.length > 0) {
          logger.info(`User ${firstName} ${lastName} (${email}) successfully created`);
          set.status = 200;
          return {
            success: true,
            message: 'Account created successfully'
          };
        } else {
          throw new Error('Failed to create user');
        }
      } catch (error) {
        logger.error(`Error processing account creation: ${error}`)
        set.status = 500
        return {
          success: false,
          message: 'Failed to process account creation'
        }
      }
    })
}