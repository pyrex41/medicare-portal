import { Elysia } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import { config } from '../config';
import { getUserFromSession } from '../services/auth';

// Mock Stripe if not available
let stripe: any;
try {
  if (config.stripeApiKey) {
    // @ts-ignore - Dynamic import for Stripe
    stripe = require('stripe')(config.stripeApiKey);
  } else {
    // Mock Stripe for development
    stripe = {
      checkout: {
        sessions: {
          create: async () => ({ id: `test_session_id_${Date.now()}` })
        }
      },
      webhooks: {
        constructEvent: () => ({ 
          type: 'test.event', 
          data: { object: {} } 
        })
      }
    };
    logger.info('Using mock Stripe implementation');
  }
} catch (error) {
  logger.error(`Error loading Stripe: ${error}`);
  // Create a mock implementation
  stripe = {
    checkout: {
      sessions: {
        create: async () => ({ id: `test_session_id_${Date.now()}` })
      }
    },
    webhooks: {
      constructEvent: () => ({ 
        type: 'test.event', 
        data: { object: {} } 
      })
    }
  };
  logger.info('Using mock Stripe implementation due to error');
}

export const createStripeRoutes = () => {
  return new Elysia({ prefix: '/api/stripe' })
    // Create a Stripe checkout session
    .post('/create-checkout-session', async ({ body, set, request }) => {
      const db = new Database();
      
      try {
        const user = await getUserFromSession(request);
        if (!user) {
          set.status = 401;
          return { error: 'Unauthorized' };
        }
        
        const { orgSlug, tierId, extraAgents, extraContacts } = body as {
          orgSlug: string;
          tierId: string;
          extraAgents: number;
          extraContacts: number;
        };
        
        // Get the organization
        const organization = await db.query(
          'SELECT id, name, stripe_customer_id FROM organizations WHERE slug = ?',
          [orgSlug]
        ).then(rows => rows[0]);
        
        if (!organization) {
          set.status = 404;
          return { error: 'Organization not found' };
        }
        
        // Get the subscription tier pricing
        const tier = await db.query(
          'SELECT id, name, price_monthly, agent_limit, contact_limit FROM subscription_tiers WHERE id = ?',
          [tierId]
        ).then(rows => rows[0]);
        
        if (!tier) {
          set.status = 404;
          return { error: 'Subscription tier not found' };
        }
        
        // Calculate the total price
        const basePriceInCents = Number(tier.price_monthly);
        const extraAgentPriceInCents = extraAgents * 2000; // $20 per agent
        const extraContactsPackages = Math.ceil(extraContacts / 5000);
        const extraContactsPriceInCents = extraContactsPackages * 5000; // $50 per 5000 contacts
        
        const totalPriceInCents = basePriceInCents + extraAgentPriceInCents + extraContactsPriceInCents;
        
        let sessionConfig: any = {
          payment_method_types: ['card'],
          line_items: [
            {
              price_data: {
                currency: 'usd',
                product_data: {
                  name: tier.name + ' Plan',
                  description: `Includes ${tier.agent_limit} agents and ${tier.contact_limit} contacts`
                },
                unit_amount: basePriceInCents
              },
              quantity: 1
            }
          ],
          mode: 'subscription',
          success_url: config.clientUrl + '/dashboard?payment_success=true',
          cancel_url: config.clientUrl + '/change-plan?payment_canceled=true',
        };
        
        // Add extra agents if any
        if (extraAgents > 0) {
          sessionConfig.line_items.push({
            price_data: {
              currency: 'usd',
              product_data: {
                name: 'Extra Agents',
                description: `${extraAgents} additional agents at $20/agent/month`
              },
              unit_amount: 2000,
            },
            quantity: extraAgents
          });
        }
        
        // Add extra contacts if any
        if (extraContacts > 0) {
          sessionConfig.line_items.push({
            price_data: {
              currency: 'usd',
              product_data: {
                name: 'Extra Contacts',
                description: `${extraContactsPackages} packages of 5,000 contacts at $50/package/month`
              },
              unit_amount: 5000,
            },
            quantity: extraContactsPackages
          });
        }
        
        // If organization already has a Stripe customer ID, use it
        if (organization.stripe_customer_id) {
          sessionConfig.customer = organization.stripe_customer_id;
        } else {
          sessionConfig.customer_email = user.email;
        }

        // Create the checkout session
        let session;
        if (config.stripeApiKey) {
          session = await stripe.checkout.sessions.create(sessionConfig);
        } else {
          // For development without Stripe API key
          logger.info('Creating test checkout session with config:', sessionConfig);
          session = { id: 'test_session_id_' + Date.now() };
        }
        
        // Store the checkout session ID in the database for later verification
        await db.execute(
          `UPDATE organizations 
           SET stripe_checkout_session = ? 
           WHERE id = ?`,
          [session.id, organization.id]
        );
        
        // Return the session ID to the client
        return { sessionId: session.id };
        
      } catch (error) {
        logger.error('Error creating checkout session:', error);
        set.status = 500;
        return { error: 'Failed to create checkout session' };
      }
    })
    
    // Redirect to Stripe checkout
    .get('/redirect-to-checkout', async ({ query, set }) => {
      const sessionId = query.session_id as string;
      
      if (!sessionId) {
        set.status = 400;
        return { error: 'Session ID is required' };
      }
      
      // In production, you would use Stripe's client-side SDK to redirect
      // For this demo, we'll simulate by redirecting to a success page
      if (config.stripeApiKey) {
        set.redirect = `https://checkout.stripe.com/pay/${sessionId}`;
      } else {
        // Simulate successful payment in development
        set.redirect = '/dashboard?payment_success=true&test_mode=true';
      }
      
      return {};
    })
    
    // Webhook for Stripe events (payment completion, etc.)
    .post('/webhook', async ({ body, request, set }) => {
      let event;
      const db = new Database();
      
      try {
        const signature = request.headers.get('stripe-signature');
        
        if (config.stripeWebhookSecret && config.stripeApiKey && signature) {
          event = stripe.webhooks.constructEvent(
            body, // Raw body needed here
            signature,
            config.stripeWebhookSecret
          );
        } else {
          // For development without Stripe
          event = { 
            type: 'checkout.session.completed',
            data: { object: body } 
          };
        }
        
        // Handle the event
        if (event.type === 'checkout.session.completed') {
          await handleCheckoutComplete(event.data.object, db);
        } else {
          logger.info(`Unhandled event type ${event.type}`);
        }
        
        set.status = 200;
        return { received: true };
      } catch (error) {
        logger.error('Error processing webhook:', error);
        set.status = 400;
        return { error: `Webhook Error: ${error instanceof Error ? error.message : 'Unknown error'}` };
      }
    });
};

async function handleCheckoutComplete(session: any, db: Database): Promise<void> {
  try {
    // Find the organization by checkout session ID
    const organization = await db.query(
      `SELECT id, slug FROM organizations WHERE stripe_checkout_session = ?`,
      [session.id]
    ).then(rows => rows[0]);
    
    if (!organization) {
      logger.error('Organization not found for checkout session:', session.id);
      return;
    }
    
    // Update the organization with the subscription info
    await db.execute(
      `UPDATE organizations 
       SET stripe_subscription_id = ?, 
           stripe_customer_id = ?,
           stripe_checkout_session = NULL,
           agent_limit = (SELECT agent_limit FROM subscription_tiers WHERE id = ?) + ?,
           contact_limit = (SELECT contact_limit FROM subscription_tiers WHERE id = ?) + ?,
           subscription_tier = ?
       WHERE id = ?`,
      [
        session.subscription, 
        session.customer, 
        session.metadata?.tierId || 'basic',
        Number(session.metadata?.extraAgents || 0),
        session.metadata?.tierId || 'basic',
        Number(session.metadata?.extraContacts || 0),
        session.metadata?.tierId || 'basic',
        organization.id
      ]
    );
    
    logger.info(`Subscription updated for organization: ${organization.slug}`);
  } catch (error) {
    logger.error('Error handling checkout completion:', error);
  }
} 