import Stripe from 'stripe';
import { config } from '../config';
import { logger } from '../logger';

// Initialize Stripe with the secret key from environment variables
const stripe = new Stripe(config.stripe.secretKey, {
  apiVersion: '2023-10-16',
});

export type PlanTier = 'basic' | 'pro' | 'enterprise';

// Map subscription tier IDs to Stripe price IDs
const PRICE_MAP: Record<PlanTier, string> = {
  basic: config.stripe.prices.basic,
  pro: config.stripe.prices.pro,
  enterprise: config.stripe.prices.enterprise,
};

// Extra resource price IDs
const EXTRA_AGENT_PRICE = config.stripe.prices.extraAgent;
const EXTRA_CONTACT_PRICE = config.stripe.prices.extraContact;

interface CreateSubscriptionOptions {
  tierId: PlanTier;
  organizationId: number;
  email: string;
  extraAgents: number;
  extraContacts: number;
  stripeCustomerId?: string;
}

export interface SubscriptionResult {
  customerId: string;
  subscriptionId: string;
  clientSecret?: string;
}

/**
 * Creates or updates a Stripe subscription for an organization
 */
export async function createOrUpdateSubscription({
  tierId,
  organizationId,
  email,
  extraAgents,
  extraContacts,
  stripeCustomerId,
}: CreateSubscriptionOptions): Promise<SubscriptionResult> {
  try {
    // Get or create a customer
    let customerId = stripeCustomerId;
    
    if (!customerId) {
      const customer = await stripe.customers.create({
        email,
        metadata: {
          organizationId: organizationId.toString(),
        },
      });
      customerId = customer.id;
      logger.info(`Created Stripe customer for organization ${organizationId}: ${customerId}`);
    }

    // Create subscription line items
    const items = [
      {
        price: PRICE_MAP[tierId],
        quantity: 1,
      },
    ];

    // Add extra agents if needed
    if (extraAgents > 0 && EXTRA_AGENT_PRICE) {
      items.push({
        price: EXTRA_AGENT_PRICE,
        quantity: extraAgents,
      });
    }

    // Add extra contacts (subscription is based on 100s of contacts)
    if (extraContacts > 0 && EXTRA_CONTACT_PRICE) {
      items.push({
        price: EXTRA_CONTACT_PRICE,
        // Calculate how many 100s of contacts
        quantity: Math.ceil(extraContacts / 100),
      });
    }

    // Create the subscription
    const subscription = await stripe.subscriptions.create({
      customer: customerId,
      items,
      payment_behavior: 'default_incomplete',
      payment_settings: {
        save_default_payment_method: 'on_subscription',
        payment_method_types: ['card'],
      },
      expand: ['latest_invoice.payment_intent'],
      metadata: {
        organizationId: organizationId.toString(),
        tierId,
        extraAgents: extraAgents.toString(),
        extraContacts: extraContacts.toString(),
      },
    });

    // Get the client secret from the subscription
    const invoice = subscription.latest_invoice as Stripe.Invoice;
    const paymentIntent = invoice.payment_intent as Stripe.PaymentIntent;
    const clientSecret = paymentIntent?.client_secret || undefined;

    logger.info(`Created Stripe subscription for organization ${organizationId}: ${subscription.id}`);

    return {
      customerId,
      subscriptionId: subscription.id,
      clientSecret,
    };
  } catch (error) {
    logger.error('Error creating Stripe subscription:', error);
    throw error;
  }
}

/**
 * Webhook handler for Stripe events
 */
export async function handleStripeWebhook(event: Stripe.Event): Promise<void> {
  logger.info(`Processing Stripe webhook: ${event.type}`);
  const db = new Database();

  try {
    switch (event.type) {
      case 'customer.subscription.updated': {
        const subscription = event.data.object as Stripe.Subscription;
        // Find organization with this subscription
        const orgResult = await db.query<{ id: number }>(
          'SELECT id FROM organizations WHERE stripe_subscription_id = ?',
          [subscription.id]
        );
        
        if (orgResult && orgResult.length > 0) {
          const organizationId = orgResult[0].id;
          
          // Map Stripe status to our status
          let subscriptionStatus = 'active';
          if (subscription.status === 'active' || subscription.status === 'trialing') {
            subscriptionStatus = 'active';
          } else if (subscription.status === 'past_due') {
            subscriptionStatus = 'past_due';
          } else {
            subscriptionStatus = 'inactive';
          }
          
          // Get extra agents and contacts
          const extraAgents = parseInt(subscription.metadata.extraAgents || '0', 10);
          const extraContacts = parseInt(subscription.metadata.extraContacts || '0', 10);
          
          // Update organization with subscription status
          await db.execute(
            `UPDATE organizations 
             SET subscription_status = ?,
                 billing_cycle_end = ?,
                 trial_end_date = ?,
                 extra_agents = ?,
                 extra_contacts = ?
             WHERE id = ?`,
            [
              subscriptionStatus,
              new Date(subscription.current_period_end * 1000).toISOString(),
              subscription.trial_end ? new Date(subscription.trial_end * 1000).toISOString() : null,
              extraAgents,
              extraContacts,
              organizationId
            ]
          );
          
          logger.info(`Updated organization ${organizationId} with subscription status: ${subscriptionStatus}`);
        } else {
          logger.warn(`No organization found for subscription: ${subscription.id}`);
        }
        break;
      }
      
      case 'invoice.payment_succeeded': {
        const invoice = event.data.object as Stripe.Invoice;
        if (invoice.subscription) {
          // Find organization with this subscription
          const orgResult = await db.query<{ id: number }>(
            'SELECT id FROM organizations WHERE stripe_subscription_id = ?',
            [invoice.subscription]
          );
          
          if (orgResult && orgResult.length > 0) {
            // Update last payment date and reset failure count
            await db.execute(
              `UPDATE organizations 
               SET last_payment_date = ?,
                   payment_failure_count = 0,
                   subscription_status = 'active'
               WHERE id = ?`,
              [new Date().toISOString(), orgResult[0].id]
            );
            
            logger.info(`Payment succeeded for organization ${orgResult[0].id}`);
          }
        }
        break;
      }
      
      case 'invoice.payment_failed': {
        const invoice = event.data.object as Stripe.Invoice;
        if (invoice.subscription) {
          // Find organization with this subscription
          const orgResult = await db.query<{ id: number, payment_failure_count: number }>(
            'SELECT id, payment_failure_count FROM organizations WHERE stripe_subscription_id = ?',
            [invoice.subscription]
          );
          
          if (orgResult && orgResult.length > 0) {
            const org = orgResult[0];
            const newFailureCount = org.payment_failure_count + 1;
            
            // Update payment failure count
            await db.execute(
              `UPDATE organizations 
               SET payment_failure_count = ?,
                   subscription_status = ?
               WHERE id = ?`,
              [
                newFailureCount,
                newFailureCount >= 3 ? 'past_due' : 'active', // Mark as past_due after 3 failures
                org.id
              ]
            );
            
            logger.warn(`Payment failed for organization ${org.id}. Failure count: ${newFailureCount}`);
          }
        }
        break;
      }
      
      case 'customer.subscription.deleted': {
        const subscription = event.data.object as Stripe.Subscription;
        const orgResult = await db.query<{ id: number }>(
          'SELECT id FROM organizations WHERE stripe_subscription_id = ?',
          [subscription.id]
        );
        
        if (orgResult && orgResult.length > 0) {
          // Mark subscription as canceled
          await db.execute(
            `UPDATE organizations SET subscription_status = 'canceled' WHERE id = ?`,
            [orgResult[0].id]
          );
          
          logger.info(`Subscription canceled for organization ${orgResult[0].id}`);
        }
        break;
      }
    }
  } catch (error) {
    logger.error('Error handling Stripe webhook:', error);
    throw error;
  }
}