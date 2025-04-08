import Stripe from 'stripe';
import { config } from '../config';
import { logger } from '../logger';
import { Database } from '../database';
import { Elysia, t, type Context } from 'elysia';

/**
 * Create a simple mock Stripe implementation for development
 */
class MockStripe {
  private generateId = (prefix: string) => `${prefix}_${Date.now()}${Math.floor(Math.random() * 1000)}`;
  
  customers = {
    create: async (data: any) => ({
      id: this.generateId('cus'),
      email: data.email,
      metadata: data.metadata || {},
      name: data.name,
    }),
  };

  subscriptions = {
    create: async (data: any) => ({
      id: this.generateId('sub'),
      customer: data.customer,
      items: { 
        data: [
          { id: this.generateId('si'), price: data.items[0].price, quantity: data.items[0].quantity }
        ] 
      },
      latest_invoice: {
        id: this.generateId('in'),
        payment_intent: {
          id: this.generateId('pi'),
          client_secret: this.generateId('secret'),
        }
      },
      metadata: data.metadata || {},
      status: 'active',
    }),
    update: async (id: string, data: any) => ({
      id,
      items: {
        data: [
          { id: this.generateId('si'), price: { id: (data.items && data.items[0] && data.items[0].price) || 'mock_price' }, quantity: (data.items && data.items[0] && data.items[0].quantity) || 1 }
        ]
      },
      metadata: data.metadata || {},
      status: 'active',
    }),
    retrieve: async (id: string) => ({
      id,
      items: { 
        data: [
          { id: this.generateId('si'), price: 'mock_price', quantity: 1 }
        ] 
      },
      metadata: {},
      status: 'active',
    }),
  };

  usageRecords = {
    create: async (data: any) => ({
      id: this.generateId('ur'),
      quantity: data.quantity,
      subscription_item: data.subscription_item,
    }),
  };
}

// Initialize Stripe client or mock
let stripeClient: any;

if (config.stripe?.secretKey && !config.stripe.useMock) {
  try {
    logger.info('Initializing real Stripe client');
    stripeClient = new Stripe(config.stripe.secretKey, {
      apiVersion: '2023-10-16' as any, // Use a supported version with type casting
    });
    logger.info('Using real Stripe implementation');
  } catch (error) {
    logger.error(`Failed to initialize Stripe client: ${error}`);
    logger.info('Falling back to mock Stripe implementation');
    stripeClient = new MockStripe();
  }
} else {
  logger.info('Using mock Stripe implementation');
  stripeClient = new MockStripe();
}

// Stripe price IDs - note these should be price IDs, not product IDs
// The product ID from the screenshot is: prod_S4pZJG9JSjfrnG
// But we need to use the price IDs associated with this product
const BASE_SUBSCRIPTION_PRICE_ID = config.stripe?.prices?.contactBaseTier || 'price_1RAfzdCBUPXAZKNGI0cj0qP3'; // This should be a price ID
const ADDITIONAL_CONTACTS_PRICE_ID = config.stripe?.prices?.additionalContacts || 'price_1RAfzoCBUPXAZKNG6OWSGA60'; // This should be a price ID
const PRICING_TABLE_ID = config.stripe?.pricingTableId || 'prctbl_1RAfz9CBUPXAZKNG0EyV8bRU';

// Define interfaces
export type PlanTier = 'basic' | 'pro' | 'enterprise' | string;

interface CreateSubscriptionOptions {
  userId: string;
  email: string;
  organizationId?: number;
  stripeCustomerId?: string;
  initialContactCount?: number;
}

interface SubscriptionResult {
  customerId: string;
  subscriptionId: string;
  subscriptionItemId?: string;
  clientSecret?: string;
}

/**
 * Generate a unique ID for database records
 */
function generateId(prefix: string): string {
  return `${prefix}_${Date.now()}${Math.floor(Math.random() * 1000)}`;
}

/**
 * Creates a new usage-based subscription for contacts
 */
export async function createContactSubscription(options: CreateSubscriptionOptions): Promise<SubscriptionResult> {
  const { userId, email, organizationId, stripeCustomerId, initialContactCount = 0 } = options;
  
  try {
    // Validate required parameters
    if (!userId) {
      throw new Error('userId is required');
    }
    
    // Get or create customer
    let customerId = stripeCustomerId;
    if (!customerId) {
      const customer = await stripeClient.customers.create({
        email,
        metadata: {
          organizationId: organizationId ? organizationId.toString() : null,
          userId,
        },
      });
      customerId = customer.id;
      logger.info(`Created Stripe customer for user ${userId}: ${customerId}`);
    }
    
    // Create subscription with base fee and usage-based additional contacts
    const subscription = await stripeClient.subscriptions.create({
      customer: customerId,
      items: [
        {
          // Base subscription (flat fee for first 500 contacts)
          price: BASE_SUBSCRIPTION_PRICE_ID,
          quantity: 1,
        },
        {
          // Additional contacts as usage-based billing
          price: ADDITIONAL_CONTACTS_PRICE_ID,
        }
      ],
      payment_behavior: 'default_incomplete',
      payment_settings: {
        save_default_payment_method: 'on_subscription',
        payment_method_types: ['card'],
      },
      metadata: {
        userId,
        organizationId: organizationId ? organizationId.toString() : null,
      },
    });
    
    // Get the subscription item ID for the usage-based item
    const usageSubscriptionItemId = subscription.items.data.find(
      (item: { price: { id: string } }) => item.price.id === ADDITIONAL_CONTACTS_PRICE_ID
    )?.id;
    
    // If we have initial contacts above 500, report usage immediately
    if (initialContactCount > 500 && usageSubscriptionItemId) {
      const additionalBlocks = Math.ceil((initialContactCount - 500) / 500);
      await reportContactUsage(usageSubscriptionItemId, additionalBlocks);
      logger.info(`Reported initial usage of ${additionalBlocks} blocks for subscription ${subscription.id}`);
    }
    
    // Extract necessary information
    const subscriptionId = subscription.id;
    
    // Extract client secret
    let clientSecret;
    if (subscription.latest_invoice && 
        subscription.latest_invoice.payment_intent && 
        subscription.latest_invoice.payment_intent.client_secret) {
      clientSecret = subscription.latest_invoice.payment_intent.client_secret;
    }
    
    logger.info(`Created usage-based Stripe subscription ${subscriptionId} for user ${userId}`);
    
    // Store subscription info in the database
    const db = new Database();
    await db.execute(`
      INSERT INTO subscriptions (
        id, 
        user_id, 
        stripe_subscription_id, 
        stripe_customer_id,
        stripe_usage_item_id,
        status, 
        current_contact_count,
        created_at, 
        updated_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    `, [
      generateId('sub_local_'),
      userId,
      subscriptionId,
      customerId,
      usageSubscriptionItemId || null,
      'active',
      initialContactCount,
      new Date().toISOString(),
      new Date().toISOString()
    ]);
    
    // Create the return object with type assertion
    const result = {
      customerId,
      subscriptionId,
      // Don't include subscriptionItemId if undefined
    } as SubscriptionResult;
    
    // Add clientSecret if available
    if (clientSecret) {
      result.clientSecret = clientSecret;
    }
    
    // Add subscriptionItemId if available
    if (usageSubscriptionItemId) {
      result.subscriptionItemId = usageSubscriptionItemId;
    }
    
    return result;
  } catch (error) {
    logger.error(`Error creating usage-based Stripe subscription: ${error}`);
    throw error;
  }
}

/**
 * Report contact usage to Stripe (for usage-based billing)
 * This should be called whenever contact count changes significantly
 * @param subscriptionItemId The Stripe subscription item ID for the usage-based price
 * @param blocks Number of additional 500-contact blocks beyond the first 500
 */
export async function reportContactUsage(subscriptionItemId: string, blocks: number): Promise<void> {
  try {
    if (!subscriptionItemId) {
      throw new Error('subscriptionItemId is required');
    }
    
    // Only report positive numbers
    const quantity = Math.max(0, blocks);
    
    // Report the usage to Stripe
    await stripeClient.usageRecords.create(
      subscriptionItemId,
      {
        quantity,
        timestamp: Math.floor(Date.now() / 1000), // Current time in Unix timestamp
        action: 'set', // Replace any previous usage records
      }
    );
    
    logger.info(`Reported usage of ${quantity} blocks to Stripe for subscription item ${subscriptionItemId}`);
  } catch (error) {
    logger.error(`Error reporting usage to Stripe: ${error}`);
    throw error;
  }
}

/**
 * Update contact count and report usage to Stripe
 * This should be called whenever contacts are added or removed
 */
export async function updateContactCount(userId: string, newContactCount: number): Promise<void> {
  try {
    if (!userId) {
      throw new Error('userId is required');
    }
    
    const db = new Database();
    
    // Get current subscription
    const subscription = await db.fetchOne<{
      stripe_subscription_id: string;
      stripe_usage_item_id: string;
      current_contact_count: number;
    }>(`
      SELECT stripe_subscription_id, stripe_usage_item_id, current_contact_count 
      FROM subscriptions 
      WHERE user_id = ? AND status = 'active'
      ORDER BY created_at DESC LIMIT 1
    `, [userId]);
    
    if (!subscription) {
      logger.info(`No active subscription found for user ${userId}`);
      return;
    }
    
    // Calculate blocks for usage-based billing (each block is 500 contacts beyond the first 500)
    const additionalBlocks = Math.max(0, Math.ceil((newContactCount - 500) / 500));
    
    // Report usage to Stripe if we have a usage item ID
    if (subscription.stripe_usage_item_id) {
      await reportContactUsage(subscription.stripe_usage_item_id, additionalBlocks);
    }
    
    // Update the contact count in our database
    await db.execute(`
      UPDATE subscriptions 
      SET current_contact_count = ?, updated_at = ? 
      WHERE user_id = ? AND status = 'active'
    `, [
      newContactCount,
      new Date().toISOString(),
      userId
    ]);
    
    logger.info(`Updated contact count to ${newContactCount} for user ${userId}`);
  } catch (error) {
    logger.error(`Error updating contact count: ${error}`);
    throw error;
  }
}

/**
 * Get the pricing table ID for embedding
 */
export function getPricingTableId(): string {
  return PRICING_TABLE_ID;
}

/**
 * Get subscription info for a user
 */
export async function getSubscriptionInfo(userId: string): Promise<{
  hasSubscription: boolean;
  contactCount: number;
  status: string;
  subscriptionId?: string;
}> {
  try {
    if (!userId) {
      throw new Error('userId is required');
    }
    
    const db = new Database();
    
    // Get subscription from database
    const subscription = await db.fetchOne<{
      stripe_subscription_id: string;
      current_contact_count: number;
      status: string;
    }>(`
      SELECT stripe_subscription_id, current_contact_count, status
      FROM subscriptions 
      WHERE user_id = ? AND status != 'cancelled'
      ORDER BY created_at DESC LIMIT 1
    `, [userId]);
    
    if (!subscription) {
      return {
        hasSubscription: false,
        contactCount: 0,
        status: 'none',
      };
    }
    
    return {
      hasSubscription: true,
      contactCount: subscription.current_contact_count,
      status: subscription.status,
      subscriptionId: subscription.stripe_subscription_id,
    };
  } catch (error) {
    logger.error(`Error getting subscription info: ${error}`);
    throw error;
  }
}

/**
 * Check if a user's subscription tier is appropriate for their contact count
 * @param userId The user ID
 * @param contactCount The total number of contacts
 * @returns Information about current tier, required tier, and if upgrade is needed
 */
export async function checkSubscriptionTier(userId: string, contactCount: number): Promise<{
  currentTier: PlanTier;
  requiredTier: PlanTier;
  needsUpgrade: boolean;
}> {
  try {
    if (!userId) {
      throw new Error('userId is required');
    }
    
    const db = new Database();
    
    // Get current subscription info
    const subscription = await getSubscriptionInfo(userId);
    
    // Determine current tier
    let currentTier: PlanTier = 'basic';
    
    if (subscription.hasSubscription) {
      // For now, all subscriptions are the same tier
      // This can be expanded later for multiple tiers
      currentTier = 'basic';
    }
    
    // Determine required tier based on contact count
    // For now, we only have one tier with usage-based billing
    const requiredTier: PlanTier = 'basic';
    
    // Check if upgrade is needed (currently always false with our usage-based model)
    const needsUpgrade = false;
    
    return {
      currentTier,
      requiredTier,
      needsUpgrade
    };
  } catch (error) {
    logger.error(`Error checking subscription tier: ${error}`);
    throw error;
  }
}

/**
 * Update a user's subscription tier if needed
 * @param userId The user ID
 * @param targetTier The target subscription tier
 * @returns The updated subscription info
 */
export async function updateSubscriptionTier(userId: string, targetTier: PlanTier): Promise<{
  success: boolean;
  message: string;
  subscriptionId?: string;
}> {
  try {
    if (!userId) {
      throw new Error('userId is required');
    }
    
    const db = new Database();
    
    // Get current subscription
    const subscription = await db.fetchOne<{
      stripe_subscription_id: string;
      stripe_customer_id: string;
      status: string;
    }>(`
      SELECT stripe_subscription_id, stripe_customer_id, status
      FROM subscriptions 
      WHERE user_id = ? AND status != 'cancelled'
      ORDER BY created_at DESC LIMIT 1
    `, [userId]);
    
    if (!subscription) {
      throw new Error(`No active subscription found for user ${userId}`);
    }
    
    // With our current usage-based model, we don't actually need to change tiers
    // This function is here for future expansion to multiple tiers
    
    return {
      success: true,
      message: 'Subscription tier is already appropriate',
      subscriptionId: subscription.stripe_subscription_id
    };
  } catch (error) {
    logger.error(`Error updating subscription tier: ${error}`);
    throw error;
  }
}

/**
 * Alias for createContactSubscription (for backwards compatibility)
 */
export const createContactBasedSubscription = createContactSubscription;

/**
 * Creates a Stripe Checkout session for a new subscription
 */
export async function createCheckoutSession(options: {
  userId: string;
  email: string;
  orgSlug: string;
  successUrl: string;
  cancelUrl: string;
  stripeCustomerId?: string;
}): Promise<{ id: string }> {
  const { userId, email, orgSlug, successUrl, cancelUrl } = options;
  let { stripeCustomerId } = options;

  try {
    // Get or create customer
    if (!stripeCustomerId) {
      const customer = await stripeClient.customers.create({
        email,
        metadata: {
          userId,
          orgSlug,
        },
      });
      stripeCustomerId = customer.id;
      logger.info(`Created Stripe customer for user ${userId} during checkout: ${stripeCustomerId}`);
    }

    // Create a Checkout Session
    // Uses the usage-based billing model (base fee + per 500 contacts)
    const session = await stripeClient.checkout.sessions.create({
      payment_method_types: ['card'],
      mode: 'subscription',
      customer: stripeCustomerId,
      line_items: [
        {
          // Base subscription (includes first 500 contacts)
          price: BASE_SUBSCRIPTION_PRICE_ID,
          quantity: 1,
        },
        {
          // Additional contacts usage-based item (reported separately)
          price: ADDITIONAL_CONTACTS_PRICE_ID,
        }
      ],
      success_url: successUrl,
      cancel_url: cancelUrl,
      metadata: {
        userId,
        orgSlug,
      },
      // Enable collecting the customer's billing address
      billing_address_collection: 'required',
    });

    logger.info(`Created Stripe Checkout session ${session.id} for user ${userId}, org ${orgSlug}`);

    return { id: session.id };
  } catch (error) {
    logger.error(`Error creating Stripe Checkout session: ${error}`);
    throw error;
  }
}