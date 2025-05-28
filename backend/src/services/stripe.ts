import Stripe from 'stripe';
import { config } from '../config';
import { logger } from '../logger';
import { Database } from '../database';
import { Elysia, t } from 'elysia';
import type { UserContext } from '../types';
import { requireAuth } from '../middleware/auth';

const stripe = new Stripe(config.stripeSecretKey, {
  apiVersion: '2025-02-24.acacia'
});

export type SubscriptionStatus = {
  isActive: boolean;
  tier: string;
  currentPeriodEnd?: number;
  cancelAtPeriodEnd?: boolean;
  paymentStatus: 'paid' | 'unpaid' | 'no_subscription';
};

export async function checkPaymentStatus(db: Database, organizationId: number): Promise<SubscriptionStatus> {
  // First check local database
  const org = await db.fetchOne<{
    stripe_customer_id: string;
    stripe_subscription_id: string;
    subscription_status: string;
    payment_completed: number;
  }>(
    'SELECT stripe_customer_id, stripe_subscription_id, subscription_status, payment_completed FROM organizations WHERE id = ?',
    [organizationId]
  );

  if (!org) {
    throw new Error('Organization not found');
  }

  // If payment is already marked as completed locally, no need to check Stripe
  if (org.payment_completed === 1) {
    return {
      isActive: org.subscription_status === 'active',
      tier: 'paid',
      paymentStatus: 'paid'
    };
  }

  // If no Stripe subscription yet, return unpaid status0
  if (!org.stripe_subscription_id) {
    return {
      isActive: false,
      tier: 'basic',
      paymentStatus: 'no_subscription'
    };
  }

  // Check Stripe for current status
  try {
    const subscription = await stripe.subscriptions.retrieve(org.stripe_subscription_id);
    
    // Update local payment_completed status if paid
    if (subscription.status === 'active' && !subscription.cancel_at_period_end) {
      await db.execute(
        'UPDATE organizations SET payment_completed = 1 WHERE id = ?',
        [organizationId]
      );
    }

    return {
      isActive: subscription.status === 'active',
      tier: subscription.items.data[0].price.nickname || 'paid',
      currentPeriodEnd: subscription.current_period_end,
      cancelAtPeriodEnd: subscription.cancel_at_period_end,
      paymentStatus: subscription.status === 'active' ? 'paid' : 'unpaid'
    };
  } catch (error) {
    logger.error(`Error checking Stripe subscription status: ${error}`);
    return {
      isActive: false,
      tier: 'basic',
      paymentStatus: 'unpaid'
    };
  }
}

export async function activateSubscription(db: Database, organizationId: number): Promise<void> {
  await db.execute(
    'UPDATE organizations SET payment_completed = 1, subscription_status = ? WHERE id = ?',
    ['active', organizationId]
  );
}

export async function reportContactUsage(db: Database, organizationId: number): Promise<void> {
  const org = await db.fetchOne<{
    stripe_subscription_id: string;
    contact_count: number;
  }>(
    'SELECT stripe_subscription_id, (SELECT COUNT(*) FROM contacts WHERE organization_id = organizations.id) as contact_count FROM organizations WHERE id = ?',
    [organizationId]
  );

  if (!org?.stripe_subscription_id) {
    logger.info(`No subscription found for organization ${organizationId}`);
    return;
  }

  try {
    // Get subscription items to find the metered price item
    const subscription = await stripe.subscriptions.retrieve(org.stripe_subscription_id);
    const meteredItem = subscription.items.data.find(item => 
      item.price.id === config.stripe.prices.additionalContacts
    );

    if (!meteredItem) {
      logger.info(`No metered item found for subscription ${org.stripe_subscription_id}`);
      return;
    }

    // Report current contact count as usage
    await stripe.subscriptionItems.createUsageRecord(
      meteredItem.id,
      {
        quantity: org.contact_count,
        timestamp: Math.floor(Date.now() / 1000),
        action: 'set'
      }
    );

    logger.info(`Successfully reported contact usage for organization ${organizationId} (${org.contact_count} contacts)`);
  } catch (error) {
    logger.error(`Error reporting contact usage to Stripe: ${error}`);
    throw error;
  }
}

