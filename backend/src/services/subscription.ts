import { Database } from '../database';
import { logger } from '../logger';
import Stripe from 'stripe';
import { config } from '../config';

// Define possible account statuses
export type AccountStatus = 
  | 'good_standing'           // Everything is fine
  | 'inactive'                // Subscription inactive (canceled, past_due, etc.)
  | 'agent_limit_exceeded'    // Too many agents for current plan
  | 'contact_limit_exceeded'; // Too many contacts for current plan

export interface AccountStatusDetails {
  status: AccountStatus;
  message: string;
  organizationId: number;
  organizationName: string;
  organizationSlug: string;
  subscriptionTier: string;
  subscriptionStatus: string;
  agentLimit: number;
  contactLimit: number;
  currentAgentCount: number;
  currentContactCount: number;
  billingCycleEnd?: Date;
  paymentFailureCount: number;
}

/**
 * Updates organization subscription status from Stripe
 */
export async function syncSubscriptionStatus(organizationId: number): Promise<void> {
  const db = new Database();
  
  try {
    // Get organization details
    const orgResult = await db.execute(
      'SELECT stripe_subscription_id, stripe_customer_id FROM organizations WHERE id = ?', 
      [organizationId]
    );
    
    if (!orgResult.rows || orgResult.rows.length === 0) {
      logger.error(`Organization not found: ${organizationId}`);
      return;
    }
    
    const org = orgResult.rows[0];
    
    // If no Stripe subscription ID, nothing to sync
    if (!org[0]) {
      logger.info(`No Stripe subscription for organization ${organizationId}`);
      return;
    }
    
    // Initialize Stripe client
    const stripe = new Stripe(config.stripe.secretKey, {
      apiVersion: '2025-02-24.acacia' as Stripe.LatestApiVersion,
    });
    
    // Fetch the subscription from Stripe
    const subscription = await stripe.subscriptions.retrieve(org[0]);
    
    // Map Stripe status to our status
    let subscriptionStatus = 'active';
    if (subscription.status === 'active' || subscription.status === 'trialing') {
      subscriptionStatus = 'active';
    } else if (subscription.status === 'past_due') {
      subscriptionStatus = 'past_due';
    } else {
      subscriptionStatus = 'inactive';
    }
    
    // Get the billing cycle end
    const billingCycleEnd = new Date(subscription.current_period_end * 1000);
    
    // Get trial end if applicable
    const trialEnd = subscription.trial_end 
      ? new Date(subscription.trial_end * 1000) 
      : null;
    
    // Get extra agents and contacts from subscription metadata
    const extraAgents = parseInt(subscription.metadata.extraAgents || '0', 10);
    const extraContacts = parseInt(subscription.metadata.extraContacts || '0', 10);
    
    // Update the organization with Stripe data
    await db.execute(`
      UPDATE organizations 
      SET 
        subscription_status = ?,
        billing_cycle_end = ?,
        trial_end_date = ?,
        extra_agents = ?,
        extra_contacts = ?
      WHERE id = ?
    `, [
      subscriptionStatus,
      billingCycleEnd.toISOString(),
      trialEnd?.toISOString() || null,
      extraAgents,
      extraContacts,
      organizationId
    ]);
    
    logger.info(`Updated subscription status for organization ${organizationId}: ${subscriptionStatus}`);
    
  } catch (error) {
    logger.error(`Error syncing subscription status for organization ${organizationId}:`, error);
    throw error;
  }
}

/**
 * Checks if an organization's account is in good standing
 */
export async function checkAccountStatus(organizationId: number): Promise<AccountStatusDetails> {
  const db = new Database();
  
  try {
    // Try to sync with Stripe first
    try {
      await syncSubscriptionStatus(organizationId);
    } catch (error) {
      logger.warn(`Could not sync with Stripe for organization ${organizationId}:`, error);
    }
    
    // Get basic organization info first (this should always work)
    const orgResult = await db.execute(
      'SELECT id, name, slug, subscription_tier, subscription_status, agent_limit, contact_limit, extra_agents, extra_contacts, billing_cycle_end, payment_failure_count FROM organizations WHERE id = ?', 
      [organizationId]
    );
    
    if (!orgResult.rows || orgResult.rows.length === 0) {
      throw new Error(`Organization not found: ${organizationId}`);
    }
    
    const org = orgResult.rows[0];
    
    try {
      // Try to query the organization_status view
      const statusResult = await db.execute(
        'SELECT * FROM organization_status WHERE id = ?', 
        [organizationId]
      );
      
      if (statusResult.rows && statusResult.rows.length > 0) {
        const status = statusResult.rows[0];
        const columns = statusResult.columns || [];
        
        // Create an object from the row array using column names
        const statusObj: any = {};
        columns.forEach((col, i) => {
          statusObj[col] = status[i];
        });
        
        // Prepare status details with appropriate message
        let message = '';
        switch (statusObj.account_status) {
          case 'inactive':
            message = `Your subscription is ${statusObj.subscription_status}. Please update your payment method.`;
            break;
            
          case 'agent_limit_exceeded':
            message = `Your account has ${statusObj.current_agent_count} agents, but your plan only allows for ${statusObj.agent_limit + statusObj.extra_agents}. Please remove some agents or upgrade your plan.`;
            break;
            
          case 'contact_limit_exceeded':
            message = `Your account has ${statusObj.current_contact_count} contacts, but your plan only allows for ${statusObj.contact_limit + statusObj.extra_contacts}. Please remove some contacts or upgrade your plan.`;
            break;
            
          case 'good_standing':
          default:
            message = 'Your account is in good standing.';
            break;
        }
        
        return {
          status: statusObj.account_status,
          message,
          organizationId: statusObj.id,
          organizationName: statusObj.name,
          organizationSlug: statusObj.slug,
          subscriptionTier: statusObj.subscription_tier,
          subscriptionStatus: statusObj.subscription_status,
          agentLimit: statusObj.agent_limit + statusObj.extra_agents,
          contactLimit: statusObj.contact_limit + statusObj.extra_contacts,
          currentAgentCount: statusObj.current_agent_count,
          currentContactCount: statusObj.current_contact_count,
          billingCycleEnd: statusObj.billing_cycle_end ? new Date(statusObj.billing_cycle_end) : undefined,
          paymentFailureCount: statusObj.payment_failure_count
        };
      }
    } catch (viewError) {
      // If there's an error querying the view (e.g., it doesn't exist),
      // we'll fall through to the default implementation below
      logger.warn(`Error querying organization_status view: ${viewError}`);
    }
    
    // If we get here, either the view doesn't exist or there was no result
    // Provide default values based on the organization record
    
    // Default to good standing
    const accountStatus: AccountStatus = 'good_standing';
    const message = 'Your account is in good standing.';
    
    // Extract values from the row array
    const columns = orgResult.columns || [];
    const orgObj: any = {};
    columns.forEach((col, i) => {
      orgObj[col] = org[i];
    });
    
    // Use defaults for missing values
    const subscriptionStatus = orgObj.subscription_status || 'active';
    const agentLimit = orgObj.agent_limit || 5;
    const contactLimit = orgObj.contact_limit || 100;
    const extraAgents = orgObj.extra_agents || 0;
    const extraContacts = orgObj.extra_contacts || 0;
    
    return {
      status: accountStatus,
      message,
      organizationId: orgObj.id,
      organizationName: orgObj.name,
      organizationSlug: orgObj.slug,
      subscriptionTier: orgObj.subscription_tier,
      subscriptionStatus,
      agentLimit: agentLimit + extraAgents,
      contactLimit: contactLimit + extraContacts,
      currentAgentCount: 0, // Default to 0 since we can't query agents table
      currentContactCount: 0, // Default to 0 since we can't query contacts table
      billingCycleEnd: orgObj.billing_cycle_end ? new Date(orgObj.billing_cycle_end) : undefined,
      paymentFailureCount: orgObj.payment_failure_count || 0
    };
    
  } catch (error) {
    logger.error(`Error checking account status for organization ${organizationId}:`, error);
    throw error;
  }
}