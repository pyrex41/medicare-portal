/**
 * Contact Tracking Service
 * 
 * This service handles tracking unique contacts for usage-based billing.
 * It ensures that:
 * 1. Each unique contact is only counted once per billing cycle
 * 2. Contact counts are properly reported to Stripe
 * 3. Organizations can see their current usage
 */

import { Database } from '../database';
import { logger } from '../logger';
import { reportContactUsage } from './stripe';
import { v4 as uuidv4 } from 'uuid';

/**
 * Generate a unique ID for database records
 */
function generateId(): string {
  return uuidv4();
}

/**
 * Get the current billing cycle key for an organization
 * Format: YYYY-MM for monthly cycles
 */
export async function getCurrentBillingCycle(organizationId: string): Promise<string> {
  const db = new Database();
  
  try {
    // Check if organization has a current billing cycle
    const org = await db.fetchOne<{
      current_billing_cycle_key: string | null;
      contact_count_billing_date: string | null;
    }>('SELECT current_billing_cycle_key, contact_count_billing_date FROM organizations WHERE id = ?', [organizationId]);
    
    if (!org) {
      throw new Error(`Organization not found: ${organizationId}`);
    }
    
    // If organization has a current billing cycle, return it
    if (org.current_billing_cycle_key) {
      return org.current_billing_cycle_key;
    }
    
    // If not, create a new billing cycle
    const now = new Date();
    const cycleKey = `${now.getFullYear()}-${String(now.getMonth() + 1).padStart(2, '0')}`;
    
    // Determine billing start date (default to 1st of month if not set)
    const billingDay = org.contact_count_billing_date || '01';
    const month = now.getMonth();
    const year = now.getFullYear();
    
    // Create start and end dates for the cycle
    const startDate = new Date(year, month, parseInt(billingDay, 10));
    const endDate = new Date(year, month + 1, parseInt(billingDay, 10));
    
    // Handle case where current date is before billing day in current month
    if (now.getDate() < parseInt(billingDay, 10)) {
      startDate.setMonth(startDate.getMonth() - 1);
      endDate.setMonth(endDate.getMonth() - 1);
    }
    
    // Create the billing cycle record
    await db.execute(`
      INSERT INTO billing_cycle_history (
        id, organization_id, cycle_key, start_date, end_date
      ) VALUES (?, ?, ?, ?, ?)
    `, [
      generateId(),
      organizationId,
      cycleKey,
      startDate.toISOString(),
      endDate.toISOString()
    ]);
    
    // Update the organization with the new billing cycle
    await db.execute(`
      UPDATE organizations 
      SET current_billing_cycle_key = ? 
      WHERE id = ?
    `, [cycleKey, organizationId]);
    
    logger.info(`Created new billing cycle ${cycleKey} for organization ${organizationId}`);
    
    return cycleKey;
  } catch (error) {
    logger.error(`Error getting current billing cycle: ${error}`);
    throw error;
  }
}

/**
 * Track a contact upload to ensure it's counted properly for billing
 */
export async function trackContact(
  organizationId: string, 
  userId: string, 
  contactEmail: string, 
  firstName?: string, 
  lastName?: string
): Promise<{ isNew: boolean; contactId: string }> {
  const db = new Database();
  
  try {
    // Validate inputs
    if (!organizationId || !userId || !contactEmail) {
      throw new Error('Missing required parameters for contact tracking');
    }
    
    // Normalize email for consistent lookups
    const normalizedEmail = contactEmail.trim().toLowerCase();
    
    // Get current billing cycle
    const billingCycle = await getCurrentBillingCycle(organizationId);
    
    // Check if contact already exists in this organization
    const existingContact = await db.fetchOne<{ id: string; billing_cycle_key: string | null }>(
      'SELECT id, billing_cycle_key FROM contact_history WHERE organization_id = ? AND email = ?',
      [organizationId, normalizedEmail]
    );
    
    let isNew = false;
    let contactId: string;
    
    if (existingContact) {
      // Contact exists - update last_uploaded timestamp
      contactId = existingContact.id;
      await db.execute(`
        UPDATE contact_history 
        SET last_uploaded = CURRENT_TIMESTAMP,
            first_name = COALESCE(?, first_name),
            last_name = COALESCE(?, last_name)
        WHERE id = ?
      `, [
        firstName || null,
        lastName || null,
        contactId
      ]);
      
      // If contact was from a different billing cycle, count it as new for this cycle
      if (existingContact.billing_cycle_key !== billingCycle) {
        isNew = true;
        await db.execute(`
          UPDATE contact_history 
          SET billing_cycle_key = ? 
          WHERE id = ?
        `, [billingCycle, contactId]);
      }
    } else {
      // New contact - insert into history
      isNew = true;
      contactId = generateId();
      
      await db.execute(`
        INSERT INTO contact_history (
          id, organization_id, user_id, email, first_name, last_name, billing_cycle_key
        ) VALUES (?, ?, ?, ?, ?, ?, ?)
      `, [
        contactId,
        organizationId,
        userId,
        normalizedEmail,
        firstName || null,
        lastName || null,
        billingCycle
      ]);
    }
    
    // If this is a new contact for this billing cycle, increment the count
    if (isNew) {
      await incrementUniqueContactCount(organizationId);
    }
    
    return { isNew, contactId };
  } catch (error) {
    logger.error(`Error tracking contact: ${error}`);
    throw error;
  }
}

/**
 * Track a batch of contacts
 */
export async function trackContactBatch(
  organizationId: string,
  userId: string,
  contacts: Array<{ email: string; firstName?: string; lastName?: string }>
): Promise<{ newCount: number; totalProcessed: number }> {
  const db = new Database();
  
  try {
    let newCount = 0;
    const totalProcessed = contacts.length;
    
    // Process contacts in batches to avoid transaction timeouts
    const batchSize = 100;
    for (let i = 0; i < contacts.length; i += batchSize) {
      const batch = contacts.slice(i, i + batchSize);
      
      await db.transaction('write', async (tx) => {
        for (const contact of batch) {
          const result = await trackContact(
            organizationId,
            userId,
            contact.email,
            contact.firstName,
            contact.lastName
          );
          
          if (result.isNew) {
            newCount++;
          }
        }
      });
      
      logger.info(`Processed ${i + batch.length}/${totalProcessed} contacts for organization ${organizationId}`);
    }
    
    // Get total unique contacts in current billing cycle
    const uniqueCount = await getUniqueContactCount(organizationId);
    
    // Report usage to Stripe
    await updateStripeUsage(organizationId, uniqueCount);
    
    return { newCount, totalProcessed };
  } catch (error) {
    logger.error(`Error tracking contact batch: ${error}`);
    throw error;
  }
}

/**
 * Increment the unique contact count for an organization
 */
async function incrementUniqueContactCount(organizationId: string): Promise<number> {
  const db = new Database();
  
  try {
    // Get current unique contact count
    const org = await db.fetchOne<{ current_unique_contacts: number }>(
      'SELECT current_unique_contacts FROM organizations WHERE id = ?',
      [organizationId]
    );
    
    if (!org) {
      throw new Error(`Organization not found: ${organizationId}`);
    }
    
    // Increment the count
    const newCount = (org.current_unique_contacts || 0) + 1;
    
    // Update the organization
    await db.execute(`
      UPDATE organizations 
      SET current_unique_contacts = ? 
      WHERE id = ?
    `, [newCount, organizationId]);
    
    // Update the billing cycle history
    const billingCycle = await getCurrentBillingCycle(organizationId);
    await db.execute(`
      UPDATE billing_cycle_history 
      SET contact_count = ? 
      WHERE organization_id = ? AND cycle_key = ?
    `, [newCount, organizationId, billingCycle]);
    
    return newCount;
  } catch (error) {
    logger.error(`Error incrementing unique contact count: ${error}`);
    throw error;
  }
}

/**
 * Get the count of unique contacts in the current billing cycle
 */
export async function getUniqueContactCount(organizationId: string): Promise<number> {
  const db = new Database();
  
  try {
    // Get current billing cycle
    const billingCycle = await getCurrentBillingCycle(organizationId);
    
    // Get unique contact count for this cycle
    const result = await db.fetchOne<{ count: number }>(
      'SELECT COUNT(*) as count FROM contact_history WHERE organization_id = ? AND billing_cycle_key = ?',
      [organizationId, billingCycle]
    );
    
    return result ? result.count : 0;
  } catch (error) {
    logger.error(`Error getting unique contact count: ${error}`);
    throw error;
  }
}

/**
 * Get the subscription info and update Stripe usage
 */
async function updateStripeUsage(organizationId: string, contactCount: number): Promise<void> {
  const db = new Database();
  
  try {
    // Get the organization's Stripe subscription info
    const org = await db.fetchOne<{ 
      id: number;
      stripe_subscription_id: string;
    }>('SELECT id, stripe_subscription_id FROM organizations WHERE id = ?', [organizationId]);
    
    if (!org || !org.stripe_subscription_id) {
      logger.info(`Organization ${organizationId} has no Stripe subscription, skipping usage update`);
      return;
    }
    
    // Get the subscription item for usage reporting
    const subscriptionItem = await db.fetchOne<{ stripe_usage_item_id: string }>(
      'SELECT stripe_usage_item_id FROM subscriptions WHERE stripe_subscription_id = ? AND status = "active" LIMIT 1',
      [org.stripe_subscription_id]
    );
    
    if (!subscriptionItem || !subscriptionItem.stripe_usage_item_id) {
      logger.warn(`No active subscription item found for organization ${organizationId}`);
      return;
    }
    
    // Calculate the number of 500-contact blocks beyond the initial 500
    const additionalBlocks = Math.max(0, Math.ceil((contactCount - 500) / 500));
    
    // Report usage to Stripe
    await reportContactUsage(subscriptionItem.stripe_usage_item_id, additionalBlocks);
    
    logger.info(`Reported usage of ${additionalBlocks} additional contact blocks for organization ${organizationId}`);
    
    // Mark the billing cycle as reported
    const billingCycle = await getCurrentBillingCycle(organizationId);
    await db.execute(`
      UPDATE billing_cycle_history 
      SET usage_reported = TRUE 
      WHERE organization_id = ? AND cycle_key = ?
    `, [organizationId, billingCycle]);
  } catch (error) {
    logger.error(`Error updating Stripe usage: ${error}`);
    throw error;
  }
}

/**
 * Get contact usage stats for an organization
 */
export async function getContactUsageStats(organizationId: string): Promise<{
  currentCycle: string;
  uniqueContacts: number;
  billingCycleStart: string;
  billingCycleEnd: string;
  contactsRemaining: number;
  isOverLimit: boolean;
}> {
  const db = new Database();
  
  try {
    // Get current billing cycle
    const billingCycle = await getCurrentBillingCycle(organizationId);
    
    // Get billing cycle details
    const cycle = await db.fetchOne<{
      start_date: string;
      end_date: string;
      contact_count: number;
    }>(`
      SELECT start_date, end_date, contact_count 
      FROM billing_cycle_history 
      WHERE organization_id = ? AND cycle_key = ?
    `, [organizationId, billingCycle]);
    
    if (!cycle) {
      throw new Error(`Billing cycle not found for organization ${organizationId}`);
    }
    
    // Calculate remaining contacts in the base tier (500)
    const contactsRemaining = Math.max(0, 500 - cycle.contact_count);
    const isOverLimit = cycle.contact_count > 500;
    
    return {
      currentCycle: billingCycle,
      uniqueContacts: cycle.contact_count,
      billingCycleStart: cycle.start_date,
      billingCycleEnd: cycle.end_date,
      contactsRemaining,
      isOverLimit
    };
  } catch (error) {
    logger.error(`Error getting contact usage stats: ${error}`);
    throw error;
  }
}

/**
 * Reset contact count for a specific email (for admin/support use)
 */
export async function resetContactCount(
  organizationId: string,
  email: string,
  reason: string
): Promise<boolean> {
  const db = new Database();
  
  try {
    // Normalize email
    const normalizedEmail = email.trim().toLowerCase();
    
    // Find the contact
    const contact = await db.fetchOne<{ id: string }>(
      'SELECT id FROM contact_history WHERE organization_id = ? AND email = ?',
      [organizationId, normalizedEmail]
    );
    
    if (!contact) {
      logger.warn(`Contact not found for reset: ${email} in organization ${organizationId}`);
      return false;
    }
    
    // Set the status to deleted
    await db.execute(`
      UPDATE contact_history 
      SET status = 'deleted' 
      WHERE id = ?
    `, [contact.id]);
    
    // Log the operation for audit purposes
    await db.execute(`
      INSERT INTO admin_action_log (
        id, organization_id, action, details, reason
      ) VALUES (?, ?, ?, ?, ?)
    `, [
      generateId(),
      organizationId,
      'reset_contact',
      JSON.stringify({ email: normalizedEmail }),
      reason
    ]);
    
    // Recalculate the contact count
    await recalculateContactCount(organizationId);
    
    return true;
  } catch (error) {
    logger.error(`Error resetting contact count: ${error}`);
    throw error;
  }
}

/**
 * Recalculate contact count for an organization
 */
async function recalculateContactCount(organizationId: string): Promise<number> {
  const db = new Database();
  
  try {
    // Get current billing cycle
    const billingCycle = await getCurrentBillingCycle(organizationId);
    
    // Count active contacts in current billing cycle
    const result = await db.fetchOne<{ count: number }>(
      'SELECT COUNT(*) as count FROM contact_history WHERE organization_id = ? AND billing_cycle_key = ? AND status = "active"',
      [organizationId, billingCycle]
    );
    
    const count = result ? result.count : 0;
    
    // Update the organization
    await db.execute(`
      UPDATE organizations 
      SET current_unique_contacts = ? 
      WHERE id = ?
    `, [count, organizationId]);
    
    // Update the billing cycle history
    await db.execute(`
      UPDATE billing_cycle_history 
      SET contact_count = ? 
      WHERE organization_id = ? AND cycle_key = ?
    `, [count, organizationId, billingCycle]);
    
    // Update Stripe usage
    await updateStripeUsage(organizationId, count);
    
    return count;
  } catch (error) {
    logger.error(`Error recalculating contact count: ${error}`);
    throw error;
  }
} 