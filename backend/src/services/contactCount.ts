import { Database } from '../database';
import { logger } from '../logger';
import { checkSubscriptionTier, updateSubscriptionTier } from './stripe';

/**
 * Updates the contact count for a user and handles subscription tier changes if needed
 */
export async function updateContactCount(userId: string, totalContacts: number): Promise<{
  success: boolean;
  currentTier: number;
  newTier?: number;
  tierUpgraded?: boolean;
  error?: string;
}> {
  const db = new Database();
  
  try {
    // Check if we need to upgrade the subscription
    const { currentTier, requiredTier, needsUpgrade } = await checkSubscriptionTier(userId, totalContacts);
    
    // Update the contact count in the database
    await upsertContactCount(userId, totalContacts);
    
    // If no upgrade needed, just return success
    if (!needsUpgrade) {
      return {
        success: true,
        currentTier,
      };
    }
    
    // Check if auto-upgrade is enabled
    const user = await db.fetchOne<{ auto_upgrade_limit: number }>(
      'SELECT auto_upgrade_limit FROM users WHERE id = ?',
      [userId]
    );
    
    if (!user) {
      return {
        success: false,
        currentTier,
        error: 'User not found',
      };
    }
    
    // Check if the required tier exceeds the auto-upgrade limit
    if (user.auto_upgrade_limit > 0 && requiredTier * 500 <= user.auto_upgrade_limit) {
      // Auto-upgrade is allowed
      const subscription = await db.fetchOne<{
        stripe_subscription_id: string;
        stripe_subscription_item_id: string;
      }>(
        'SELECT stripe_subscription_id, stripe_subscription_item_id FROM subscriptions WHERE user_id = ? AND status = "active" ORDER BY created_at DESC LIMIT 1',
        [userId]
      );
      
      if (!subscription) {
        return {
          success: false,
          currentTier,
          error: 'No active subscription found',
        };
      }
      
      // Upgrade the subscription
      try {
        await updateSubscriptionTier(
          subscription.stripe_subscription_id,
          subscription.stripe_subscription_item_id,
          requiredTier
        );
        
        // Update the subscription in our database
        await db.execute(
          'UPDATE subscriptions SET tier = ?, updated_at = CURRENT_TIMESTAMP WHERE stripe_subscription_id = ?',
          [requiredTier, subscription.stripe_subscription_id]
        );
        
        logger.info(`Auto-upgraded subscription for user ${userId} from tier ${currentTier} to ${requiredTier}`);
        
        return {
          success: true,
          currentTier,
          newTier: requiredTier,
          tierUpgraded: true,
        };
      } catch (error) {
        logger.error(`Error auto-upgrading subscription for user ${userId}:`, error);
        return {
          success: false,
          currentTier,
          error: 'Failed to auto-upgrade subscription',
        };
      }
    } else {
      // Manual upgrade required
      return {
        success: false,
        currentTier,
        error: 'Contact limit exceeded. Please upgrade your subscription.',
      };
    }
  } catch (error) {
    logger.error(`Error updating contact count for user ${userId}:`, error);
    return {
      success: false,
      currentTier: 0,
      error: 'Failed to update contact count',
    };
  }
}

/**
 * Updates or inserts a contact count record for a user
 */
export async function upsertContactCount(userId: string, count: number): Promise<void> {
  const db = new Database();
  
  try {
    // Check if a record already exists
    const existingCount = await db.fetchOne<{ count: number }>(
      'SELECT count FROM contact_counts WHERE user_id = ?',
      [userId]
    );
    
    if (existingCount) {
      // Update existing record
      await db.execute(
        'UPDATE contact_counts SET count = ?, last_updated = CURRENT_TIMESTAMP WHERE user_id = ?',
        [count, userId]
      );
    } else {
      // Insert new record
      await db.execute(
        'INSERT INTO contact_counts (user_id, count, last_updated) VALUES (?, ?, CURRENT_TIMESTAMP)',
        [userId, count]
      );
    }
    
    logger.info(`Updated contact count for user ${userId} to ${count}`);
  } catch (error) {
    logger.error(`Error upserting contact count for user ${userId}:`, error);
    throw error;
  }
}

/**
 * Gets the current contact count for a user
 */
export async function getContactCount(userId: string): Promise<number> {
  const db = new Database();
  
  try {
    const countRecord = await db.fetchOne<{ count: number }>(
      'SELECT count FROM contact_counts WHERE user_id = ?',
      [userId]
    );
    
    return countRecord?.count || 0;
  } catch (error) {
    logger.error(`Error getting contact count for user ${userId}:`, error);
    throw error;
  }
}

/**
 * Gets the current subscription tier limit for a user
 */
export async function getContactLimit(userId: string): Promise<number> {
  const db = new Database();
  
  try {
    const subscription = await db.fetchOne<{ tier: number }>(
      'SELECT tier FROM subscriptions WHERE user_id = ? AND status = "active" ORDER BY created_at DESC LIMIT 1',
      [userId]
    );
    
    // Each tier represents 500 contacts
    return subscription ? subscription.tier * 500 : 0;
  } catch (error) {
    logger.error(`Error getting contact limit for user ${userId}:`, error);
    throw error;
  }
}

/**
 * Updates the auto-upgrade limit for a user
 */
export async function updateAutoUpgradeLimit(userId: string, limit: number): Promise<boolean> {
  const db = new Database();
  
  try {
    await db.execute(
      'UPDATE users SET auto_upgrade_limit = ? WHERE id = ?',
      [limit, userId]
    );
    
    logger.info(`Updated auto-upgrade limit for user ${userId} to ${limit}`);
    return true;
  } catch (error) {
    logger.error(`Error updating auto-upgrade limit for user ${userId}:`, error);
    return false;
  }
}

/**
 * Checks if adding a specified number of contacts would exceed the user's current tier
 */
export async function checkContactAddition(userId: string, additionalContacts: number): Promise<{
  canAdd: boolean;
  currentCount: number;
  limit: number;
  newTotal: number;
  requiredTier: number;
  currentTier: number;
}> {
  const db = new Database();
  
  try {
    // Get the current count
    const currentCount = await getContactCount(userId);
    
    // Get the current tier
    const subscription = await db.fetchOne<{ tier: number }>(
      'SELECT tier FROM subscriptions WHERE user_id = ? AND status = "active" ORDER BY created_at DESC LIMIT 1',
      [userId]
    );
    
    const currentTier = subscription?.tier || 0;
    const limit = currentTier * 500;
    const newTotal = currentCount + additionalContacts;
    const requiredTier = Math.ceil(newTotal / 500);
    
    return {
      canAdd: newTotal <= limit,
      currentCount,
      limit,
      newTotal,
      requiredTier,
      currentTier,
    };
  } catch (error) {
    logger.error(`Error checking contact addition for user ${userId}:`, error);
    throw error;
  }
} 