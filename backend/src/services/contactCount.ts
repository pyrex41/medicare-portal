import { Database } from '../database';
import { logger } from '../logger';

/**
 * Updates the contact count for a user and handles subscription tier changes if needed
 */
export async function updateContactCount(userId: string, totalContacts: number): Promise<{
  success: boolean;
  error?: string;
}> {
  const db = new Database();
  
  try {
   
    // Update the contact count in the database
    await upsertContactCount(userId, totalContacts);

    // Check if auto-upgrade is enabled
    const user = await db.fetchOne<{ auto_upgrade_limit: number }>(
      'SELECT user_id FROM users WHERE id = ?',
      [userId]
    );
    
    if (!user) {
      return {
        success: false,
        error: 'User not found',
      };
    }
  } catch (error) {
    logger.error(`Error updating contact count for user ${userId}: ${error}`);
    return {
      success: false,
      error: 'Failed to update contact count',
    };
  }
  
  // Return success if no errors occurred
  return {
    success: true
  };
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