import { Client } from '@replit/object-storage';
import { logger } from '../logger';

export class LockingService {
  private client: Client;
  private readonly LOCK_TIMEOUT = 10 * 60 * 1000; // 10 minutes

  constructor() {
    this.client = new Client();
  }

  /**
   * Acquires a lock for the given organization ID
   * Returns true if lock was acquired, false if already locked
   */
  async acquireLock(orgId: string): Promise<boolean> {
    const lockKey = `locks/${orgId}.lock`;
    const lockData = JSON.stringify({
      orgId,
      timestamp: Date.now(),
      timeout: this.LOCK_TIMEOUT
    });

    try {
      // Check if lock already exists
      const existsResult = await this.client.exists(lockKey);
      if (!existsResult.ok) {
        logger.error(`[Lock] Error checking lock existence for org ${orgId}: ${existsResult.error}`);
        return false;
      }

      if (existsResult.value) {
        // Lock exists, check if it's expired
        const downloadResult = await this.client.downloadAsText(lockKey);
        if (!downloadResult.ok) {
          logger.error(`[Lock] Error downloading lock for org ${orgId}: ${downloadResult.error}`);
          return false;
        }

        try {
          const existingLock = JSON.parse(downloadResult.value);
          const lockAge = Date.now() - existingLock.timestamp;
          
          if (lockAge < this.LOCK_TIMEOUT) {
            logger.info(`[Lock] Lock already exists for org ${orgId}, age: ${Math.round(lockAge / 1000)}s`);
            return false; // Lock is still valid
          }
          
          logger.warn(`[Lock] Expired lock found for org ${orgId}, age: ${Math.round(lockAge / 1000)}s, removing...`);
          // Continue to create new lock (will overwrite)
        } catch (parseError) {
          logger.error(`[Lock] Error parsing existing lock for org ${orgId}: ${parseError}`);
          return false;
        }
      }

      // Create the lock
      const uploadResult = await this.client.uploadFromText(lockKey, lockData);
      if (!uploadResult.ok) {
        logger.error(`[Lock] Failed to create lock for org ${orgId}: ${uploadResult.error}`);
        return false;
      }

      logger.info(`[Lock] Successfully acquired lock for org ${orgId}`);
      return true;
    } catch (error) {
      logger.error(`[Lock] Error acquiring lock for org ${orgId}: ${error instanceof Error ? error.message : String(error)}`);
      return false;
    }
  }

  /**
   * Releases the lock for the given organization ID
   */
  async releaseLock(orgId: string): Promise<void> {
    const lockKey = `locks/${orgId}.lock`;

    try {
      const deleteResult = await this.client.delete(lockKey);
      if (!deleteResult.ok) {
        logger.error(`[Lock] Failed to release lock for org ${orgId}: ${deleteResult.error}`);
      } else {
        logger.info(`[Lock] Successfully released lock for org ${orgId}`);
      }
    } catch (error) {
      logger.error(`[Lock] Error releasing lock for org ${orgId}: ${error instanceof Error ? error.message : String(error)}`);
    }
  }

  /**
   * Checks if a lock exists for the given organization ID
   */
  async isLocked(orgId: string): Promise<boolean> {
    const lockKey = `locks/${orgId}.lock`;

    try {
      const existsResult = await this.client.exists(lockKey);
      if (!existsResult.ok) {
        logger.error(`[Lock] Error checking lock for org ${orgId}: ${existsResult.error}`);
        return false;
      }

      if (!existsResult.value) {
        return false;
      }

      // Check if lock is expired
      const downloadResult = await this.client.downloadAsText(lockKey);
      if (!downloadResult.ok) {
        logger.error(`[Lock] Error downloading lock for org ${orgId}: ${downloadResult.error}`);
        return false;
      }

      try {
        const lockData = JSON.parse(downloadResult.value);
        const lockAge = Date.now() - lockData.timestamp;
        return lockAge < this.LOCK_TIMEOUT;
      } catch (parseError) {
        logger.error(`[Lock] Error parsing lock data for org ${orgId}: ${parseError}`);
        return false;
      }
    } catch (error) {
      logger.error(`[Lock] Error checking if locked for org ${orgId}: ${error instanceof Error ? error.message : String(error)}`);
      return false;
    }
  }
}

// Singleton instance
export const lockingService = new LockingService();