import { Client } from '@replit/object-storage';
import { logger } from '../utils/logger.js';

/**
 * Unified Replit Object Storage manager for both locking and replica operations
 * Provides a clean interface over the Replit Object Storage SDK
 */
export class ReplitStorageManager {
  private client: Client;
  private static instance: ReplitStorageManager;

  private constructor() {
    this.client = new Client();
  }

  public static getInstance(): ReplitStorageManager {
    if (!ReplitStorageManager.instance) {
      ReplitStorageManager.instance = new ReplitStorageManager();
    }
    return ReplitStorageManager.instance;
  }

  // === LOCKING OPERATIONS ===

  /**
   * Acquire a lock for exclusive operations
   */
  async acquireLock(lockKey: string, ttlMs: number = 30000): Promise<boolean> {
    const lockPath = `locks/${lockKey}`;
    
    try {
      // Check if lock already exists
      const existsResult = await this.client.exists(lockPath);
      if (!existsResult.ok) {
        logger.error(`Failed to check lock existence: ${existsResult.error}`);
        return false;
      }

      if (existsResult.value) {
        // Lock exists, check if it's expired
        const lockDataResult = await this.client.downloadAsText(lockPath);
        if (lockDataResult.ok) {
          const lockData = JSON.parse(lockDataResult.value);
          if (Date.now() < lockData.expiresAt) {
            logger.info(`Lock ${lockKey} is still active`);
            return false; // Lock is still valid
          }
        }
      }

      // Create/update lock
      const lockData = {
        acquiredAt: Date.now(),
        expiresAt: Date.now() + ttlMs,
        lockKey
      };

      const uploadResult = await this.client.uploadFromText(lockPath, JSON.stringify(lockData));
      if (!uploadResult.ok) {
        logger.error(`Failed to acquire lock: ${uploadResult.error}`);
        return false;
      }

      logger.info(`Lock ${lockKey} acquired successfully`);
      return true;
    } catch (error) {
      logger.error(`Error acquiring lock ${lockKey}:`, error);
      return false;
    }
  }

  /**
   * Release a lock
   */
  async releaseLock(lockKey: string): Promise<boolean> {
    const lockPath = `locks/${lockKey}`;
    
    try {
      const deleteResult = await this.client.delete(lockPath);
      if (!deleteResult.ok) {
        logger.error(`Failed to release lock: ${deleteResult.error}`);
        return false;
      }

      logger.info(`Lock ${lockKey} released successfully`);
      return true;
    } catch (error) {
      logger.error(`Error releasing lock ${lockKey}:`, error);
      return false;
    }
  }

  // === REPLICA MANAGEMENT ===

  /**
   * Check if a replica exists for an organization
   */
  async hasReplica(orgId: string): Promise<boolean> {
    const replicaPath = `litestream-replicas/${orgId}/`;
    
    try {
      const listResult = await this.client.list({ prefix: replicaPath });
      if (!listResult.ok) {
        logger.error(`Failed to check replica existence: ${listResult.error}`);
        return false;
      }

      return listResult.value.length > 0;
    } catch (error) {
      logger.error(`Error checking replica for org ${orgId}:`, error);
      return false;
    }
  }

  /**
   * List all organization replicas
   */
  async listOrganizations(): Promise<string[]> {
    try {
      const listResult = await this.client.list({ prefix: 'litestream-replicas/' });
      if (!listResult.ok) {
        logger.error(`Failed to list organizations: ${listResult.error}`);
        return [];
      }

      // Extract org IDs from paths like "litestream-replicas/org-123/..."
      const orgIds = new Set<string>();
      for (const object of listResult.value) {
        const pathParts = object.name.split('/');
        if (pathParts.length >= 2 && pathParts[0] === 'litestream-replicas') {
          orgIds.add(pathParts[1]);
        }
      }

      return Array.from(orgIds);
    } catch (error) {
      logger.error('Error listing organizations:', error);
      return [];
    }
  }

  /**
   * Get replica metadata and size for an organization
   */
  async getReplicaInfo(orgId: string): Promise<{
    exists: boolean;
    size: number;
    lastModified?: Date;
    fileCount: number;
  }> {
    const replicaPath = `litestream-replicas/${orgId}/`;
    
    try {
      const listResult = await this.client.list({ prefix: replicaPath });
      if (!listResult.ok) {
        return { exists: false, size: 0, fileCount: 0 };
      }

      const files = listResult.value;
      const totalSize = files.reduce((sum, file) => sum + (file.size || 0), 0);
      const latestFile = files.reduce((latest, file) => {
        if (!latest || !latest.lastModified || !file.lastModified) return latest || file;
        return new Date(file.lastModified) > new Date(latest.lastModified) ? file : latest;
      }, files[0]);

      return {
        exists: files.length > 0,
        size: totalSize,
        fileCount: files.length,
        lastModified: latestFile?.lastModified ? new Date(latestFile.lastModified) : undefined
      };
    } catch (error) {
      logger.error(`Error getting replica info for org ${orgId}:`, error);
      return { exists: false, size: 0, fileCount: 0 };
    }
  }

  /**
   * Clean up old locks (utility method)
   */
  async cleanupExpiredLocks(): Promise<number> {
    try {
      const listResult = await this.client.list({ prefix: 'locks/' });
      if (!listResult.ok) {
        logger.error(`Failed to list locks for cleanup: ${listResult.error}`);
        return 0;
      }

      let cleanedCount = 0;
      const now = Date.now();

      for (const lockObject of listResult.value) {
        try {
          const lockDataResult = await this.client.downloadAsText(lockObject.name);
          if (lockDataResult.ok) {
            const lockData = JSON.parse(lockDataResult.value);
            if (now > lockData.expiresAt) {
              const deleteResult = await this.client.delete(lockObject.name);
              if (deleteResult.ok) {
                cleanedCount++;
                logger.info(`Cleaned up expired lock: ${lockObject.name}`);
              }
            }
          }
        } catch (error) {
          logger.error(`Error processing lock ${lockObject.name} during cleanup:`, error);
        }
      }

      if (cleanedCount > 0) {
        logger.info(`Cleaned up ${cleanedCount} expired locks`);
      }

      return cleanedCount;
    } catch (error) {
      logger.error('Error during lock cleanup:', error);
      return 0;
    }
  }

  // === UTILITY METHODS ===

  /**
   * Get the underlying Replit Object Storage client for advanced operations
   */
  getClient(): Client {
    return this.client;
  }

  /**
   * Test connectivity and permissions
   */
  async testConnection(): Promise<{ success: boolean; error?: string }> {
    try {
      const testKey = `health-check/${Date.now()}`;
      const testData = JSON.stringify({ test: true, timestamp: Date.now() });

      // Test upload
      const uploadResult = await this.client.uploadFromText(testKey, testData);
      if (!uploadResult.ok) {
        return { success: false, error: `Upload failed: ${uploadResult.error}` };
      }

      // Test download
      const downloadResult = await this.client.downloadAsText(testKey);
      if (!downloadResult.ok) {
        return { success: false, error: `Download failed: ${downloadResult.error}` };
      }

      // Test delete
      const deleteResult = await this.client.delete(testKey);
      if (!deleteResult.ok) {
        return { success: false, error: `Delete failed: ${deleteResult.error}` };
      }

      return { success: true };
    } catch (error) {
      return { success: false, error: `Connection test failed: ${error}` };
    }
  }
}