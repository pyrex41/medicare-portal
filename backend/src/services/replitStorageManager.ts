import { Client } from '@replit/object-storage';
import { logger } from '../logger';

/**
 * Unified Replit Object Storage manager for replica management operations
 * Uses the Replit SDK for managed auth and simplified operations
 * Note: For atomic locking operations, we still use the GCS SDK in lockingService.ts
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
      logger.error(`Error checking replica for org ${orgId}: ${error instanceof Error ? error.message : String(error)}`);
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
      logger.error(`Error listing organizations: ${error instanceof Error ? error.message : String(error)}`);
      return [];
    }
  }

  /**
   * Get replica metadata and file count for an organization
   * Note: Replit SDK doesn't expose size/lastModified consistently, 
   * so we focus on file count and existence
   */
  async getReplicaInfo(orgId: string): Promise<{
    exists: boolean;
    fileCount: number;
    hasSnapshots: boolean;
    hasWalFiles: boolean;
  }> {
    const replicaPath = `litestream-replicas/${orgId}/`;
    
    try {
      const listResult = await this.client.list({ prefix: replicaPath });
      if (!listResult.ok) {
        return { exists: false, fileCount: 0, hasSnapshots: false, hasWalFiles: false };
      }

      const files = listResult.value;
      const hasSnapshots = files.some(f => f.name.includes('/snapshots/'));
      const hasWalFiles = files.some(f => f.name.includes('/wal/'));

      return {
        exists: files.length > 0,
        fileCount: files.length,
        hasSnapshots,
        hasWalFiles
      };
    } catch (error) {
      logger.error(`Error getting replica info for org ${orgId}: ${error instanceof Error ? error.message : String(error)}`);
      return { exists: false, fileCount: 0, hasSnapshots: false, hasWalFiles: false };
    }
  }

  /**
   * Clean up orphaned replica directories (utility method)
   */
  async cleanupOrphanedReplicas(validOrgIds: string[]): Promise<number> {
    try {
      const allOrgs = await this.listOrganizations();
      const orphanedOrgs = allOrgs.filter(orgId => !validOrgIds.includes(orgId));
      
      let cleanedCount = 0;
      for (const orgId of orphanedOrgs) {
        try {
          const replicaPath = `litestream-replicas/${orgId}/`;
          const listResult = await this.client.list({ prefix: replicaPath });
          
          if (listResult.ok) {
            for (const object of listResult.value) {
              const deleteResult = await this.client.delete(object.name);
              if (deleteResult.ok) {
                cleanedCount++;
              }
            }
            logger.info(`Cleaned up orphaned replica for org: ${orgId}`);
          }
        } catch (error) {
          logger.error(`Error cleaning up replica for org ${orgId}: ${error instanceof Error ? error.message : String(error)}`);
        }
      }

      if (cleanedCount > 0) {
        logger.info(`Cleaned up ${cleanedCount} orphaned replica files`);
      }

      return cleanedCount;
    } catch (error) {
      logger.error(`Error during replica cleanup: ${error instanceof Error ? error.message : String(error)}`);
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
   * Test connectivity and permissions using Replit's managed auth
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

  /**
   * Get replica size estimate by listing files
   * (Since size isn't reliably exposed, we estimate based on file count)
   */
  async getReplicaSizeEstimate(orgId: string): Promise<{
    totalFiles: number;
    snapshotFiles: number;
    walFiles: number;
    sizeCategory: 'small' | 'medium' | 'large';
  }> {
    const replicaPath = `litestream-replicas/${orgId}/`;
    
    try {
      const listResult = await this.client.list({ prefix: replicaPath });
      if (!listResult.ok) {
        return { totalFiles: 0, snapshotFiles: 0, walFiles: 0, sizeCategory: 'small' };
      }

      const files = listResult.value;
      const snapshotFiles = files.filter(f => f.name.includes('/snapshots/')).length;
      const walFiles = files.filter(f => f.name.includes('/wal/')).length;
      const totalFiles = files.length;

      // Rough categorization based on file count
      let sizeCategory: 'small' | 'medium' | 'large' = 'small';
      if (totalFiles > 100) sizeCategory = 'large';
      else if (totalFiles > 20) sizeCategory = 'medium';

      return {
        totalFiles,
        snapshotFiles, 
        walFiles,
        sizeCategory
      };
    } catch (error) {
      logger.error(`Error estimating replica size for org ${orgId}: ${error instanceof Error ? error.message : String(error)}`);
      return { totalFiles: 0, snapshotFiles: 0, walFiles: 0, sizeCategory: 'small' };
    }
  }
}