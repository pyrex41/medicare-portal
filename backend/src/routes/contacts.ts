import { Elysia, t } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import type { ContactCreate } from '../types';
import fs from 'fs/promises';
import path from 'path';
import os from 'os';

type User = {
  id: number;
  organization_id: number;
  is_admin: boolean;
};

type Store = {
  user: User;
};

type Context = {
  request: Request;
  store: Store;
  set: { status: number };
  body: { overwriteExisting?: boolean };
  params: { orgId: string };
};

/**
 * Contacts API endpoints
 */
export const contactsRoutes = new Elysia({ prefix: '/api/contacts' })
  .guard({ beforeHandle: [Database.getUserFromSession] }, app => app
    
    // Bulk import contacts from CSV
    .post('/bulk-import',
      {
        body: t.Object({
          overwriteExisting: t.Optional(t.Boolean())
        })
      },
      async ({ request, store, set, body }: Context) => {
        const user = store.user;
        if (!user || !user.organization_id || !user.is_admin) {
          set.status = 401;
          return { error: 'Not authorized for bulk import' };
        }

        try {
          const formData = await request.formData();
          const file = formData.get('file');
          if (!file || !(file instanceof File) || path.extname(file.name).toLowerCase() !== '.csv') {
            set.status = 400;
            return { error: 'File must be a CSV' };
          }

          const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'import-'));
          const tempFilePath = path.join(tempDir, 'contacts.csv');
          await fs.writeFile(tempFilePath, Buffer.from(await file.arrayBuffer()));

          logger.info(`CSV file saved to ${tempFilePath}, starting import`);

          try {
            await Database.bulkImportContacts(
              user.organization_id.toString(), 
              tempFilePath, 
              body.overwriteExisting || false
            );

            await fs.rm(tempDir, { recursive: true, force: true });

            return { 
              success: true, 
              message: 'Contacts imported successfully',
              error_csv: null,
              converted_carriers_csv: null,
              total_rows: 0,
              error_rows: 0,
              valid_rows: 0,
              converted_carrier_rows: 0,
              supported_carriers: []
            };
          } catch (error) {
            await fs.rm(tempDir, { recursive: true, force: true }).catch(() => {});
            throw error;
          }
        } catch (error) {
          const err = error as Error;
          logger.error(`Error in bulk import: ${err}`);
          set.status = 500;
          return { error: 'Failed to process import: ' + err.message };
        }
      }
    )
    
    // Add route to delete all contacts
    .delete('/all',
      async ({ store, set }: Context) => {
        const user = store.user;
        if (!user || !user.organization_id || !user.is_admin) {
          set.status = 401;
          return { error: 'Not authorized to delete all contacts' };
        }

        try {
          const orgDb = await Database.getOrgDb(user.organization_id.toString());
          await orgDb.execute('DELETE FROM contacts');
          return { success: true, message: 'All contacts deleted successfully' };
        } catch (error) {
          const err = error as Error;
          logger.error(`Error deleting all contacts: ${err}`);
          set.status = 500;
          return { error: 'Failed to delete contacts: ' + err.message };
        }
      }
    )
    
    // Add route to check import status
    .get('/import-status/:orgId', async ({ params, store, set }: Context) => {
      const user = store.user;
      
      if (!user || !user.organization_id) {
        set.status = 401;
        return { error: 'Not authorized' };
      }
      
      // Only admins can check import status for any org
      // Regular users can only check their own org's status
      if (Number(params.orgId) !== user.organization_id && !user.is_admin) {
        set.status = 403;
        return { error: 'Not authorized to view this organization' };
      }
      
      try {
        // Look for a backup file to indicate an import is in progress
        const backupDir = path.join(process.cwd(), 'backups');
        const files = await fs.readdir(backupDir);
        
        // Filter files for this organization
        const orgBackups = files.filter(file => file.startsWith(`org-${params.orgId}-`));
        
        if (orgBackups.length === 0) {
          return { 
            status: 'none',
            message: 'No import in progress or recently completed' 
          };
        }
        
        // Sort by creation time (most recent first)
        const statPromises = orgBackups.map(async (file) => {
          const stats = await fs.stat(path.join(backupDir, file));
          return { file, stats };
        });
        
        const fileStats = await Promise.all(statPromises);
        fileStats.sort((a, b) => b.stats.mtimeMs - a.stats.mtimeMs);
        
        const latestBackup = fileStats[0];
        const ageInMinutes = (Date.now() - latestBackup.stats.mtimeMs) / 60000;
        
        // If backup file is less than 30 minutes old, consider it active
        if (ageInMinutes < 30) {
          const timestamp = latestBackup.file.split('-').pop()?.split('.')[0] || '';
          
          // Check logs for progress info
          let progress = "unknown";
          try {
            const logs = await fs.readFile(path.join(process.cwd(), 'logs', 'ai-interactions.log'), 'utf-8');
            const progressLines = logs.split('\n').filter(line => 
              line.includes(`org-${params.orgId}-${timestamp}`) && line.includes('contacts')
            );
            
            if (progressLines.length > 0) {
              const lastLine = progressLines[progressLines.length - 1];
              if (lastLine.includes('completed successfully')) {
                progress = "completed";
              } else {
                progress = "in progress";
              }
            }
          } catch (error) {
            // Can't read logs, continue with unknown status
          }
          
          return {
            status: progress,
            message: `Import ${progress === "completed" ? "completed" : "in progress"}`,
            timestamp: new Date(parseInt(timestamp)).toISOString(),
            age: Math.round(ageInMinutes)
          };
        } else {
          return {
            status: 'completed',
            message: 'Import completed',
            timestamp: new Date(latestBackup.stats.mtimeMs).toISOString(),
            age: Math.round(ageInMinutes)
          };
        }
      } catch (error) {
        const err = error as Error;
        logger.error(`Error checking import status: ${err}`);
        return { 
          status: 'error',
          message: `Error checking import status: ${err.message}`
        };
      }
    })
  );