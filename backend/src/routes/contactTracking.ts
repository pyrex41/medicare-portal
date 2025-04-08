import { Elysia, t } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import { getUserFromSession } from '../services/auth';
import { 
  trackContact, 
  trackContactBatch, 
  getContactUsageStats, 
  getUniqueContactCount,
  resetContactCount
} from '../services/contactTracking';

export const createContactTrackingRoutes = () => {
  return new Elysia({ prefix: '/api/contact-tracking' })

    // Track a single contact
    .post('/track', 
      async ({ request, body, set }) => {
        try {
          const user = await getUserFromSession(request);
          
          // Check if request includes required fields
          if (!user || !body) {
            set.status = 400;
            return { 
              success: false, 
              error: 'Invalid request' 
            };
          }
          
          const { email, firstName, lastName } = body as {
            email: string;
            firstName?: string;
            lastName?: string;
          };
          
          if (!email) {
            set.status = 400;
            return { 
              success: false, 
              error: 'Email is required' 
            };
          }
          
          // Track the contact
          const result = await trackContact(
            user.organization_id.toString(),
            user.id.toString(),
            email,
            firstName,
            lastName
          );
          
          // Get updated usage stats
          const stats = await getContactUsageStats(user.organization_id.toString());
          
          return {
            success: true,
            isNew: result.isNew,
            contactId: result.contactId,
            stats
          };
        } catch (error) {
          logger.error(`Error tracking contact: ${error}`);
          set.status = 500;
          return { 
            success: false, 
            error: 'Failed to track contact' 
          };
        }
      },
      {
        body: t.Object({
          email: t.String(),
          firstName: t.Optional(t.String()),
          lastName: t.Optional(t.String())
        })
      }
    )
    
    // Track a batch of contacts
    .post('/batch', 
      async ({ request, body, set }) => {
        try {
          const user = await getUserFromSession(request);
          
          // Check if request includes required fields
          if (!user || !body || !body.contacts || !Array.isArray(body.contacts)) {
            set.status = 400;
            return { 
              success: false, 
              error: 'Invalid request' 
            };
          }
          
          const { contacts } = body as {
            contacts: Array<{
              email: string;
              firstName?: string;
              lastName?: string;
            }>
          };
          
          // Track the contacts in batch
          const result = await trackContactBatch(
            user.organization_id.toString(),
            user.id.toString(),
            contacts
          );
          
          // Get updated usage stats
          const stats = await getContactUsageStats(user.organization_id.toString());
          
          return {
            success: true,
            newCount: result.newCount,
            totalProcessed: result.totalProcessed,
            stats
          };
        } catch (error) {
          logger.error(`Error tracking contact batch: ${error}`);
          set.status = 500;
          return { 
            success: false, 
            error: 'Failed to track contacts' 
          };
        }
      },
      {
        body: t.Object({
          contacts: t.Array(
            t.Object({
              email: t.String(),
              firstName: t.Optional(t.String()),
              lastName: t.Optional(t.String())
            })
          )
        })
      }
    )
    
    // Get contact usage stats
    .get('/usage-stats', async ({ request, set }) => {
      try {
        const user = await getUserFromSession(request);
        
        if (!user) {
          set.status = 401;
          return { 
            success: false, 
            error: 'Unauthorized' 
          };
        }
        
        const stats = await getContactUsageStats(user.organization_id.toString());
        
        return {
          success: true,
          stats
        };
      } catch (error) {
        logger.error(`Error getting contact usage stats: ${error}`);
        set.status = 500;
        return { 
          success: false, 
          error: 'Failed to get contact usage stats' 
        };
      }
    })
    
    // Reset contact count (admin/support only)
    .post('/reset', 
      async ({ request, body, set }) => {
        try {
          const user = await getUserFromSession(request);
          
          // Check if user is admin
          if (!user || !user.is_admin) {
            set.status = 403;
            return { 
              success: false, 
              error: 'You do not have permission to perform this action' 
            };
          }
          
          const { email, reason } = body as {
            email: string;
            reason: string;
          };
          
          if (!email || !reason) {
            set.status = 400;
            return { 
              success: false, 
              error: 'Email and reason are required' 
            };
          }
          
          // Reset the contact
          const success = await resetContactCount(
            user.organization_id.toString(),
            email,
            reason
          );
          
          if (!success) {
            set.status = 404;
            return { 
              success: false, 
              error: 'Contact not found' 
            };
          }
          
          // Get updated stats
          const stats = await getContactUsageStats(user.organization_id.toString());
          
          return {
            success: true,
            stats
          };
        } catch (error) {
          logger.error(`Error resetting contact: ${error}`);
          set.status = 500;
          return { 
            success: false, 
            error: 'Failed to reset contact' 
          };
        }
      },
      {
        body: t.Object({
          email: t.String(),
          reason: t.String()
        })
      }
    );
}; 