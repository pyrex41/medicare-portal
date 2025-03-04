import { Elysia } from 'elysia';
import { getUserFromSession } from '../services/auth';
import { logger } from '../logger';
import { Database } from '../database';

/**
 * Middleware to check if an organization's account is in good standing
 * This can be used on routes that should be blocked if the account has issues
 */
export const accountStatusMiddleware = new Elysia()
  .derive(async ({ request, set }) => {
    try {
      // Get current user from session
      const currentUser = await getUserFromSession(request);
      if (!currentUser) {
        // If no user, let the auth middleware handle it
        return { accountStatus: null };
      }
      
      const db = new Database();
      
      // Query organization status from the view
      const statusResult = await db.query<{
        account_status: string;
        subscription_status: string;
      }>('SELECT account_status, subscription_status FROM organization_status WHERE id = ?', 
         [currentUser.organization_id]);
      
      if (!statusResult || statusResult.length === 0) {
        logger.error(`Could not find organization status for org ID: ${currentUser.organization_id}`);
        return { accountStatus: null };
      }
      
      const status = statusResult[0];
      
      // If account is not in good standing, block access to certain operations
      if (status.account_status !== 'good_standing') {
        logger.warn(`Blocked access due to account status: ${status.account_status} for org ${currentUser.organization_id}`);
        return { accountStatus: status.account_status, subscription_status: status.subscription_status };
      }
      
      // Account is in good standing
      return { accountStatus: 'good_standing' };
      
    } catch (error) {
      logger.error(`Error in account status middleware:`, error);
      return { accountStatus: null };
    }
  });