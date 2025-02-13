import { Elysia } from 'elysia';
import { cookie } from '@elysiajs/cookie';
import { AuthService } from '../services/auth';
import { EmailService } from '../services/email';
import { logger } from '../logger';
import { randomBytes } from 'crypto';
import { config } from '../config';
import crypto from 'crypto';
import { db } from '../database';

export function createAuthRoutes() {
  const auth = new AuthService(
    process.env.NODE_ENV === 'development' 
      ? 'http://localhost:5173'  // Frontend URL in development
      : (process.env.PUBLIC_URL || 'http://localhost:3000')
  );
  const email = new EmailService();

  return new Elysia()
    .use(cookie())
    .post('/api/auth/login', async ({ body }) => {
      const { email: userEmail } = body as { email: string };

      try {
        // Look up the user and their organization
        const userOrg = await db.fetchOne<{
          userId: number,
          orgSlug: string,
          isActive: number  // Change this to number since SQLite uses 0/1
        }>(
          `SELECT 
            u.id as userId,
            o.slug as orgSlug,
            u.is_active as isActive
          FROM users u
          JOIN organizations o ON u.organization_id = o.id
          WHERE LOWER(u.email) = LOWER(?)`,
          [userEmail]
        );

        // Check isActive as 1 since SQLite uses 0/1 for booleans
        if (!userOrg || userOrg.isActive !== 1) {
          logger.warn(`Login attempt by non-user or inactive user: ${userEmail}`);
          logger.info(`User found: ${JSON.stringify(userOrg)}`);  // Add this for debugging
          return {
            success: true,
            message: "If this email is registered, you'll receive a login link shortly."
          };
        }

        const magicLink = await auth.createMagicLink(userEmail, userOrg.orgSlug);

        // In development, log the magic link
        if (process.env.NODE_ENV === 'development') {
          logger.info('=======================================');
          logger.info('Magic Link for Development:');
          logger.info(magicLink);
          logger.info('=======================================');
          
          return { success: true, message: 'Magic link generated - check server logs' };
        }

        // In production, send email
        await email.sendMagicLink(userEmail, magicLink, userOrg.orgSlug);
        return {
          success: true,
          message: "If this email is registered, you'll receive a login link shortly."
        };

      } catch (error) {
        logger.error(`Login failed: ${error}`);
        return { 
          success: false, 
          error: 'Failed to process login request' 
        };
      }
    })

    .get('/api/auth/verify/:organizationSlug/:token', async ({ params, cookie, setCookie }) => {
      const { token, organizationSlug } = params;
      
      logger.info(`Starting verification for org ${organizationSlug}`);

      try {
        logger.info('Verifying magic link');
        const result = await auth.verifyMagicLink(token, organizationSlug);
        logger.info(`Magic link verification result: ${JSON.stringify(result)}`);
        
        if (!result.valid) {
          logger.error('Magic link validation failed');
          return {
            success: false,
            redirectUrl: "/login",
            session: "",
            email: ""
          };
        }

        // Create session ID
        const sessionId = crypto.randomBytes(32).toString('hex');
        logger.info(`Created session ID: ${sessionId}`);

        // Get user info
        const user = await db.fetchOne<User>(
          `SELECT u.* FROM users u 
           JOIN organizations o ON u.organization_id = o.id 
           WHERE LOWER(u.email) = LOWER(?) AND o.slug = ?`,
          [result.email, organizationSlug]
        );

        if (!user) {
          logger.error('User not found after magic link verification');
          return {
            success: false,
            redirectUrl: "/login",
            session: "",
            email: ""
          };
        }

        // Create session in database
        const expiresAt = new Date();
        expiresAt.setDate(expiresAt.getDate() + 7); // 7 days from now

        await db.execute(
          'INSERT INTO sessions (id, user_id, expires_at) VALUES (?, ?, ?)',
          [sessionId, user.id, expiresAt.toISOString()]
        );

        logger.info(`Created session in database for user ${user.id}`);

        // Set session cookie
        setCookie('session', sessionId, {
          path: '/',
          httpOnly: true,
          secure: process.env.NODE_ENV === 'production',
          sameSite: 'lax',
          maxAge: 60 * 60 * 24 * 7 // 7 days
        });

        const verificationResult = {
          success: true,
          redirectUrl: result.redirectUrl || '/dashboard',
          session: sessionId,
          email: result.email,
          orgSlug: organizationSlug
        };
        logger.info(`Sending verification response: ${JSON.stringify(verificationResult)}`);
        return verificationResult;

      } catch (error) {
        logger.error(`Verification error: ${error}`);
        if (error instanceof Error) {
          logger.error(`Error details: ${error.message}`);
          logger.error(`Stack trace: ${error.stack}`);
        }
        return {
          success: false,
          redirectUrl: "/login",
          session: "",
          email: ""
        };
      }
    })

    .get('/api/auth/session', async ({ cookie }) => {
      const sessionId = cookie.session;
      
      if (!sessionId) {
        return { 
          valid: false,
          session: "",
          email: "",
          organizationSlug: "",
          first_name: "",
          last_name: "",
          id: ""
        };
      }

      try {
        // Get user and organization info from session
        const sessionUser = await db.fetchOne(
          `SELECT u.id, u.email, u.first_name, u.last_name, o.slug as organization_slug
           FROM sessions s
           JOIN users u ON s.user_id = u.id
           JOIN organizations o ON u.organization_id = o.id
           WHERE s.id = ?`,
          [sessionId]
        );

        if (!sessionUser) {
          return { 
            valid: false,
            session: "",
            email: "",
            organizationSlug: "",
            first_name: "",
            last_name: "",
            id: ""
          };
        }

        return { 
          valid: true,
          session: sessionId,
          email: sessionUser.email,
          organizationSlug: sessionUser.organization_slug,
          first_name: sessionUser.first_name,
          last_name: sessionUser.last_name,
          id: sessionUser.id
        };
      } catch (error) {
        logger.error('Error getting session info:', error);
        return { 
          valid: false,
          session: "",
          email: "",
          organizationSlug: "",
          first_name: "",
          last_name: "",
          id: ""
        };
      }
    })

    .post('/api/auth/logout', async ({ setCookie }) => {
        // Clear the session cookie
        setCookie('session', '', {
            httpOnly: true,
            secure: process.env.NODE_ENV === 'production',
            maxAge: 0, // Expire immediately
            path: '/'
        });

        return { success: true };
    });
} 