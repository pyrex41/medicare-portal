import { Elysia } from 'elysia';
import { cookie } from '@elysiajs/cookie';
import { AuthService } from '../services/auth';
import { EmailService } from '../services/email';
import { logger } from '../logger';
import { randomBytes } from 'crypto';
import { config } from '../config';
import crypto from 'crypto';
import { db } from '../database';
import { Database } from '../database';
import { getUserFromSession } from '../services/auth';
import type { User } from '../types';

const dbInstance = new Database();

export function createAuthRoutes() {
  const auth = new AuthService(
    process.env.NODE_ENV === 'development' 
      ? 'http://localhost:5173'  // Frontend URL in development
      : (process.env.BASE_URL || 'http://localhost:3000')
  );
  const emailService = new EmailService();

  return new Elysia()
    .use(cookie())
    .post('/api/auth/login', async ({ body, set }) => {
      try {
        const { email } = body as { email: string };
        logger.info(`Login request for email: ${email}`);

        // Check if user exists
        const client = dbInstance.getClient();
        const userResult = await client.execute({
          sql: 'SELECT id, email FROM users WHERE email = ? AND is_active = 1',
          args: [email]
        });

        if (userResult.rows.length === 0) {
          // Don't reveal if user exists or not
          logger.info(`No active user found for email: ${email}`);
          return { success: true };
        }

        // Generate and send magic link
        const magicLink = await auth.createMagicLink(
          email,
          'default', // Default organization for now
          { redirectUrl: '/dashboard' }
        );

        if (process.env.NODE_ENV === 'development') {
          logger.info(`Development mode - Magic link: ${magicLink}`);
        } else {
          // In production, send email with magic link
          await emailService.sendLoginLink(email, magicLink);
        }

        return { success: true };

      } catch (e) {
        logger.error(`Login error: ${e}`);
        set.status = 500;
        return { 
          success: false,
          error: 'Internal server error'
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

        // First find user by email
        const user = await db.fetchOne<User>(
          `SELECT u.*, o.slug as organization_slug 
           FROM users u 
           JOIN organizations o ON u.organization_id = o.id 
           WHERE LOWER(u.email) = LOWER(?) AND u.is_active = 1`,
          [result.email]
        );

        if (!user) {
          logger.error(`No active user found for email: ${result.email}`);
          return {
            success: false,
            redirectUrl: "/login",
            session: "",
            email: ""
          };
        }

        logger.info(`Found user: ${JSON.stringify(user)}`);

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
          orgSlug: user.organization_slug
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
      logger.info(`Session check - Cookie session ID: ${sessionId}`);
      
      if (!sessionId) {
        logger.info('No session cookie found');
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
        logger.info(`Looking up session in database: ${sessionId}`);
        const sessionUser = await db.fetchOne<{
          id: number;
          email: string;
          first_name: string;
          last_name: string;
          organization_slug: string;
        }>(
          `SELECT u.id, u.email, u.first_name, u.last_name, o.slug as organization_slug
           FROM sessions s
           JOIN users u ON s.user_id = u.id
           JOIN organizations o ON u.organization_id = o.id
           WHERE s.id = ?`,
          [sessionId]
        );

        if (!sessionUser) {
          logger.info(`No session found in database for ID: ${sessionId}`);
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

        logger.info(`Found valid session for user: ${sessionUser.email}`);
        logger.info(`Session details: ${JSON.stringify(sessionUser, null, 2)}`);

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
        logger.error(`Error getting session info: ${error}`);
        if (error instanceof Error) {
          logger.error(`Error details: ${error.message}`);
          logger.error(`Stack trace: ${error.stack}`);
        }
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

    .post('/api/auth/logout', async ({ set }) => {
      set.headers['Set-Cookie'] = 'session=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT';
      return { success: true };
    });
} 