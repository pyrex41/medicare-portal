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
  const auth = new AuthService();
  const emailService = new EmailService();

  return new Elysia()
    .use(cookie())
    .post('/api/auth/set-session', async ({ body, set, setCookie }) => {
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
          set.status = 404;
          return { success: false, error: 'User not found' };
        }
      const sessionId = crypto.randomBytes(32).toString('hex');
      const expiresAt = new Date();
      expiresAt.setDate(expiresAt.getDate() + 7); // 7 days from now

      await db.execute(
        'INSERT INTO sessions (id, user_id, expires_at) VALUES (?, ?, ?)',
        [sessionId, userResult.rows[0].id, expiresAt.toISOString()]
      );

      setCookie('session', sessionId, {
        path: '/',
        httpOnly: true,
        secure: process.env.NODE_ENV === 'production',
        sameSite: 'lax',
        maxAge: 60 * 60 * 24 * 1 // 1 day -- shorter to force them to login again soon after onboarding
      });

      set.status = 200;

      set.status = 200;
      return { success: true };
    })
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

        // Always send the email, but also log in development
        if (process.env.NODE_ENV === 'development') {
          logger.info(`Development mode - Magic link: ${magicLink}`);
        }
        
        // Send the email
        await emailService.sendMagicLink(email, magicLink, 'default');

        set.status = 200;
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

    .get('/api/signup/verify/:token', async ({ params, cookie, setCookie }) => {
      const { token } = params;
      logger.info(`Starting verification for token: ${token}`);

      try {
        const result = await auth.verifySignupLink(token);
        logger.info(`Signup verification result: ${JSON.stringify(result)}`);

        if (!result.valid) {
          logger.error('Signup verification failed');
          return {
            success: false, 
            redirectUrl: "/signup",
            session: "",
            email: ""
          };
        }

        logger.info(`Signup verification successful`);

        return {
          success: true,
          redirectUrl: result.redirectUrl || '/onboarding',
          session: "",
          email: result.email
        };
      } catch (error) {
        logger.error(`Signup verification error: ${error}`);
        return {
          success: false,
          redirectUrl: "/signup",
          session: "",  
          email: ""
        };
      }
    })

    .post('/api/auth/onboarding-login', async ({ body, set, setCookie }) => {
      const { emailRaw } = body as { emailRaw: string };
      logger.info(`Onboarding login request for email: ${emailRaw}`);

      // Decode the email if it's URL encoded and trim whitespace
      const email = decodeURIComponent(emailRaw).trim();
      
      const user = await db.fetchOne<User>(
        `SELECT u.*, o.slug as organization_slug 
        FROM users u 
        JOIN organizations o ON u.organization_id = o.id 
        WHERE LOWER(u.email) = LOWER(?) AND u.is_active = 1`,
        [email]
      );
      
      if (!user) {
        logger.error(`No active user found for email: ${email}`);
        return {
          success: false,
          redirectUrl: "/signup",
          email: ""
        };
      }
      
      logger.info(`Found user: ${JSON.stringify(user)}`);
      
      const sessionId = crypto.randomBytes(32).toString('hex');
      logger.info(`Created session ID: ${sessionId}`);
      const expiresAt = new Date();
      expiresAt.setDate(expiresAt.getDate() + 1); // 1 day from now

      await db.execute(
        'INSERT INTO sessions (id, user_id, expires_at) VALUES (?, ?, ?)',
        [sessionId, user.id, expiresAt.toISOString()]
      );

      logger.info(`Created session in database for user ${user.id}`);

      setCookie('session', sessionId, {
        path: '/',
        httpOnly: true,
        secure: process.env.NODE_ENV === 'production',
        sameSite: 'lax',
        maxAge: 60 * 60 * 24 * 1 // 1 day -- shorter to force them to login again soon after onboarding
      });

      set.status = 200;
      return {
        success: true,  
        redirectUrl: "/contacts",
        email: user.email
      };
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
          redirectUrl: result.redirectUrl || '/walkthrough',  // Use the redirectUrl from magic link payload
          session: sessionId,
          email: result.email,
          orgSlug: user.organization_id.toString()  // Use organization_id since organization_slug may not exist
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

    .get('/api/auth/session', async ({ cookie, set }) => {
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
          id: "",
          demo_mode: true,
          orgCreateDate: ""
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
          is_admin: boolean;
          organization_slug: string;
          demo_mode: boolean;
          orgCreateDate: string;
        }>(
          `SELECT u.id, u.email, u.first_name, u.last_name, u.is_admin, o.slug as organization_slug, o.demo_mode, o.created_at as orgCreateDate
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
            is_admin: false,
            id: "",
            demo_mode: true,
            orgCreateDate: ""
          };
        }

        logger.info(`Found valid session for user: ${sessionUser.email}`);
        logger.info(`Session details: ${JSON.stringify(sessionUser, null, 2)}`);

        set.status = 200;
        return { 
          valid: true,
          session: sessionId,
          email: sessionUser.email,
          organizationSlug: sessionUser.organization_slug,
          first_name: sessionUser.first_name,
          last_name: sessionUser.last_name,
          is_admin: sessionUser.is_admin,
          id: sessionUser.id,
          demo_mode: sessionUser.demo_mode,
          orgCreateDate: sessionUser.orgCreateDate.split(/[ T]/)[0]
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
          is_admin: false,
          id: "",
          demo_mode: true,
          orgCreateDate: ""
        };
      }
    })

    .post('/api/auth/logout', async ({ set }) => {
      set.headers['Set-Cookie'] = 'session=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT';
      return { success: true };
    });
} 