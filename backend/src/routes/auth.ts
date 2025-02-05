import { Elysia } from 'elysia';
import { AuthService } from '../services/auth';
import { Database } from '../database';
import { logger } from '../logger';
import { EmailService } from '../services/email';
import { randomBytes } from 'crypto';
import { config } from '../config';

export function createAuthRoutes(db: Database) {
  const auth = new AuthService(
    db,
    process.env.PUBLIC_URL || 'http://localhost:3000'
  );
  const email = new EmailService();

  return new Elysia()
    .post('/api/auth/login', async ({ body }) => {
      const { email } = body as { email: string };
      
      try {
        // Generate random token
        const token = randomBytes(32).toString('hex');
        const expiresAt = new Date(Date.now() + 1000 * 60 * 15); // 15 minutes

        // Store magic link
        await db.execute(
          `INSERT INTO magic_links (token, email, expires_at) 
           VALUES (?, ?, ?)`,
          [token, email, expiresAt.toISOString()]
        );

        // Construct magic link URL
        const baseUrl = process.env.NODE_ENV === 'development' 
          ? 'http://localhost:5173'
          : config.FRONTEND_URL;
        const magicLink = `${baseUrl}/verify?token=${token}`;

        // In development, log the magic link
        if (process.env.NODE_ENV === 'development') {
          logger.info('=======================================');
          logger.info('Magic Link for Development:');
          logger.info(magicLink);
          logger.info('=======================================');
          
          return { success: true, message: 'Magic link generated - check server logs' };
        }

        // In production, would send email here
        // await sendEmail(email, magicLink);

        return { success: true, message: 'Magic link sent to email' };
      } catch (error) {
        logger.error(`Login error: ${error}`);
        return { success: false, error: 'Failed to process login request' };
      }
    })

    .post('/api/auth/login/:organizationSlug', async ({ params, body }) => {
      const { email: userEmail } = body as { email: string };
      const { organizationSlug } = params;

      try {
        // For demo purposes, create organization if it doesn't exist
        let org = await db.fetchOne(
          'SELECT id, name FROM organizations WHERE slug = ? OR name = ?',
          [organizationSlug, organizationSlug]
        );

        if (!org) {
          // Create new organization
          const result = await db.execute(
            `INSERT INTO organizations (name, slug, subscription_tier) 
             VALUES (?, ?, 'basic')`,
            [organizationSlug, organizationSlug]
          );
          
          org = await db.fetchOne(
            'SELECT id, name FROM organizations WHERE id = ?',
            [result[0].lastInsertId]
          );
          
          if (!org) {
            throw new Error('Failed to create organization');
          }
        }

        // Create or update user
        await db.execute(
          `INSERT INTO users (email, organization_id, role) 
           VALUES (?, ?, 'admin')
           ON CONFLICT(email) DO UPDATE SET 
           organization_id = excluded.organization_id`,
          [userEmail, org.id]
        );

        const magicLink = await auth.createMagicLink(userEmail, organizationSlug);

        // In development, log the magic link
        if (process.env.NODE_ENV === 'development') {
          logger.info('=======================================');
          logger.info('Organization Magic Link for Development:');
          logger.info(magicLink);
          logger.info('=======================================');
          
          return { success: true, message: 'Magic link generated - check server logs' };
        }

        // In production, send email
        await email.sendMagicLink(userEmail, magicLink, org.name);

        return { success: true };
      } catch (error) {
        logger.error('Login failed:', error);
        throw new Error('Login failed');
      }
    })

    .get('/api/auth/verify/:organizationSlug/:token', async ({ params, setCookie, query }) => {
      const { token, organizationSlug } = params;

      const result = await auth.verifyMagicLink(token, organizationSlug);
      
      if (!result.valid) {
        throw new Error('Invalid or expired magic link');
      }

      // Create session
      const sessionId = crypto.randomBytes(32).toString('hex');
      const sessionResult = await db.execute(
        `INSERT INTO sessions (id, user_id, expires_at)
         SELECT ?, u.id, datetime('now', '+7 days')
         FROM users u
         WHERE u.email = ? AND u.organization_id = ?
         RETURNING id, user_id`,
        [sessionId, result.email, result.organizationId]
      );

      if (!sessionResult || sessionResult.length === 0) {
        throw new Error('Failed to create session');
      }

      // Set session cookie
      setCookie('session', sessionId, {
        httpOnly: true,
        secure: process.env.NODE_ENV === 'production',
        maxAge: 7 * 24 * 60 * 60, // 7 days
        path: '/'
      });

      // Return with session info and redirect URL
      return { 
        success: true,
        redirectUrl: '/templanding',
        session: sessionId,
        email: result.email
      };
    })

    .get('/api/auth/session', async ({ cookie }) => {
      const sessionId = cookie.session;
      
      if (!sessionId) {
        return { valid: false };
      }

      const session = await db.fetchOne(
        `SELECT s.*, u.email, u.organization_id 
         FROM sessions s
         JOIN users u ON u.id = s.user_id
         WHERE s.id = ? AND s.expires_at > datetime('now')`,
        [sessionId]
      );

      if (!session) {
        return { valid: false };
      }

      return { 
        valid: true,
        session: sessionId,
        email: session.email,
        organizationId: session.organization_id
      };
    });
} 