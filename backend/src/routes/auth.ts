import { Elysia } from 'elysia';
import { AuthService } from '../services/auth';
import { Database } from '../database';
import { logger } from '../logger';
import { EmailService } from '../services/email';

export function createAuthRoutes(db: Database) {
  const auth = new AuthService(
    db,
    process.env.PUBLIC_URL || 'http://localhost:3000'
  );
  const email = new EmailService();

  return new Elysia()
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
        await email.sendMagicLink(userEmail, magicLink, org.name);

        return { success: true };
      } catch (error) {
        logger.error('Login failed:', error);
        throw new Error('Login failed');
      }
    })

    .get('/api/auth/verify/:organizationSlug/:token', async ({ params, setCookie }) => {
      const { token, organizationSlug } = params;

      const result = await auth.verifyMagicLink(token, organizationSlug);
      
      if (!result.valid) {
        throw new Error('Invalid or expired magic link');
      }

      // Create session
      const sessionId = crypto.randomBytes(32).toString('hex');
      await db.execute(
        `INSERT INTO sessions (id, user_id, expires_at)
         SELECT ?, u.id, datetime('now', '+7 days')
         FROM users u
         WHERE u.email = ? AND u.organization_id = ?`,
        [sessionId, result.email, result.organizationId]
      );

      // Set session cookie
      setCookie('session', sessionId, {
        httpOnly: true,
        secure: process.env.NODE_ENV === 'production',
        maxAge: 7 * 24 * 60 * 60, // 7 days
        path: '/'
      });

      return { success: true };
    });
} 