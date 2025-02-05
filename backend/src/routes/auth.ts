import { Elysia } from 'elysia';
import { cookie } from '@elysiajs/cookie';
import { AuthService } from '../services/auth';
import { EmailService } from '../services/email';
import { logger } from '../logger';
import { randomBytes } from 'crypto';
import { config } from '../config';
import crypto from 'crypto';

export function createAuthRoutes() {
  const auth = new AuthService(
    process.env.NODE_ENV === 'development' 
      ? 'http://localhost:5173'  // Frontend URL in development
      : (process.env.PUBLIC_URL || 'http://localhost:3000')
  );
  const email = new EmailService();

  return new Elysia()
    .use(cookie())
    .post('/api/auth/login/:organizationSlug', async ({ params, body }) => {
      const { email: userEmail } = body as { email: string };
      const { organizationSlug } = params;

      try {
        const magicLink = await auth.createMagicLink(userEmail, organizationSlug);

        // In development, log the magic link
        if (process.env.NODE_ENV === 'development') {
          logger.info('=======================================');
          logger.info('Magic Link for Development:');
          logger.info(magicLink);
          logger.info('=======================================');
          
          return { success: true, message: 'Magic link generated - check server logs' };
        }

        // In production, send email
        await email.sendMagicLink(userEmail, magicLink, organizationSlug);
        return { success: true };

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

      try {
        logger.info('Verifying magic link');
        const result = await auth.verifyMagicLink(token, organizationSlug);
        logger.info(`Verification result: ${JSON.stringify(result)}`);
        
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

        // Set session cookie using Elysia's cookie API
        setCookie('session', sessionId, {
          httpOnly: true,
          secure: process.env.NODE_ENV === 'production',
          maxAge: 7 * 24 * 60 * 60, // 7 days
          path: '/'
        });

        const response = { 
          success: true,
          redirectUrl: result.redirectUrl || '/templanding',
          session: sessionId,
          email: result.email || ''
        };
        logger.info(`Sending verification response: ${JSON.stringify(response)}`);
        return response;

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
          email: ""
        };
      }

      // For now, just validate that the session cookie exists
      return { 
        valid: true,
        session: sessionId,
        email: "" // We could store this in the session if needed
      };
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