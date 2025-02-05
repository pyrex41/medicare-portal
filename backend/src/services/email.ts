import sgMail from '@sendgrid/mail';
import { logger } from '../logger';

export class EmailService {
  constructor() {
    if (!process.env.SENDGRID_API_KEY) {
      throw new Error('Missing SENDGRID_API_KEY environment variable');
    }
    sgMail.setApiKey(process.env.SENDGRID_API_KEY);
  }

  async sendMagicLink(email: string, magicLink: string, organizationName: string) {
    try {
      // Always log in development
      if (process.env.NODE_ENV === 'development') {
        logger.info('=======================================');
        logger.info('Magic Link Email Details:');
        logger.info(`To: ${email}`);
        logger.info(`Organization: ${organizationName}`);
        logger.info(`Link: ${magicLink}`);
        logger.info('=======================================');
      }

      await sgMail.send({
        to: email,
        from: process.env.SENDGRID_FROM_EMAIL || 'noreply@medicaremax.com',
        subject: 'Your Login Link for Medicare Max',
        text: `Click this link to log in: ${magicLink}`,
        html: `
          <h2>Welcome to Medicare Max</h2>
          <p>Click the button below to log in to ${organizationName}:</p>
          <p>
            <a href="${magicLink}" 
               style="background-color: #4CAF50; color: white; padding: 12px 20px; 
                      text-decoration: none; border-radius: 4px; display: inline-block;">
              Log In
            </a>
          </p>
          <p>This link will expire in 30 minutes.</p>
          <p>If you didn't request this login link, you can safely ignore this email.</p>
          <p>After logging in, you'll be redirected to your dashboard.</p>
        `
      });
      
      // Only log success message, not the actual link in production
      if (process.env.NODE_ENV === 'production') {
        logger.info(`Magic link email sent to ${email}`);
      }
    } catch (error) {
      logger.error('Failed to send magic link email:', error);
      throw new Error('Failed to send login email');
    }
  }
} 