import sgMail from '@sendgrid/mail';
import { logger } from '../logger';

interface MagicLinkEmailParams {
  email: string;
  token: string;
  orgId: number;
  name: string;
}

export class EmailService {
  constructor() {
    if (!process.env.SENDGRID_API_KEY) {
      throw new Error('Missing SENDGRID_API_KEY environment variable');
    }
    sgMail.setApiKey(process.env.SENDGRID_API_KEY);
  }

  async sendMagicLink(email: string, magicLink: string, organizationSlug: string) {
    // In development, just log the magic link
    if (process.env.NODE_ENV === 'development') {
      logger.info('=======================================');
      logger.info('Magic Link Email would be sent to:', email);
      logger.info('Organization:', organizationSlug);
      logger.info('Link:', magicLink);
      logger.info('=======================================');
      return;
    }

    // TODO: Implement actual email sending in production
    // You would integrate with your email service here (SendGrid, AWS SES, etc.)
    throw new Error('Email sending not implemented in production');
  }
}

// For the organization signup flow
export async function sendMagicLink({ email, magicLink, name }: {
  email: string;
  magicLink: string;
  name: string;
}) {
  try {
    logger.info('Attempting to send magic link email', { email, name });
    
    if (process.env.NODE_ENV === 'development') {
      logger.info('=======================================');
      logger.info('Organization Verification Email');
      logger.info('To:', email);
      logger.info('Name:', name);
      logger.info('Magic Link:', magicLink);
      logger.info('=======================================');
      return;
    }

    // TODO: Add production email sending
    logger.info('Email sending would happen here in production');
    return;
  } catch (error) {
    logger.error('Failed to send magic link email:', error);
    if (error instanceof Error) {
      logger.error('Error details:', error.message);
    }
    throw error;
  }
} 