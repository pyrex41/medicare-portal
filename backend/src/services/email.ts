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
    try {
      const msg = {
        to: email,
        from: process.env.SENDGRID_FROM_EMAIL || 'information@medicaremax.ai',
        subject: 'Your Login Link',
        text: `Click this link to log in: ${magicLink}\n\nThis link will expire in 30 minutes.`,
        html: `
          <div style="font-family: Arial, sans-serif; max-width: 600px; margin: 0 auto;">
            <h2 style="color: #333;">Welcome to MedicareMax</h2>
            <p>Click the button below to log in to your account. This link will expire in 30 minutes.</p>
            <div style="margin: 30px 0;">
              <a href="${magicLink}" 
                 style="background-color: #4F46E5; color: white; padding: 12px 24px; 
                        text-decoration: none; border-radius: 4px; display: inline-block;">
                Log In
              </a>
            </div>
            <p style="color: #666; font-size: 14px;">
              If the button doesn't work, copy and paste this link into your browser:
              <br>
              <a href="${magicLink}" style="color: #4F46E5;">${magicLink}</a>
            </p>
          </div>
        `
      };

      await sgMail.send(msg);
      logger.info(`Magic link email sent successfully to ${email}`);
    } catch (error) {
      logger.error(`Error sending magic link email: ${error}`);
      throw new Error('Failed to send magic link email');
    }
  }
}

// For the organization signup flow
export async function sendMagicLink({ email, magicLink, name }: {
  email: string;
  magicLink: string;
  name: string;
}) {
  try {
    const msg = {
      to: email,
      from: process.env.SENDGRID_FROM_EMAIL || 'information@medicaremax.ai',
      subject: 'Verify Your MedicareMax Account',
      text: `Hi ${name},\n\nClick this link to verify your account: ${magicLink}\n\nThis link will expire in 30 minutes.`,
      html: `
        <div style="font-family: Arial, sans-serif; max-width: 600px; margin: 0 auto;">
          <h2 style="color: #333;">Welcome to MedicareMax</h2>
          <p>Hi ${name},</p>
          <p>Click the button below to verify your account and complete your organization setup. This link will expire in 30 minutes.</p>
          <div style="margin: 30px 0;">
            <a href="${magicLink}" 
               style="background-color: #4F46E5; color: white; padding: 12px 24px; 
                      text-decoration: none; border-radius: 4px; display: inline-block;">
              Verify Account
            </a>
          </div>
          <p style="color: #666; font-size: 14px;">
            If the button doesn't work, copy and paste this link into your browser:
            <br>
            <a href="${magicLink}" style="color: #4F46E5;">${magicLink}</a>
          </p>
        </div>
      `
    };

    await sgMail.send(msg);
    logger.info('Magic link email sent successfully');
  } catch (error) {
    logger.error(`Failed to send magic link email: ${error}`);
    throw error;
  }
} 