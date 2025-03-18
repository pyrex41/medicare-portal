import sgMail from '@sendgrid/mail';
import { logger } from '../logger';
import crypto from 'crypto';


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

  async sendQuoteEmail(params: {
    email: string;
    firstName: string;
    lastName: string;
    quoteUrl: string;
    planType: string;
  }) {
    try {
      const { email, firstName, lastName, quoteUrl, planType } = params;
      const fullName = `${firstName} ${lastName}`;
      
      const msg = {
        to: email,
        from: process.env.SENDGRID_FROM_EMAIL || 'information@medicaremax.ai',
        subject: 'Your Personalized Medicare Quote',
        text: `Hello ${fullName},\n\nThank you for your interest in Medicare coverage options. We've prepared a personalized quote for your ${planType} plan. You can view and compare your options by clicking the link below:\n\n${quoteUrl}\n\nIf you have any questions, please don't hesitate to reach out.\n\nThe MedicareMax Team`,
        html: `
          <div style="font-family: Arial, sans-serif; max-width: 600px; margin: 0 auto;">
            <h2 style="color: #333;">Your Medicare Quote is Ready</h2>
            <p>Hello ${firstName},</p>
            <p>Thank you for your interest in Medicare coverage options. We've prepared a personalized quote for your ${planType} plan.</p>
            <div style="margin: 30px 0;">
              <a href="${quoteUrl}" 
                 style="background-color: #4F46E5; color: white; padding: 12px 24px; 
                        text-decoration: none; border-radius: 4px; display: inline-block;">
                View Your Quote
              </a>
            </div>
            <p>This quote has been tailored specifically for your needs based on the information you provided.</p>
            <p style="color: #666; font-size: 14px;">
              If the button doesn't work, copy and paste this link into your browser:
              <br>
              <a href="${quoteUrl}" style="color: #4F46E5;">${quoteUrl}</a>
            </p>
            <div style="margin-top: 30px; padding-top: 20px; border-top: 1px solid #eee;">
              <p style="color: #888; font-size: 14px;">
                If you have any questions, please don't hesitate to contact us.
                <br>
                The MedicareMax Team
              </p>
            </div>
          </div>
        `
      };

      await sgMail.send(msg);
      logger.info(`Quote email sent successfully to ${email}`);
      return { success: true };
    } catch (error) {
      logger.error(`Error sending quote email: ${error}`);
      throw new Error('Failed to send quote email');
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

// Generate a hash for onboarding link validation
function generateHash(orgId: string, email: string): string {
  const SECRET = process.env.HASH_SECRET || 'default-hash-secret'; // Ensure this is set in your environment
  return crypto.createHash('sha256').update(`${orgId}${email}${SECRET}`).digest('hex').slice(0, 16);
}

/**
 * Sends an onboarding link that includes organizationId, and optionally email and hash
 * @param orgId Organization ID to include in the link
 * @param email Optional email to include in the link and use as recipient
 */
export async function sendOnboardingLink(orgId: string, email?: string) {
  try {
    // Base onboarding URL with orgId
    const baseUrl = process.env.FRONTEND_URL || 'https://medicaremax.ai';
    let onboardingUrl = `${baseUrl}/onboard?orgId=${orgId}`;
    
    // If email is provided, add email and hash to the URL
    if (email) {
      const hash = generateHash(orgId, email);
      onboardingUrl += `&email=${encodeURIComponent(email)}&hash=${hash}`;
    }
    
    // Create email message
    const msg = {
      to: email || process.env.FALLBACK_EMAIL || 'information@medicaremax.ai', // Fallback if no email provided
      from: process.env.SENDGRID_FROM_EMAIL || 'information@medicaremax.ai',
      subject: 'Join MedicareMax',
      text: `Click this link to create or update your profile: ${onboardingUrl}`,
      html: `
        <div style="font-family: Arial, sans-serif; max-width: 600px; margin: 0 auto;">
          <h2 style="color: #333;">Welcome to MedicareMax</h2>
          <p>Click the button below to create or update your profile:</p>
          <div style="margin: 30px 0;">
            <a href="${onboardingUrl}" 
               style="background-color: #4F46E5; color: white; padding: 12px 24px; 
                      text-decoration: none; border-radius: 4px; display: inline-block;">
              Get Started
            </a>
          </div>
          <p style="color: #666; font-size: 14px;">
            If the button doesn't work, copy and paste this link into your browser:
            <br>
            <a href="${onboardingUrl}" style="color: #4F46E5;">${onboardingUrl}</a>
          </p>
          <div style="margin-top: 30px; padding-top: 20px; border-top: 1px solid #eee;">
            <p style="color: #888; font-size: 14px;">
              If you have any questions, please don't hesitate to contact us.
              <br>
              The MedicareMax Team
            </p>
          </div>
        </div>
      `
    };

    await sgMail.send(msg);
    logger.info(`Onboarding link email sent successfully${email ? ` to ${email}` : ''}`);
    return { success: true };
  } catch (error) {
    logger.error(`Error sending onboarding link email: ${error}`);
    throw new Error('Failed to send onboarding link email');
  }
} 