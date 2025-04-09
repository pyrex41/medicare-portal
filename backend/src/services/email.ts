import sgMail from '@sendgrid/mail';
import { logger } from '../logger';
import crypto from 'crypto';
import { Database } from '../database';

interface MagicLinkEmailParams {
  email: string;
  token: string;
  orgId: number;
  name: string;
}

interface EmailTrackingRecord {
  orgId: number;
  contactId: string | number;
  emailType: string;
  sendStatus: 'pending' | 'processing' | 'accepted' | 'delivered' | 'sent' | 'deferred' | 'bounced' | 'dropped' | 'failed' | 'skipped';
  sendMode: 'test' | 'production';
  testEmail?: string;
  batchId: string;
  messageId?: string;
}

export class EmailService {
  constructor() {
    if (!process.env.SENDGRID_API_KEY) {
      throw new Error('Missing SENDGRID_API_KEY environment variable');
    }
    sgMail.setApiKey(process.env.SENDGRID_API_KEY);
  }

  /**
   * Records an email send in the email_send_tracking table
   * @param orgDb Organization database instance
   * @param tracking Email tracking data
   * @returns Result of the database operation
   */
  async recordEmailSend(orgDb: Database, tracking: EmailTrackingRecord): Promise<any> {
    try {
      const now = new Date().toISOString();
      
      return await orgDb.execute(
        `INSERT INTO email_send_tracking (
          org_id, 
          contact_id, 
          email_type, 
          scheduled_date, 
          send_status,
          send_mode,
          test_email,
          batch_id,
          message_id,
          last_attempt_date
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
        [
          tracking.orgId,
          tracking.contactId.toString(),
          tracking.emailType,
          now,
          tracking.sendStatus,
          tracking.sendMode,
          tracking.testEmail || null,
          tracking.batchId,
          tracking.messageId || null,
          now
        ]
      );
    } catch (error) {
      logger.error(`Error recording email send: ${error}`);
      throw new Error(`Failed to record email send: ${error}`);
    }
  }

  async sendMagicLink(email: string, magicLink: string, organizationSlug: string) {
    try {
      const fromEmail = process.env.SENDGRID_FROM_EMAIL || 'information@medicaremax.ai';
      const msg = {
        to: email,
        from: {
          email: fromEmail,
          name: organizationSlug ? organizationSlug.charAt(0).toUpperCase() + organizationSlug.slice(1) : 'MedicareMax'
        },
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
    organization?: {
      name?: string;
      logo_data?: string;
      primary_color?: string;
      phone?: string;
      website?: string;
    };
  }) {
    try {
      const { email, firstName, lastName, quoteUrl, planType, organization } = params;
      const fullName = `${firstName} ${lastName}`;
      const orgName = organization?.name || '';
      const primaryColor = organization?.primary_color || '#4F46E5';
      const phone = organization?.phone || '';
      const website = organization?.website || '';
      const websiteUrl = website.startsWith('http') ? website : `https://${website}`;
      
      // Format phone number if present
      const formatPhoneNumber = (phoneStr: string): string => {
        // Remove any non-digit characters
        const digitsOnly = phoneStr.replace(/\D/g, '');
        if (digitsOnly.length === 10) {
          return `(${digitsOnly.substring(0, 3)}) ${digitsOnly.substring(3, 6)}-${digitsOnly.substring(6)}`;
        } else if (digitsOnly.length === 11 && digitsOnly.startsWith('1')) {
          return `(${digitsOnly.substring(1, 4)}) ${digitsOnly.substring(4, 7)}-${digitsOnly.substring(7)}`;
        }
        // Return original if not a standard format
        return phoneStr;
      };
      
      const formattedPhone = phone ? formatPhoneNumber(phone) : '';
      
      const fromEmail = process.env.SENDGRID_FROM_EMAIL || 'information@medicaremax.ai';
      
      // Prepare email message
      const msg: any = {
        to: email,
        from: {
          email: fromEmail,
          name: orgName || 'MedicareMax'
        },
        subject: 'Your Personalized Medicare Quote',
        text: `Hello ${fullName},\n\nWe recently reviewed Medigap premiums for your zip code and found some options that might interest you. These plans offer the same comprehensive benefits you currently enjoy, potentially at a better value. We've done the research to find plans that maintain your coverage while possibly reducing your costs.\n\nReview your options here: ${quoteUrl}\n\nMany Medicare beneficiaries don't realize they can be paying different rates for identical coverage. We'd be happy to show you your options and potential savings. If we don't find a better value now, we'll keep monitoring rates and reach out when we find something promising.\n\nIf you have any questions, please give me a call: ${formattedPhone}\n\nBest,\n${orgName}\nYour Medicare Specialist\n${formattedPhone}`,
        attachments: []
      };
      
      // Handle logo as attachment if present
      let logoHtml = '';
      if (organization?.logo_data) {
        // Extract the base64 content, removing data URL prefix if present
        let logoBase64 = organization.logo_data;
        if (logoBase64.startsWith('data:image/')) {
          const base64Parts = logoBase64.split(',');
          if (base64Parts.length > 1) {
            logoBase64 = base64Parts[1];
          }
        }
        
        // Add as attachment with content ID
        msg.attachments.push({
          content: logoBase64,
          filename: 'organization-logo.png',
          type: 'image/png',
          disposition: 'inline',
          content_id: 'organizationLogo'
        });
        
        // Reference the content ID in HTML
        logoHtml = `<div class="logo">
          <img src="cid:organizationLogo" alt="${orgName} Logo" width="160" 
               style="max-width: 250px; height: auto; margin-bottom: 25px; display: block; object-fit: contain; border: 0; outline: none;" />
        </div>`;
      } else {
        // Fallback if no logo
        logoHtml = `<div class="logo" style="margin-bottom: 25px;">
          <h1 style="font-size: 24px; font-weight: bold; color: ${primaryColor}; margin: 0;">${orgName}</h1>
        </div>`;
      }
      
      // Set HTML content
      msg.html = `
        <!DOCTYPE html>
        <html>
        <head>
          <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
          <title>Your Medicare Coverage Update</title>
          <style type="text/css">
            /* GLOBAL RESETS */
            body {
              margin: 0;
              padding: 0;
              background-color: #f8f8f8;
              font-family: Arial, sans-serif;
            }
            table {
              border-collapse: collapse;
            }
            img {
              display: block;
              border: 0;
              outline: none;
              text-decoration: none;
              -ms-interpolation-mode: bicubic;
            }
            a {
              text-decoration: none;
              color: inherit;
            }
            h1, h2, h3, h4, h5, h6 {
              margin: 0;
              padding: 0;
              font-weight: bold;
            }

            /* CONTAINER STYLES */
            .email-container {
              width: 100%;
              max-width: 600px;
              background-color: #ffffff;
              border-radius: 8px;
              overflow: hidden;
            }

            /* CONTENT STYLES */
            .content {
              padding: 30px 40px;
              text-align: left;
              color: #333333;
              font-size: 16px;
              line-height: 24px;
            }
            .logo img {
              max-width: 100px;
              margin-bottom: 20px;
              display: block;
            }
            .message {
              margin: 0 0 30px 0;
            }
            /* BUTTON STYLES */
            .button-wrapper {
              text-align: left;
              margin-top: 20px;
              margin-bottom: 25px;
            }
            .cta-button {
              color: #ffffff !important;
              padding: 14px 28px;
              font-size: 16px;
              font-weight: bold;
              border-radius: 4px;
              display: inline-block;
              text-decoration: none;
              box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }
          </style>
        </head>
        <body style="margin:0; padding:0; background-color:#f8f8f8;">
          <table width="100%" border="0" cellspacing="0" cellpadding="0" bgcolor="#f8f8f8">
            <tr>
              <td align="center" style="padding: 40px 0;">
                <table class="email-container" border="0" cellspacing="0" cellpadding="0">
                  <tr>
                    <td>
                      <div class="content">
                        ${logoHtml}
                        
                        <div class="message">
                          <p>Hi ${firstName},</p>
                          <p>We recently reviewed Medigap premiums for your zip code and found some options that might interest you.</p>
                          <p>These plans offer the same comprehensive benefits you currently enjoy, potentially at a better value. We've done the research to find plans that maintain your coverage while possibly reducing your costs.</p>
                        </div>
                        
                        <div class="button-wrapper">
                          <a href="${quoteUrl}" target="_blank" class="cta-button" style="background-color:rgb(3, 3, 20); color: #ffffff !important; text-decoration: none; font-weight: bold;">  
                            Review Your Medicare Options
                          </a>
                        </div>
                        
                        <div class="message" style="margin-top: 30px;">
                          <p>Many Medicare beneficiaries don't realize they can be paying different rates for identical coverage. We'd be happy to show you your options and potential savings. If we don't find a better value now, we'll keep monitoring rates and reach out when we find something promising.</p>
                          ${formattedPhone ? `<p>If you have any questions, give me a call: ${formattedPhone}</p>` : ''}
                          <p style="margin-top: 25px; margin-bottom: 5px;">
                            Best,<br>
                            <strong>${orgName}</strong><br>
                            Your Medicare Specialist<br>
                            ${formattedPhone ? `<span style="color: #444;">${formattedPhone}</span>` : ''}
                          </p>
                        </div>

                        <div style="font-size: 12px; color: #666666; margin-top: 30px; padding-top: 15px; border-top: 1px solid #eeeeee;">
                          <p style="margin: 5px 0;">
                            Medicare Services<br>
                            ${formattedPhone ? `Phone: <a href="tel:${phone}" style="color: #0066cc; text-decoration: none;">${formattedPhone}</a><br>` : ''}
                            ${website ? `Website: <a href="${websiteUrl}" target="_blank" style="color: #0066cc; text-decoration: underline;">${website}</a>` : ''}
                          </p>
                        </div>
                      </div>
                    </td>
                  </tr>
                </table>
              </td>
            </tr>
          </table>
        </body>
        </html>
      `;
      
      const result = await sgMail.send(msg);
      logger.info(`Quote email sent successfully to ${email}`);
      
      // Return the SendGrid response and message ID if available
      return { 
        success: true,
        messageId: result && result[0] && result[0].headers ? result[0].headers['x-message-id'] : undefined
      };
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
    const fromEmail = process.env.SENDGRID_FROM_EMAIL || 'information@medicaremax.ai';
    const msg = {
      to: email,
      from: {
        email: fromEmail,
        name: 'MedicareMax'
      },
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
 * @param orgName Optional organization name to use as sender name
 */
export async function sendOnboardingLink(orgId: string, email?: string, orgName?: string) {
  try {
    // Base onboarding URL with orgId
    const baseUrl = process.env.FRONTEND_URL || 'https://medicaremax.ai';
    let onboardingUrl = `${baseUrl}/onboard?orgId=${orgId}`;
    
    // If email is provided, add email and hash to the URL
    if (email) {
      const hash = generateHash(orgId, email);
      onboardingUrl += `&email=${encodeURIComponent(email)}&hash=${hash}`;
    }
    
    const fromEmail = process.env.SENDGRID_FROM_EMAIL || 'information@medicaremax.ai';
    // Create email message
    const msg = {
      to: email || process.env.FALLBACK_EMAIL || 'information@medicaremax.ai', // Fallback if no email provided
      from: {
        email: fromEmail,
        name: orgName || 'MedicareMax'
      },
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