import { Elysia, t } from 'elysia';
import { logger } from '../logger';
import Database from '../services/unifiedDatabase';
import fs from 'fs/promises';
import path from 'path';

const generateTempId = () => Math.random().toString(36).substring(2, 10);

// Initialize database - automatically uses Replit DB if available, otherwise JSON mock
const db = new Database();

export const createStageDemoRoutes = () => {
  return new Elysia({ prefix: '/api/stage-demo' })
    .post('/submit', async ({ body, set }) => {
      try {
        const { firstName, lastName, email, phone } = body as {
          firstName: string;
          lastName: string;
          email: string;
          phone: string;
        };

        if (!firstName || !lastName || !email || !phone) {
          set.status = 400;
          return { success: false, error: 'All fields are required.' };
        }

        logger.info(`Stage Demo: Submission received for ${email}`);

        const tempQuoteId = generateTempId();
        const quoteLink = `${process.env.PUBLIC_URL || 'http://localhost:5173'}/stage-demo/quote/${tempQuoteId}`;

        // Store the submission data
        const submissionData = {
          firstName,
          lastName,
          email,
          phone,
          submittedAt: new Date().toISOString(),
          quoteId: tempQuoteId,
          quoteLink
        };

        await db.set(`stage_demo_${tempQuoteId}`, submissionData);
        logger.info(`Stage Demo: Stored submission data for ${email} with ID ${tempQuoteId}`);

        const mailData = {
          to: email,
          subject: "Your MedicareMax Demo Quote",
          html: `<p>Hi ${firstName},</p>
<p>Thanks for your interest! Here's your personalized Medicare quote:</p>
<p><a href="${quoteLink}" style="background-color: #7C3AED; color: white; padding: 12px 24px; text-decoration: none; border-radius: 6px; display: inline-block;">View Your Quote</a></p>
<p>Best,<br>The MedicareMax Team</p>`,
          text: `Hi ${firstName},\n\nThanks for your interest! Here's your personalized Medicare quote: ${quoteLink}\n\nBest,\nThe MedicareMax Team`
        };

        // Send email using SendGrid directly
        const sgMail = require('@sendgrid/mail');
        sgMail.setApiKey(process.env.SENDGRID_API_KEY);
        
        await sgMail.send({
          to: email,
          from: process.env.SENDGRID_FROM_EMAIL || 'noreply@medicaremax.ai',
          subject: mailData.subject,
          text: mailData.text,
          html: mailData.html
        });
        logger.info(`Stage Demo: Email sent to ${email}`);

        // SMS sending - format phone to E.164 if needed
        const formattedPhone = phone.replace(/\D/g, '');
        const e164Phone = formattedPhone.startsWith('1') ? `+${formattedPhone}` : `+1${formattedPhone}`;
        
        // Send SMS using Twilio if configured
        if (process.env.TWILIO_ACCOUNT_SID && process.env.TWILIO_AUTH_TOKEN && process.env.TWILIO_PHONE_NUMBER) {
          try {
            const twilio = require('twilio');
            const client = twilio(process.env.TWILIO_ACCOUNT_SID, process.env.TWILIO_AUTH_TOKEN);
            
            await client.messages.create({
              body: `Hi ${firstName}, here's your MedicareMax quote: ${quoteLink}`,
              from: process.env.TWILIO_PHONE_NUMBER,
              to: e164Phone
            });
            logger.info(`Stage Demo: SMS sent to ${e164Phone}`);
          } catch (smsError) {
            logger.warn(`Stage Demo: SMS sending failed: ${smsError}`);
          }
        } else {
          logger.warn(`Stage Demo: SMS not configured - missing: ${!process.env.TWILIO_ACCOUNT_SID ? 'TWILIO_ACCOUNT_SID ' : ''}${!process.env.TWILIO_AUTH_TOKEN ? 'TWILIO_AUTH_TOKEN ' : ''}${!process.env.TWILIO_PHONE_NUMBER ? 'TWILIO_PHONE_NUMBER' : ''}`);
        }
        set.status = 200;
        return { success: true, message: 'Check your email and texts for your personalized quote!' };
      } catch (error) {
        logger.error(`Stage Demo Error: ${error}`);
        set.status = 500;
        return { success: false, error: 'An error occurred. Please try again.' };
      }
    }, {
      body: t.Object({
        firstName: t.String(),
        lastName: t.String(),
        email: t.String({ format: 'email' }),
        phone: t.String()
      })
    })
    .get('/submissions', async ({ set }) => {
      try {
        // Get all stage demo submissions
        const listResult = await db.list('stage_demo_');
        
        if (!listResult.ok) {
          set.status = 500;
          return { success: false, error: 'Failed to retrieve submissions' };
        }

        const submissions = [];
        for (const key of listResult.value) {
          const dataResult = await db.get(key);
          if (dataResult.ok && dataResult.value) {
            submissions.push({
              id: key.replace('stage_demo_', ''),
              ...dataResult.value
            });
          }
        }

        // Sort by submission date (newest first)
        submissions.sort((a, b) => 
          new Date(b.submittedAt).getTime() - new Date(a.submittedAt).getTime()
        );

        return { 
          success: true, 
          count: submissions.length,
          submissions 
        };
      } catch (error) {
        logger.error(`Stage Demo Error: ${error}`);
        set.status = 500;
        return { success: false, error: 'Failed to retrieve submissions' };
      }
    })
    .get('/submission/:id', async ({ params, set }) => {
      try {
        const { id } = params;
        const result = await db.get(`stage_demo_${id}`);
        
        if (!result.ok) {
          set.status = 500;
          return { success: false, error: 'Failed to retrieve submission' };
        }

        if (!result.value) {
          set.status = 404;
          return { success: false, error: 'Submission not found' };
        }

        return { 
          success: true, 
          submission: {
            id,
            ...result.value
          }
        };
      } catch (error) {
        logger.error(`Stage Demo Error: ${error}`);
        set.status = 500;
        return { success: false, error: 'Failed to retrieve submission' };
      }
    })
    .get('/plans', async ({ set }) => {
      try {
        const filePath = path.join(__dirname, '../../data/plans.json');
        const fileContent = await fs.readFile(filePath, 'utf-8');
        const plansData = JSON.parse(fileContent);
        return plansData;
      } catch (error) {
        logger.error(`Stage Demo Plans Error: ${error}`);
        set.status = 500;
        return { success: false, error: 'Failed to retrieve plans data.' };
      }
    }, {
      beforeHandle: []
    });
};