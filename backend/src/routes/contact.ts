import { Elysia } from "elysia";
import sgMail from '@sendgrid/mail';
import { logger } from '../logger';

// Initialize SendGrid
if (process.env.SENDGRID_API_KEY) {
  sgMail.setApiKey(process.env.SENDGRID_API_KEY);
}

export const contactUsRoutes = new Elysia({ prefix: "/api/contact-us" })
  .post("/", async ({ body, set }) => {
    try {
      const { name, email, phone, message } = body as {
        name: string;
        email: string;
        phone?: string;
        message: string;
      };

      // Validate required fields
      if (!name || !email || !message) {
        set.status = 400;
        return { error: "Missing required fields" };
      }

      // Send email notification to support
      const fromEmail = 'information@medicaremax.ai';
      
      await sgMail.send({
        to: "information@medicaremax.ai",
        from: {
          email: fromEmail,
          name: 'Medicare Max Contact Form'
        },
        subject: `Contact Form Submission from ${name}`,
        html: `
          <h2>New Contact Form Submission</h2>
          <p><strong>Name:</strong> ${name}</p>
          <p><strong>Email:</strong> ${email}</p>
          ${phone ? `<p><strong>Phone:</strong> ${phone}</p>` : ""}
          <p><strong>Message:</strong></p>
          <p>${message.replace(/\n/g, "<br>")}</p>
        `,
      });

      // Send confirmation email to the user
      await sgMail.send({
        to: email,
        from: {
          email: fromEmail,
          name: 'Medicare Max'
        },
        subject: "Thank you for contacting Medicare Max",
        html: `
          <h2>Thank you for contacting us!</h2>
          <p>Dear ${name},</p>
          <p>We have received your message and will get back to you as soon as possible.</p>
          <p>Here's a copy of your message:</p>
          <blockquote style="background: #f0f0f0; padding: 10px; margin: 10px 0;">
            ${message.replace(/\n/g, "<br>")}
          </blockquote>
          <p>Best regards,<br>Medicare Max Team</p>
        `,
      });
      
      logger.info(`Contact form submitted by ${name} (${email})`);

      return { success: true };
    } catch (error) {
      console.error("Error processing contact form:", error);
      set.status = 500;
      return { error: "Failed to process contact form" };
    }
  });