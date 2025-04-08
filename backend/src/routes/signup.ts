import { Elysia } from 'elysia';
import { logger } from '../logger';
import { Database } from '../database';
import { EmailService } from '../services/email';
import { AuthService } from '../services/auth';
import { cookie } from '@elysiajs/cookie';

// Enhanced slug generation with uniqueness check
async function generateUniqueSlug(db: Database, name: string): Promise<string> {
  let slug = name
    .toLowerCase()
    .trim()
    .replace(/[^a-z0-9]+/g, '-') // Replace non-alphanumeric chars with hyphens
    .replace(/^-+|-+$/g, '') // Remove leading/trailing hyphens
    .substring(0, 50); // Limit length

  // Check if slug exists
  let counter = 0;
  let uniqueSlug = slug;
  
  while (true) {
    const existing = await db.query<{ count: number }>(
      'SELECT COUNT(*) as count FROM organizations WHERE slug = ?',
      [uniqueSlug]
    );

    if (existing[0]?.count === 0) {
      break;
    }

    counter++;
    uniqueSlug = `${slug}-${counter}`;
  }

  return uniqueSlug;
}

// Create a standalone email check handler that doesn't require authentication
// This function can be directly mounted at the root level
export async function checkEmailHandler({ params, set }: { params: { email: string }, set: { status: number } }) {
  try {
    const { email } = params;
    const dbInstance = new Database();
    
    logger.info(`Public email check for: "${email}"`);
    
    if (!email || !email.trim()) {
      set.status = 400;
      return {
        available: false,
        message: 'Email is required'
      };
    }
    
    // Basic email format validation
    if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)) {
      logger.info(`Email validation failed for: "${email}"`);
      return {
        available: false,
        message: 'Invalid email format'
      };
    }
    
    // Check if email already exists
    const existingUser = await dbInstance.query<{ count: number }>(
      'SELECT COUNT(*) as count FROM users WHERE LOWER(email) = LOWER(?)',
      [email]
    );
    
    const count = existingUser[0]?.count || 0;
    logger.info(`Email check query result count: ${count} for email: "${email}"`);
    
    if (count > 0) {
      logger.info(`Email "${email}" is already registered`);
      return {
        available: false,
        message: 'This email address is already registered'
      };
    }
    
    logger.info(`Email "${email}" is available`);
    return {
      available: true,
      message: 'Email is available'
    };
    
  } catch (error) {
    logger.error(`Error checking email availability: ${error}`);
    set.status = 500;
    return {
      available: false,
      message: 'Failed to check email availability'
    };
  }
}

// Simple function to validate the token structure without strict verification
// For signup flow, we're just making sure the token format is valid
const validateSignupToken = (token: string): boolean => {
  try {
    // Token should have the format stored in our magic link
    const parts = token.split(':');
    return parts.length >= 2; // At minimum should have IV and encrypted data parts
  } catch (error) {
    logger.error(`Error validating signup token: ${error}`);
    return false;
  }
};

export function createSignupRoutes() {
  const dbInstance = new Database();
  const emailService = new EmailService();
  const authService = new AuthService();

  return new Elysia()
    .use(cookie())
    
    // New route for handling signup verification that just redirects to onboarding
    .get('/signup/verify', async ({ query, set }) => {
      try {
        // Extract query parameters
        const { token } = query as { 
          token: string, 
        };
        
        logger.info(`Processing signup verification for token: ${token}`);
        
        if (!token) {
          logger.warn('Missing token in signup verification');
          set.status = 400;
          set.redirect = '/signup';
          return { error: 'Invalid verification link' };
        }
        
        // Verify the token format is valid - simpler check for signup flow
        const { valid, email, redirectUrl } = await authService.verifySignupLink(token);
        
        logger.info(`Signup verified successfully, redirecting to: ${redirectUrl}`);
        set.redirect = redirectUrl;
        
        return {
          success: true,
          message: 'Email verified successfully'
        };
        
      } catch (error) {
        logger.error(`Error processing signup verification: ${error}`);
        set.status = 500;
        set.redirect = '/signup';
        return {
          success: false,
          message: 'Failed to verify email'
        };
      }
    })
    
    // Simplified signup process - only validate and send magic link
    .post('/api/signup', async ({ body, set }) => {
      try {
        const { name, email } = body as { name: string, email: string };
        
        logger.info(`Processing signup for name: "${name}", email: "${email}"`);
        
        if (!name || !name.trim() || !email || !email.trim()) {
          set.status = 400;
          return {
            success: false,
            message: 'Name and email are required'
          };
        }
        
        // Basic email format validation
        if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)) {
          logger.info(`Email validation failed for: "${email}"`);
          set.status = 400;
          return {
            success: false,
            message: 'Invalid email format'
          };
        }
        
        // Check if email already exists
        const existingUser = await dbInstance.query<{ count: number }>(
          'SELECT COUNT(*) as count FROM users WHERE LOWER(email) = LOWER(?)',
          [email]
        );
        
        if (existingUser[0]?.count > 0) {
          logger.info(`Email "${email}" is already registered`);
          set.status = 400;
          return {
            success: false,
            message: 'This email address is already registered'
          };
        }
        
        // Split name into first and last
        const nameParts = name.trim().split(' ');
        const firstName = nameParts[0];
        const lastName = nameParts.length > 1 ? nameParts.slice(1).join(' ') : '';
        
        // Generate magic link with user info in query parameters
        const magicLink = await authService.createSignupLink(
          email,
          { 
            // Use signup-verify instead of auth/verify to use a different flow
            redirectUrl: `/onboarding?firstName=${encodeURIComponent(firstName)}&lastName=${encodeURIComponent(lastName)}&email=${encodeURIComponent(email)}` 
          }
        );
        
        // Send the email with magic link
        await emailService.sendMagicLink(email, magicLink, firstName);
        
        logger.info(`Sent signup magic link to ${email}`);
        
        set.status = 201;
        return {
          success: true,
          message: 'Please check your email to continue with account setup.'
        };
        
      } catch (error) {
        logger.error(`Error processing signup: ${error}`);
        set.status = 500;
        return {
          success: false,
          message: 'Failed to create account'
        };
      }
    });
} 