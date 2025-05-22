import { Elysia } from 'elysia';
import { Database } from '../database';
import { TursoService } from '../services/turso';
import { z } from 'zod';
import { sendMagicLink } from '../services/email';
import { generateToken, getUserFromSession } from '../services/auth';
import { logger } from '../logger';
import { AuthService } from '../services/auth';
import { config } from '../config';
import sgMail from '@sendgrid/mail';
import { cookie } from '@elysiajs/cookie';

// Update the validation schema to include slug rules
const signupSchema = z.object({
  organizationName: z.string()
    .min(1, "Organization name is required")
    .max(100, "Organization name must be less than 100 characters")
    .regex(/^[a-zA-Z0-9\s\-_.]+$/, "Organization name can only contain letters, numbers, spaces, hyphens, dots, and underscores"),
  adminFirstName: z.string().min(1, "First name is required"),
  adminLastName: z.string().min(1, "Last name is required"),
  adminEmail: z.string().email("Invalid email address"),
  phone: z.string().optional(),
  planType: z.string().optional(),
});

// Helper function to ensure emails are properly decoded before storing in the database
function ensureEmailDecoded(email: string): string {
  try {
    // First check if email contains encoded characters
    if (email.includes('%')) {
      const decoded = decodeURIComponent(email);
      // Check if decoding made a difference
      if (decoded !== email) {
        logger.info(`Decoded email from ${email} to ${decoded}`);
        return decoded;
      }
    }
    return email;
  } catch (error) {
    logger.warn(`Error decoding email ${email}: ${error}`);
    return email; // Return original if decoding fails
  }
}

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

// Add this function to handle the mapping from old tier to new contact-based tier
function mapLegacyTierToContactTier(tierId: string): number {
  // Extract tier number from ID if format is "tier-X"
  if (tierId.startsWith('tier-')) {
    const tierNumber = parseInt(tierId.substring(5), 10);
    if (!isNaN(tierNumber)) {
      return tierNumber;
    }
  }
  
  // Handle legacy tiers
  switch (tierId) {
    case 'basic':
      return 1; // 500 contacts
    case 'pro':
      return 2; // 1000 contacts
    case 'enterprise':
      return 5; // 2500 contacts
    default:
      return 1; // Default to Tier 1
  }
}

// Add this function to calculate the agent limit based on contact tier
function getAgentLimitForContactTier(contactTier: number): number {
  switch (contactTier) {
    case 1: return 5;    // 500 contacts
    case 2: return 10;   // 1,000 contacts
    case 5: return 25;   // 2,500 contacts
    case 10: return 50;  // 5,000 contacts
    case 20: return 100; // 10,000 contacts
    case 50: return 200; // 25,000 contacts
    case 100: return 300; // 50,000 contacts
    case 200: return 500; // 100,000 contacts
    default:
      // Formula for tiers not explicitly defined
      if (contactTier <= 0) return 5;
      if (contactTier <= 10) return contactTier * 5;
      if (contactTier <= 50) return contactTier * 4;
      if (contactTier <= 100) return contactTier * 3;
      return contactTier * 2.5; // For very high tiers
  }
}

export const organizationRoutes = new Elysia({ prefix: '/api' })
  .post('/organizations/signup', async ({ body, set }) => {
    const db = new Database();
    const turso = new TursoService();
    const auth = new AuthService();

    try {
      logger.info(`Attempting to create organization with data: ${JSON.stringify(body)}`);
      const data = signupSchema.parse(body);
      
      // Decode email to ensure it's stored properly
      const decodedEmail = ensureEmailDecoded(data.adminEmail);
      
      // Generate unique slug
      const slug = await generateUniqueSlug(db, data.organizationName);
      logger.info(`Generated unique slug: ${slug}`);
      
      // Check if email is already registered in any organization
      const existingUser = await db.query<{ count: number }>(
        'SELECT COUNT(*) as count FROM users WHERE LOWER(email) = LOWER(?)',
        [decodedEmail]
      );

      logger.info(`Existing user check result: ${JSON.stringify(existingUser)}`);

      if (existingUser[0]?.count > 0) {
        logger.warn(`Email ${decodedEmail} is already registered`);
        set.status = 400;
        return {
          success: false,
          message: 'This email address is already registered. Please use a different email or contact support.'
        };
      }

      // Check if organization name or slug is taken
      const existingOrg = await db.query<{ count: number }>(
        'SELECT COUNT(*) as count FROM organizations WHERE name = ? OR slug = ?',
        [data.organizationName, slug]
      );

      logger.info(`Existing org check result: ${JSON.stringify(existingOrg)}`);

      if (existingOrg[0]?.count > 0) {
        logger.warn(`Organization name ${data.organizationName} or slug ${slug} is already taken`);
        set.status = 400;
        return {
          success: false,
          message: 'Organization name is already taken'
        };
      }

      // Wrap all database operations in a transaction
      const orgId = await db.transaction('write', async (transactionDb) => {
        // Create organization
        logger.info('Creating organization');
        const org = await transactionDb.execute(
          `INSERT INTO organizations (
            name,
            slug,
            subscription_tier,
            agent_limit,
            contact_limit
          ) VALUES (?, ?, 'basic', 5, 100) RETURNING id`,
          [data.organizationName, slug]
        );

        const orgId = org.rows?.[0]?.id;
        if (!orgId) {
          throw new Error('Failed to create organization');
        }

        logger.info(`Organization created with ID: ${orgId}`);

        // Create inactive admin user
        logger.info('Creating admin user');
        await transactionDb.execute(
          `INSERT INTO users (
            email,
            organization_id,
            is_admin,
            is_agent,
            is_active,
            first_name,
            last_name,
            created_at
          ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
          [
            decodedEmail,
            orgId,
            1, // is_admin
            1, // is_agent - Set to 1 for basic tier since admin is also an agent
            1, // is_active
            data.adminFirstName,
            data.adminLastName,
            new Date().toISOString()
          ]
        );

        return orgId;
      });

      // Return success response without sending magic link
      set.status = 201;
      return { 
        success: true,
        message: 'Organization created successfully',
        slug: slug
      };

    } catch (error) {
      logger.error(`Organization creation error: ${error}`);
      set.status = 400;
      return {
        success: false,
        message: error instanceof z.ZodError 
          ? error.errors.map(e => e.message).join(', ')
          : 'Failed to create organization. Please try again.'
      };
    }
  })
  .get('/organizations/check-name/:name', async ({ params, set }) => {
    const db = new Database();

    try {
      const decodedName = decodeURIComponent(params.name);
      const potentialSlug = decodedName
        .toLowerCase()
        .replace(/[^a-z0-9]+/g, '-')
        .replace(/^-+|-+$/g, '');

      const existingOrg = await db.query<{ count: number }>(
        'SELECT COUNT(*) as count FROM organizations WHERE name = ? OR slug = ?',
        [decodedName, potentialSlug]
      );

      const count = existingOrg[0]?.count || 0;

      if (count > 0) {
        return {
          available: false,
          message: 'Organization name is already taken'
        };
      }

      // Validate name format
      if (!/^[a-zA-Z0-9\s\-_.]+$/.test(decodedName)) {
        return {
          available: false,
          message: 'Organization name can only contain letters, numbers, spaces, hyphens, dots, and underscores'
        };
      }

      return {
        available: true,
        message: 'Organization name is available'
      };

    } catch (error) {
      logger.error(`Error checking organization name: ${error}`);
      set.status = 500;
      return {
        available: false,
        message: 'Failed to check organization name'
      };
    }
  })
  .get('/organizations/check-email/:email', async ({ params, set }) => {
    const db = new Database();

    try {
      const decodedEmailParam = decodeURIComponent(params.email);
      // Ensure the email is fully decoded
      const decodedEmail = ensureEmailDecoded(decodedEmailParam);
      
      logger.info(`Checking email availability for: "${decodedEmail}"`);
      
      // Basic email format validation
      if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(decodedEmail)) {
        logger.info(`Email validation failed for: "${decodedEmail}"`);
        return {
          available: false,
          message: 'Invalid email format'
        };
      }

      // Use LOWER() for case-insensitive comparison to match unique constraint behavior
      logger.info(`Running SQL query: SELECT COUNT(*) as count FROM users WHERE LOWER(email) = LOWER('${decodedEmail}')`);
      const existingUser = await db.query<{ count: number }>(
        'SELECT COUNT(*) as count FROM users WHERE LOWER(email) = LOWER(?)',
        [decodedEmail]
      );

      const count = existingUser[0]?.count || 0;
      logger.info(`Email check query result count: ${count} for email: "${decodedEmail}"`);

      if (count > 0) {
        logger.info(`Email "${decodedEmail}" is already registered`);
        return {
          available: false,
          message: 'This email address is already registered'
        };
      }

      logger.info(`Email "${decodedEmail}" is available`);
      return {
        available: true,
        message: 'Email is available'
      };

    } catch (error) {
      logger.error(`Error checking email: ${error}`);
      set.status = 500;
      return {
        available: false,
        message: 'Failed to check email availability'
      };
    }
  })
  .get('/organizations/subscription-tiers', async ({ set }) => {
    try {
      // Define a broader range of contact-based pricing tiers
      const tiers = [
        {
          id: "tier-1",
          name: "Tier 1",
          price: "$60/mo",
          agentLimit: 5,
          contactLimit: 500,
          features: ["Up to 500 contacts", "Email scheduling", "CRM features", "Analytics dashboard", "5 agent accounts"]
        },
        {
          id: "tier-2",
          name: "Tier 2",
          price: "$100/mo", 
          agentLimit: 10,
          contactLimit: 1000,
          features: ["Up to 1,000 contacts", "Email scheduling", "CRM features", "Analytics dashboard", "10 agent accounts"]
        },
        {
          id: "tier-5",
          name: "Tier 5",
          price: "$220/mo",
          agentLimit: 25,
          contactLimit: 2500, 
          features: ["Up to 2,500 contacts", "Email scheduling", "CRM features", "Premium support", "25 agent accounts"]
        },
        {
          id: "tier-10",
          name: "Tier 10",
          price: "$420/mo",
          agentLimit: 50,
          contactLimit: 5000,
          features: ["Up to 5,000 contacts", "Email scheduling", "CRM features", "Premium support", "Priority service", "50 agent accounts"]
        },
        {
          id: "tier-20",
          name: "Tier 20",
          price: "$820/mo",
          agentLimit: 100,
          contactLimit: 10000,
          features: ["Up to 10,000 contacts", "Email scheduling", "CRM features", "Premium support", "Priority service", "100 agent accounts"]
        },
        {
          id: "tier-50",
          name: "Tier 50",
          price: "$2020/mo",
          agentLimit: 200,
          contactLimit: 25000,
          features: ["Up to 25,000 contacts", "Email scheduling", "CRM features", "Premium support", "Priority service", "Dedicated account manager", "200 agent accounts"]
        },
        {
          id: "tier-100",
          name: "Tier 100",
          price: "$3820/mo",
          agentLimit: 300,
          contactLimit: 50000,
          features: ["Up to 50,000 contacts", "Email scheduling", "CRM features", "Premium support", "Priority service", "Dedicated account manager", "300 agent accounts"]
        },
        {
          id: "tier-200",
          name: "Tier 200",
          price: "$7620/mo",
          agentLimit: 500,
          contactLimit: 100000,
          features: ["Up to 100,000 contacts", "Email scheduling", "CRM features", "Premium support", "Priority service", "Dedicated account manager", "Custom integrations", "500 agent accounts"]
        }
      ];

      // Add an endpoint for calculating custom tier pricing
      return { success: true, tiers };
    } catch (error) {
      logger.error(`Error fetching subscription tiers: ${error}`);
      set.status = 500;
      return { success: false, error: 'Failed to fetch subscription tiers' };
    }
  })
  .get('/organizations/:orgSlug/subscription', async ({ params, request, set }) => {
    try {
      const db = new Database();
      const orgSlug = params.orgSlug;
      
      logger.info(`GET /organizations/${orgSlug}/subscription - Request received`);
      
      // Get session cookie for debugging
      const sessionCookie = request.headers.get('cookie');
      logger.info(`Session cookie: ${sessionCookie ? 'Present' : 'Missing'}`);
      
      // Get current user from session to determine their org
      const currentUser = await getUserFromSession(request);
      logger.info(`User authentication result: ${currentUser ? `Authenticated as ${currentUser.email}` : 'Not authenticated'}`);
      
      if (!currentUser) {
        set.status = 401;
        logger.error('Subscription fetch failed: User not authenticated');
        return {
          success: false,
          error: 'You must be logged in to perform this action'
        };
      }

      // Log request information for debugging
      logger.info(`Fetching subscription - orgSlug: ${orgSlug}, userId: ${currentUser.id}, orgId: ${currentUser.organization_id}`);
      
      // Get organization details
      const orgResult = await db.query<{ 
        id: number,
        subscription_tier: string,
        agent_limit: number,
        contact_limit: number
      }>(
        'SELECT id, subscription_tier, agent_limit, contact_limit FROM organizations WHERE slug = ?',
        [orgSlug]
      );

      if (!orgResult || orgResult.length === 0) {
        set.status = 404;
        logger.error(`Subscription fetch failed: Organization not found - ${orgSlug}`);
        return {
          success: false,
          error: 'Organization not found'
        };
      }

      const organization = orgResult[0];
      
      // Verify user has permission for this org
      if (organization.id !== currentUser.organization_id) {
        logger.error(`Subscription fetch failed: Permission denied - User from org ${currentUser.organization_id} attempted to access org ${organization.id}`);
        set.status = 403;
        return {
          success: false,
          error: 'You do not have permission to view this organization'
        };
      }
      
      // Set up the response with subscription details
      const response = {
        success: true,
        tierId: organization.subscription_tier,
        agentLimit: organization.agent_limit,
        contactLimit: organization.contact_limit
      };

      logger.info(`Successfully fetched subscription for org ${organization.id}: tier=${organization.subscription_tier}, agents=${organization.agent_limit}, contacts=${organization.contact_limit}`);
      
      return response;
      
    } catch (error) {
      logger.error(`Error fetching organization subscription: ${error}`);
      set.status = 500;
      return {
        success: false,
        error: 'Failed to fetch subscription details'
      };
    }
  })
  .get('/organizations/my-subscription', async ({ request, set }) => {
    try {
      const db = new Database();
      
      logger.info('GET /organizations/my-subscription - Request received');
      
      // Get current user from session
      const currentUser = await getUserFromSession(request);
      logger.info(`User authentication result: ${currentUser ? `Authenticated as ${currentUser.email}` : 'Not authenticated'}`);
      
      if (!currentUser) {
        set.status = 401;
        logger.error('My subscription fetch failed: User not authenticated');
        return {
          success: false,
          error: 'You must be logged in to perform this action'
        };
      }

      // Get organization details using user's organization_id
      const orgResult = await db.query<{ 
        id: number,
        name: string,
        slug: string,
        subscription_tier: string,
        agent_limit: number,
        contact_limit: number,
        stripe_subscription_id: string | null
      }>(
        'SELECT id, name, slug, subscription_tier, agent_limit, contact_limit, stripe_subscription_id FROM organizations WHERE id = ?',
        [currentUser.organization_id]
      );

      if (!orgResult || orgResult.length === 0) {
        set.status = 404;
        logger.error(`My subscription fetch failed: Organization not found for user ${currentUser.id}`);
        return {
          success: false,
          error: 'Organization not found'
        };
      }

      const organization = orgResult[0];
      
      // Get subscription tier details from subscription_tiers table
      const tierResult = await db.query<{
        name: string,
        agent_limit: number,
        contact_limit: number,
        features: string
      }>(
        'SELECT name, agent_limit, contact_limit, features FROM subscription_tiers WHERE id = ?',
        [organization.subscription_tier]
      );
      
      const tier = tierResult[0] || null;
      
      // Determine subscription status based on Stripe subscription
      const subscriptionStatus = organization.stripe_subscription_id ? 'active' : 'inactive';
      
      // Set up the response with complete subscription details
      const response = {
        success: true,
        organization: {
          id: organization.id,
          name: organization.name,
          slug: organization.slug
        },
        subscription: {
          tierId: organization.subscription_tier,
          tierName: tier?.name || organization.subscription_tier,
          status: subscriptionStatus,
          agentLimit: organization.agent_limit,
          contactLimit: organization.contact_limit,
          features: tier ? JSON.parse(tier.features) : []
        }
      };

      logger.info(`Successfully fetched my subscription for org ${organization.id}: tier=${organization.subscription_tier}, agents=${organization.agent_limit}, contacts=${organization.contact_limit}`);
      
      return response;
      
    } catch (error) {
      logger.error(`Error fetching my organization subscription: ${error}`);
      set.status = 500;
      return {
        success: false,
        error: 'Failed to fetch subscription details'
      };
    }
  })
  .post('/organizations/:orgSlug/subscription', async ({ params, body, request, set }) => {
    try {
      const db = new Database();
      const orgSlug = params.orgSlug;
      
      logger.info(`POST /organizations/${orgSlug}/subscription - Request received`);
      
      // Get current user from session to determine their org
      const currentUser = await getUserFromSession(request);
      logger.info(`User authentication result: ${currentUser ? `Authenticated as ${currentUser.email}` : 'Not authenticated'}`);
      
      if (!currentUser) {
        set.status = 401;
        logger.error('Subscription update failed: User not authenticated');
        return {
          success: false,
          error: 'You must be logged in to perform this action'
        };
      }

      // Get organization details
      const orgResult = await db.query<{ 
        id: number, 
        subscription_tier: string,
        stripe_customer_id: string | null
      }>(
        'SELECT id, subscription_tier, stripe_customer_id FROM organizations WHERE slug = ?',
        [orgSlug]
      );

      if (!orgResult || orgResult.length === 0) {
        set.status = 404;
        logger.error(`Subscription update failed: Organization not found - ${orgSlug}`);
        return {
          success: false,
          error: 'Organization not found'
        };
      }

      const organization = orgResult[0];
      
      // Verify user has permission for this org
      if (organization.id !== currentUser.organization_id) {
        logger.error(`Subscription update failed: Permission denied - User from org ${currentUser.organization_id} attempted to access org ${organization.id}`);
        set.status = 403;
        return {
          success: false,
          error: 'You do not have permission to update this organization'
        };
      }

      // Extract the tier ID and additional resources from the request body
      const { tierId, extraAgents, extraContacts } = body as { 
        tierId: 'basic' | 'pro' | 'enterprise' | string,
        extraAgents: number,
        extraContacts: number
      };
      
      // Map the legacy tier to a contact-based tier
      const contactTier = mapLegacyTierToContactTier(tierId);
      const agentLimit = getAgentLimitForContactTier(contactTier);
      
      logger.info(`Updating subscription - orgId: ${organization.id}, tier: ${tierId}, contactTier: ${contactTier}, agentLimit: ${agentLimit}`);

      // Check if we need to update the Stripe subscription
      if (organization.subscription_tier !== tierId) {
        logger.info(`Organization tier is changing from ${organization.subscription_tier} to ${tierId}`);
        
        // Update Stripe subscription
        const stripe = await import('../services/stripe');
        
        try {
          // Create or update the subscription with Stripe
          await stripe.createOrUpdateSubscription({
            tierId: tierId as any, // Type cast to avoid type error with new tier IDs
            organizationId: organization.id,
            email: currentUser.email,
            extraAgents,
            extraContacts,
            stripeCustomerId: organization.stripe_customer_id || undefined,
            userId: currentUser.id.toString() // Add the userId to fix the linter error
          });
          
          logger.info(`Successfully updated Stripe subscription for organization ${organization.id}`);
        } catch (stripeError) {
          logger.error(`Error updating Stripe subscription: ${stripeError}`);
        }
      } else {
        logger.info(`Organization tier is not changing, skipping Stripe update`);
      }

      // Update the organization's subscription tier in the database
      await db.execute(
        `UPDATE organizations 
         SET subscription_tier = ?,
             agent_limit = ?,
             contact_limit = ?
         WHERE id = ?`,
        [
          tierId,
          agentLimit,
          contactTier * 500, // Contact limit is based on tier
          organization.id
        ]
      );
      
      logger.info(`Successfully updated subscription in database for organization ${organization.id}`);
      
      // Return success
      return {
        success: true,
        message: 'Subscription updated successfully'
      };
      
    } catch (error) {
      logger.error(`Error updating organization subscription: ${error}`);
      set.status = 500;
      return {
        success: false,
        error: 'Failed to update subscription'
      };
    }
  })
  // Add new endpoint to create Turso database after plan selection
  .post('/organizations/:orgSlug/setup-database', async ({ params, set }) => {
    const db = new Database();
    const turso = new TursoService();

    try {
      const orgSlug = params.orgSlug;

      // Get organization ID from slug
      const orgResult = await db.query<{ id: number, has_db: number }>(
        'SELECT id, CASE WHEN turso_db_url IS NOT NULL THEN 1 ELSE 0 END as has_db FROM organizations WHERE slug = ?',
        [orgSlug]
      );

      if (!orgResult || orgResult.length === 0) {
        set.status = 404;
        return {
          success: false,
          message: 'Organization not found'
        };
      }

      const orgId = orgResult[0].id;

      if (orgResult[0].has_db === 1) {
        set.status = 400;
        return {
          success: false,
          message: 'Organization already has a database'
        };
      }

      // Create Turso database for the organization
      const { url, token } = await turso.createOrganizationDatabase(orgId.toString());

      // Update organization with Turso database credentials and default org_signature to false
      await db.execute(
        'UPDATE organizations SET turso_db_url = ?, turso_auth_token = ?, org_signature = 0 WHERE id = ?',
        [url, token, orgId]
      );

      logger.info(`Successfully created Turso database for organization ${orgId}`);

      return {
        success: true,
        message: 'Database created successfully'
      };

    } catch (error) {
      logger.error(`Error creating database for organization ${params.orgSlug}: ${error}`);
      set.status = 500;
      const errorMessage = error instanceof Error ? error.message : 'Unknown error';
      return {
        success: false,
        message: `Failed to create database: ${errorMessage}`
      };
    }
  })
  // Get organization account status
  .get('/organizations/:orgSlug/account-status', async ({ params, set, request }) => {
    try {
      const db = new Database();
      
      // Authenticate the request
      const currentUser = await getUserFromSession(request);
      if (!currentUser) {
        set.status = 401;
        return {
          success: false,
          error: 'You must be logged in to perform this action'
        };
      }
      
      // Get organization ID from slug
      const orgResult = await db.query<{ id: number }>(
        'SELECT id FROM organizations WHERE slug = ?',
        [params.orgSlug]
      );
      
      if (!orgResult || orgResult.length === 0) {
        set.status = 404;
        return {
          success: false,
          error: 'Organization not found'
        };
      }
      
      const organizationId = orgResult[0].id;
      
      // Verify user has permission to access this organization
      if (organizationId !== currentUser.organization_id) {
        set.status = 403;
        return {
          success: false,
          error: 'You do not have permission to access this organization'
        };
      }
      
      // Import the subscription service
      const { checkAccountStatus } = await import('../services/subscription');
      
      // Check account status
      const statusDetails = await checkAccountStatus(organizationId);
      
      return {
        success: true,
        status: statusDetails
      };
      
    } catch (error) {
      logger.error(`Error checking account status: ${error}`);
      set.status = 500;
      return {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error'
      };
    }
  })
  
  // Stripe webhook handler
  .post('/stripe-webhook', async ({ body, set, request }) => {
    try {
      const signature = request.headers.get('stripe-signature');
      
      if (!signature) {
        set.status = 400;
        return { success: false, error: 'Stripe signature missing' };
      }
      
      // Import Stripe to verify the webhook
      const Stripe = await import('stripe');
      const stripe = new Stripe.default(config.stripe.secretKey, {
        apiVersion: '2025-02-24.acacia',
      });
      
      const event = stripe.webhooks.constructEvent(
        JSON.stringify(body),
        signature,
        config.stripe.webhookSecret
      );
      
      // Import the Stripe service to handle the webhook
      const { handleStripeWebhook } = await import('../services/stripe');
      await handleStripeWebhook(event);
      
      logger.info(`Processed Stripe webhook: ${event.type}`);
      return { success: true };
      
    } catch (error) {
      logger.error(`Error processing Stripe webhook: ${error}`);
      set.status = 400;
      return { 
        success: false, 
        error: error instanceof Error ? error.message : 'Unknown error'
      };
    }
  })
  // Add interface for enterprise contact form data
  .post('/enterprise-contact', async ({ body, set, request }: { 
    body: { name: string; email: string; phone: string; company: string; companySize?: string; message?: string }, 
    set: { status: number },
    request: Request 
  }) => {
    try {
      const { name, email, phone, company, companySize, message } = body;
      
      // Validate required fields
      if (!name || !email || !phone || !company) {
        set.status = 400;
        return { success: false, error: 'Missing required fields' };
      }
      
      // Try to get user/organization info from session if available
      let orgInfo = "";
      try {
        const user = await getUserFromSession(request);
        if (user) {
          orgInfo = `
          <div style="margin-top: 20px; padding-top: 20px; border-top: 1px solid #eee;">
            <p><strong>User is logged in with the following details:</strong></p>
            <p>Organization: ${user.organization_name} (ID: ${user.organization_id})</p>
            <p>User: ${user.first_name} ${user.last_name} (${user.email})</p>
          </div>`;
        }
      } catch (sessionError) {
        // Just log the error but continue - the session info is optional
        logger.warn(`Unable to get session info: ${sessionError}`);
      }
      
      // Format the email content
      const emailContent = `
        <div style="font-family: Arial, sans-serif; max-width: 600px; margin: 0 auto;">
          <h2 style="color: #333;">New Enterprise Plan Inquiry</h2>
          
          <div style="margin: 20px 0; background-color: #f7f7f7; padding: 20px; border-radius: 5px;">
            <p><strong>Name:</strong> ${name}</p>
            <p><strong>Email:</strong> ${email}</p>
            <p><strong>Phone:</strong> ${phone}</p>
            <p><strong>Company:</strong> ${company}</p>
            <p><strong>Company Size:</strong> ${companySize || 'Not specified'}</p>
            <p><strong>Message:</strong></p>
            <p style="white-space: pre-line;">${message || 'No message provided'}</p>
          </div>
          ${orgInfo}
          <p style="color: #666; font-size: 14px;">
            This inquiry was submitted through the Enterprise Contact form on the MedicareMax portal.
          </p>
        </div>
      `;
      
      // Configure email
      const msg = {
        to: ['information@medicaremax.ai', 'reuben.brooks@medicaremax.ai'],
        from: process.env.SENDGRID_FROM_EMAIL || 'information@medicaremax.ai',
        subject: `Enterprise Plan Inquiry from ${name} at ${company}`,
        text: `New Enterprise Plan Inquiry:\n\nName: ${name}\nEmail: ${email}\nPhone: ${phone}\nCompany: ${company}\nCompany Size: ${companySize || 'Not specified'}\n\nMessage: ${message || 'No message provided'}\n\n${orgInfo ? `User is logged in from organization: ${orgInfo}` : ''}\n\nThis inquiry was submitted through the Enterprise Contact form on the MedicareMax portal.`,
        html: emailContent
      };
      
      // Send the email
      await sgMail.send(msg);
      
      // Log successful submission
      logger.info(`Enterprise plan inquiry submitted by ${name} from ${company}`);
      
      // Return success response
      return { success: true };
    } catch (error) {
      logger.error(`Error processing enterprise contact submission: ${error}`);
      set.status = 500;
      return { 
        success: false, 
        error: 'Failed to process your request. Please try again later.' 
      };
    }
  })
  // Add endpoint for enterprise inquiry during onboarding
  .post('/enterprise-inquiry', async ({ body, set, request }: { 
    body: { companyName: string; contactName: string; email: string; phone: string; message?: string }, 
    set: { status: number },
    request: Request 
  }) => {
    try {
      const { companyName, contactName, email, phone, message } = body;
      
      // Validate required fields
      if (!companyName || !contactName || !email || !phone) {
        set.status = 400;
        return { success: false, error: 'Missing required fields' };
      }
      
      // Try to get user/organization info from session if available
      let orgInfo = "";
      try {
        const user = await getUserFromSession(request);
        if (user) {
          orgInfo = `
          <div style="margin-top: 20px; padding-top: 20px; border-top: 1px solid #eee;">
            <p><strong>User is logged in with the following details:</strong></p>
            <p>Organization: ${user.organization_name} (ID: ${user.organization_id})</p>
            <p>User: ${user.first_name} ${user.last_name} (${user.email})</p>
          </div>`;
        }
      } catch (sessionError) {
        // Just log the error but continue - the session info is optional
        logger.warn(`Unable to get session info: ${sessionError}`);
      }
      
      // Format the email content
      const emailContent = `
        <div style="font-family: Arial, sans-serif; max-width: 600px; margin: 0 auto;">
          <h2 style="color: #333;">New Enterprise Plan Inquiry (Onboarding)</h2>
          
          <div style="margin: 20px 0; background-color: #f7f7f7; padding: 20px; border-radius: 5px;">
            <p><strong>Company:</strong> ${companyName}</p>
            <p><strong>Contact Name:</strong> ${contactName}</p>
            <p><strong>Email:</strong> ${email}</p>
            <p><strong>Phone:</strong> ${phone}</p>
            <p><strong>Message:</strong></p>
            <p style="white-space: pre-line;">${message || 'No message provided'}</p>
          </div>
          ${orgInfo}
          <p style="color: #666; font-size: 14px;">
            This inquiry was submitted through the Enterprise Form during the onboarding process on MedicareMax.
          </p>
        </div>
      `;
      
      // Configure email
      const msg = {
        to: ['information@medicaremax.ai', 'reuben.brooks@medicaremax.ai'],
        from: process.env.SENDGRID_FROM_EMAIL || 'information@medicaremax.ai',
        subject: `Enterprise Plan Inquiry from ${contactName} at ${companyName} (Onboarding)`,
        text: `New Enterprise Plan Inquiry (Onboarding):\n\nCompany: ${companyName}\nContact Name: ${contactName}\nEmail: ${email}\nPhone: ${phone}\n\nMessage: ${message || 'No message provided'}\n\n${orgInfo ? `User is logged in from organization: ${orgInfo}` : ''}\n\nThis inquiry was submitted through the Enterprise Form during onboarding on MedicareMax.`,
        html: emailContent
      };
      
      // Send the email
      await sgMail.send(msg);
      
      // Log successful submission
      logger.info(`Enterprise plan inquiry (onboarding) submitted by ${contactName} from ${companyName}`);
      
      // Return success response
      return { success: true };
    } catch (error) {
      logger.error(`Error processing enterprise inquiry submission: ${error}`);
      set.status = 500;
      return { success: false, error: 'Failed to process inquiry' };
    }
  })
  .post('/api/agents', async ({ body, request, set }) => {
    try {
      // Get current user from session to determine their org
      const currentUser = await getUserFromSession(request)
      if (!currentUser) {
        set.status = 401
        return {
          success: false,
          error: 'You must be logged in to perform this action'
        }
      }

      // Check if user is an admin
      if (!currentUser.is_admin) {
        set.status = 403
        return {
          success: false,
          error: 'Only administrators can create new agents'
        }
      }

      const newAgent = body as NewAgentRequest
      
      // Decode the email to ensure it's stored properly
      newAgent.email = ensureEmailDecoded(newAgent.email);
      
      logger.info(`Creating new agent: ${newAgent.email} (org: ${currentUser.organization_id})`)

      // Ensure that the new user has at least one role
      if (!newAgent.is_admin && !newAgent.is_agent) {
        logger.warn(`Agent created without any roles. Defaulting to is_agent=true for: ${newAgent.email}`)
        newAgent.is_agent = true
      }

      // ... rest of the function ...
    } catch (error) {
      logger.error(`Error creating new agent: ${error}`);
      set.status = 500;
      return {
        success: false,
        error: 'Failed to create new agent'
      };
    }
  })
  .post('/organizations/exit-demo-mode', async ({ request, set }) => {
    const db = new Database();
    
    try {
      // Get current user from session
      const currentUser = await getUserFromSession(request);
      
      if (!currentUser) {
        set.status = 401;
        return { success: false, message: 'Unauthorized' };
      }

      // Only allow admins to exit demo mode
      if (!currentUser.is_admin) {
        set.status = 403;
        return { success: false, message: 'Only administrators can exit demo mode' };
      }

      // Update the organization's demo_mode to false
      const result = await db.execute(
        `UPDATE organizations 
         SET demo_mode = 0
         WHERE id = ?`,
        [currentUser.organization_id]
      );

      /* TODO: Uncomment this when we have a way to enable regular and followup scheduling
      const result = await db.execute(
        `UPDATE organizations 
         SET demo_mode = 0, regular_scheduling_active = 1, followup_scheduling_active = 1
         WHERE id = ?`,
        [currentUser.organization_id]
      );
      */

      const rowsAffected = result.rowsAffected || 0;
      
      if (rowsAffected === 0) {
        logger.info(`No changes made when exiting demo mode for org ${currentUser.organization_id}`);
      } else {
        logger.info(`Successfully exited demo mode for organization ${currentUser.organization_id}`);
      }
      set.status = 200;
      return { success: true };
    } catch (error) {
      logger.error(`Error exiting demo mode: ${error}`);
      set.status = 500;
      return { success: false, message: 'Internal server error' };
    }
  });

export function createOrganizationRoutes() {
  const dbInstance = new Database();
  const authService = new AuthService();

  return new Elysia()
    .use(cookie())
    // Add a new endpoint for direct signup from UserDetails page
    .post('/api/organizations/signup', async ({ body, set, setCookie }) => {
      try {
        const { adminFirstName, adminLastName, adminEmail, phone, organizationName, planType } = body as {
          adminFirstName: string;
          adminLastName: string;
          adminEmail: string;
          phone?: string;
          organizationName: string;
          planType?: string;
        };

        // Decode email to ensure it's stored properly
        const decodedEmail = ensureEmailDecoded(adminEmail);

        // Validate input
        const validation = signupSchema.safeParse({
          ...body,
          adminEmail: decodedEmail // Use decoded email for validation
        });
        if (!validation.success) {
          set.status = 400;
          return {
            success: false,
            message: validation.error.errors[0].message
          };
        }

        // Check if email already exists
        const existingUser = await dbInstance.query<{ count: number }>(
          'SELECT COUNT(*) as count FROM users WHERE LOWER(email) = LOWER(?)',
          [decodedEmail]
        );

        if (existingUser[0]?.count > 0) {
          set.status = 400;
          return {
            success: false,
            message: 'This email address is already registered'
          };
        }

        // Generate a unique slug from the organization name
        const slug = await generateUniqueSlug(dbInstance, organizationName);

        // Create temporary session token
        const tempSessionId = generateToken();

        // Set session cookie for 24 hours
        setCookie('onboardingSession', tempSessionId, {
          httpOnly: true,
          maxAge: 60 * 60 * 24, // 24 hours
          path: '/'
        });

        // Set org slug cookie (not HTTP only so frontend can access it)
        setCookie('orgSlug', slug, {
          httpOnly: false,
          maxAge: 60 * 60 * 24 * 30, // 30 days
          path: '/'
        });

        // Create organization
        const orgResult = await dbInstance.execute(`
          INSERT INTO organizations (
            name, 
            subscription_tier, 
            created_at, 
            onboarding_completed, 
            slug, 
            onboarding_step,
            temp_session_id
          ) VALUES (?, ?, datetime('now'), FALSE, ?, ?, ?)`,
          [organizationName, planType || 'basic', slug, 2, tempSessionId]
        );

        const orgId = Number(orgResult.lastInsertRowid);
        logger.info(`Created organization: ${orgId} with slug: ${slug}`);

        // Create admin user with decoded email
        await dbInstance.execute(
          `INSERT INTO users (
            email, 
            first_name, 
            last_name, 
            phone, 
            is_admin, 
            is_agent, 
            organization_id, 
            created_at,
            is_active
          ) VALUES (?, ?, ?, ?, 1, 1, ?, datetime('now'), 0)`,
          [decodedEmail, adminFirstName, adminLastName, phone || '', orgId]
        );

        logger.info(`Created admin user for org ${slug} - Name: ${adminFirstName} ${adminLastName}, Email: ${decodedEmail}`);

        // Generate a magic link for verification
        const magicLink = `${config.clientUrl}/auth/verify/${slug}/${tempSessionId}`;

        // Send welcome email
        try {
          await sendMagicLink({
            email: decodedEmail,
            magicLink: magicLink,
            name: adminFirstName
          });
          logger.info(`Sent welcome email to ${decodedEmail}`);
        } catch (emailError) {
          logger.error(`Failed to send welcome email: ${emailError}`);
          // Continue even if email fails
        }

        return {
          success: true,
          message: 'Organization and admin user created successfully',
          slug: slug
        };

      } catch (error) {
        logger.error(`Error creating organization: ${error}`);
        set.status = 500;
        return {
          success: false,
          message: 'Failed to create organization'
        };
      }
    })
    
    // Add a new endpoint to check email availability
    .get('/api/organizations/check-email/:email', async ({ params, set }) => {
      try {
        const { email } = params;
        
        if (!email || !email.trim()) {
          set.status = 400;
          return {
            available: false,
            message: 'Email is required'
          };
        }
        
        // Ensure email is fully decoded
        const decodedEmail = ensureEmailDecoded(email);
        
        // Basic email format validation
        if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(decodedEmail)) {
          return {
            available: false,
            message: 'Invalid email format'
          };
        }
        
        // Check if email already exists
        const existingUser = await dbInstance.query<{ count: number }>(
          'SELECT COUNT(*) as count FROM users WHERE LOWER(email) = LOWER(?)',
          [decodedEmail]
        );
        
        const count = existingUser[0]?.count || 0;
        
        if (count > 0) {
          return {
            available: false,
            message: 'This email address is already registered'
          };
        }
        
        // If we get here, the email is available
        return {
          available: true,
          message: 'Email is available'
        };
        
      } catch (error) {
        logger.error(`Error checking email availability: ${error}`);
        set.status = 500;
        return {
          available: false,
          message: 'Error checking email availability'
        };
      }
    });
} 