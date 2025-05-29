import { Elysia } from 'elysia'
import { logger } from '../logger'
import { Database } from '../database'
import { config } from '../config'
import { cookie } from '@elysiajs/cookie'
import Stripe from 'stripe'

// Initialize Stripe with secret key from environment variables
const stripe = new Stripe(process.env.STRIPE_SECRET_KEY || 'sk_test_51Qyh7RCBUPXAZKNGFySALjap1pDAtEwPtuY5TAzEuKKDq7cfAmHhmQIn8W1UMf2CuOvQ1umjiUrlpPauOc159fpM00nfohCZH3')
const YOUR_DOMAIN = config.clientUrl || 'http://localhost:3000'

// Helper function for generating unique slugs from names
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

// Helper function to ensure emails are properly decoded before storing in the database
function ensureEmailDecoded(email: string): string {
  try {
    // Check if email appears to be encoded
    if (email.includes('%')) {
      let decoded = email;
      let previous = '';
      
      // Apply decoding until the string no longer changes
      // This handles scenarios where the email might be double-encoded
      while (decoded !== previous) {
        previous = decoded;
        decoded = decodeURIComponent(decoded);
      }
      
      // Log if we actually changed something
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

export function createOnboardingRoutes() {
  const dbInstance = new Database()
  
  // Log that we're creating the routes, for debugging
  logger.info('Initializing onboarding routes including resume endpoint');

  // Define the resume endpoint separately to ensure it's properly registered
  const resumeHandler = async ({ query, set, request }: { query: any, set: any, request: any }) => {
    // Log request headers for debugging authentication issues
    logger.info(`Request headers: ${JSON.stringify(request.headers)}`);
    logger.info(`Resume endpoint called with query: ${JSON.stringify(query)}`);
    
    try {
      const { email } = query as { email: string };
      
      if (!email) {
        set.status = 400;
        return {
          success: false,
          message: 'Email is required'
        };
      }
      
      // Properly decode the email parameter
      const decodedEmail = ensureEmailDecoded(email);
      logger.info(`Processing resume request for email: ${decodedEmail}`);
      
      // Find the user and their organization
      logger.info(`Searching for user with email: ${decodedEmail}`);
      let userInfo = await dbInstance.fetchOne<{ 
        id: number, 
        organization_id: number, 
        first_name: string,
        last_name: string,
        email: string,
        phone: string
      }>(
        'SELECT id, organization_id, first_name, last_name, email, phone FROM users WHERE email = ?',
        [decodedEmail]
      );
      
      if (!userInfo) {
        logger.warn(`User not found with email: ${decodedEmail}`);
        // Try a case-insensitive search as a fallback
        const userByLowercase = await dbInstance.fetchOne<{ 
          id: number, 
          organization_id: number, 
          first_name: string,
          last_name: string,
          email: string,
          phone: string
        }>(
          'SELECT id, organization_id, first_name, last_name, email, phone FROM users WHERE LOWER(email) = LOWER(?)',
          [decodedEmail]
        );
        
        if (userByLowercase) {
          logger.info(`Found user with case-insensitive email match: ${userByLowercase.email}`);
          // Continue with the found user
          userInfo = userByLowercase;
        } else {
          logger.error(`User not found with email (case insensitive): ${decodedEmail}`);
          set.status = 401;
          return {
            success: false,
            message: 'User not found'
          };
        }
      }
      
      const organizationId = userInfo.organization_id;
      logger.info(`Found user with organization ID: ${organizationId}`);
      
      // Get organization details
      let orgInfo;
      try {
        orgInfo = await dbInstance.fetchOne<{
          id: number,
          name: string,
          onboarding_completed: number,
          website: string,
          phone: string,
          primary_color: string,
          secondary_color: string,
          logo_data: string | null,
          org_settings: string | null,
          payment_completed: number,
          stripe_customer_id: string | null,
          stripe_subscription_id: string | null
        }>(
          `SELECT id, name, onboarding_completed, website, phone, 
           primary_color, secondary_color, logo_data, org_settings,
           payment_completed, stripe_customer_id, stripe_subscription_id
           FROM organizations WHERE id = ?`,
          [organizationId]
        );
        
        logger.info(`Organization query result: ${orgInfo ? 'Found' : 'Not found'}`);
        
        if (!orgInfo) {
          logger.warn(`Organization with ID ${organizationId} not found`);
          set.status = 404;
          return {
            success: false,
            message: 'Organization not found'
          };
        }
      } catch (error) {
        logger.error(`Error retrieving organization with ID ${organizationId}: ${error}`);
        set.status = 500;
        return {
          success: false,
          message: 'Error retrieving organization'
        };
      }
      
      // Check if onboarding is already completed
      if (orgInfo.onboarding_completed === 1) {
        set.status = 200;
        return {
          success: true,
          onboardingComplete: true,
          redirectToLogin: true
        };
      }
      
      // Get all agents/users for this organization
      const agents = await dbInstance.query<{
        id: number,
        first_name: string,
        last_name: string,
        email: string,
        phone: string,
        is_admin: number
      }>(
        `SELECT id, first_name, last_name, email, phone, is_admin 
         FROM users WHERE organization_id = ?`,
        [organizationId]
      );
      
      // Parse org settings if exists
      let orgSettings = {};
      if (orgInfo.org_settings) {
        try {
          orgSettings = JSON.parse(orgInfo.org_settings);
        } catch (e) {
          logger.warn(`Could not parse org_settings for org ${organizationId}: ${e}`);
        }
      }
      
      // Check if payment fields exist in orgInfo to prevent errors
      logger.info(`Payment data: completed=${orgInfo.payment_completed !== undefined}, customerID=${orgInfo.stripe_customer_id !== undefined}, subscriptionID=${orgInfo.stripe_subscription_id !== undefined}`);
      
      // Construct response data
      set.status = 200;
      return {
        success: true,
        onboardingComplete: false,
        organization: {
          id: orgInfo.id,
          name: orgInfo.name,
          website: orgInfo.website || '',
          phone: orgInfo.phone || '',
          primaryColor: orgInfo.primary_color || '#6B46C1',
          secondaryColor: orgInfo.secondary_color || '#9F7AEA',
          logo: orgInfo.logo_data || null
        },
        user: {
          id: userInfo.id,
          firstName: userInfo.first_name,
          lastName: userInfo.last_name,
          email: userInfo.email,
          phone: userInfo.phone || ''
        },
        agents: agents.map(agent => ({
          firstName: agent.first_name,
          lastName: agent.last_name,
          email: agent.email,
          phone: agent.phone || '',
          isAdmin: agent.is_admin === 1
        })),
        carrierSettings: {
          selectedCarriers: (orgSettings as any)?.carrierContracts || [],
          useSmartSend: (orgSettings as any)?.smartSendEnabled || true
        },
        paymentStatus: {
          paymentCompleted: orgInfo.payment_completed === 1,
          stripeCustomerId: orgInfo.stripe_customer_id || null,
          stripeSubscriptionId: orgInfo.stripe_subscription_id || null
        }
      };
      
    } catch (error) {
      logger.error(`Error resuming onboarding: ${error}`);
      set.status = 500;
      return {
        success: false,
        message: 'Failed to resume onboarding'
      };
    }
  };

  const app = new Elysia()
    // Register the resume endpoint with the correct path - bypass auth
    .get('/api/onboarding/resume', (context) => {
      // Explicitly set the status to 200
      context.set.status = 200;
      return resumeHandler(context);
    })
    
    // Create a Stripe checkout session
    .post('/api/create-checkout-session', async ({ body, set }) => {
      try {
        const { priceId, meteredPriceId, customerEmail, customerName } = body as { 
          priceId: string, 
          meteredPriceId?: string,
          customerEmail?: string,
          customerName?: string
        }
        
        if (!priceId) {
          set.status = 400
          return {
            success: false,
            message: 'Price ID is required'
          }
        }
        
        // Decode the email address if it exists and contains encoded characters
        let decodedEmail: string | undefined;
        if (customerEmail) {
          try {
            // Check if the email contains encoded characters
            if (customerEmail.includes('%')) {
              decodedEmail = decodeURIComponent(customerEmail);
            } else {
              decodedEmail = customerEmail;
            }
            logger.info(`Decoded email from ${customerEmail} to ${decodedEmail}`);
          } catch (error) {
            logger.warn(`Failed to decode email: ${customerEmail}. Using as-is.`);
            decodedEmail = customerEmail;
          }
        }
        
        logger.info(`Creating checkout session for base price: ${priceId}, metered price: ${meteredPriceId || 'none'}, customer: ${decodedEmail || 'anonymous'}`);
        
        // Build line items array for the checkout session
        const lineItems: any[] = [
          {
            price: priceId,
            //quantity: 1, // Required for regular subscription
          }
        ];
        
        // Add metered price if provided, WITHOUT a quantity (Stripe doesn't want quantity for metered prices)
        if (meteredPriceId) {
          lineItems.push({
            price: meteredPriceId,
            // No quantity for metered prices
          });
        }
        
        logger.info(`Creating checkout session with line items: ${JSON.stringify(lineItems)}`);
        
        // Create the session
        const session = await stripe.checkout.sessions.create({
          ui_mode: 'embedded',
          line_items: lineItems,
          mode: 'subscription',
          redirect_on_completion: 'never',
          automatic_tax: { enabled: true },
          ...(decodedEmail ? { customer_email: decodedEmail } : {})
        })
        
        set.status = 200;
        return {
          success: true,
          clientSecret: session.client_secret
        }
      } catch (error) {
        logger.error(`Error creating checkout session: ${error}`)
        set.status = 500
        return {
          success: false,
          message: 'Failed to create checkout session'
        }
      }
    })
    
    // Get session status
    .get('/api/session-status', async ({ query, set }) => {
      try {
        const { session_id } = query as { session_id: string }
        
        if (!session_id) {
          logger.error('No session ID provided in status request');
          set.status = 400
          return {
            success: false,
            message: 'Session ID is required'
          }
        }
        
        logger.info(`Checking status for session ID: ${session_id}`);
        const session = await stripe.checkout.sessions.retrieve(session_id)
        
        logger.info(`Retrieved session with status: ${session.status}, payment status: ${session.payment_status || 'unknown'}`);
        
        // If payment is complete, store the customer and subscription info in the database
        if (session.status === 'complete' && session.customer_details?.email) {
          const customerEmail = session.customer_details.email;
          // Ensure the email is decoded
          const decodedEmail = ensureEmailDecoded(customerEmail);
          const stripeCustomerId = session.customer as string;
          const stripeSubscriptionId = session.subscription as string;
          
          logger.info(`Payment completed for ${decodedEmail}, updating database records`);
          logger.info(`Customer ID: ${stripeCustomerId}, Subscription ID: ${stripeSubscriptionId}`);
          
          // Find the user by email
          const userInfo = await dbInstance.fetchOne<{ organization_id: number }>(
            'SELECT organization_id FROM users WHERE email = ?',
            [decodedEmail]
          );
          
          if (userInfo) {
            // Update the organization with payment info
            await dbInstance.execute(
              `UPDATE organizations SET 
                payment_completed = 1,
                stripe_customer_id = ?,
                stripe_subscription_id = ?
               WHERE id = ?`,
              [stripeCustomerId, stripeSubscriptionId, userInfo.organization_id]
            );
            
            logger.info(`Updated payment status for organization ID: ${userInfo.organization_id}`);
          } else {
            logger.warn(`Could not find user with email ${decodedEmail} to update payment status`);
          }
        } else if (session.status === 'open') {
          logger.info(`Session ${session_id} is still open, payment not completed yet`);
        } else {
          logger.info(`Session ${session_id} has status ${session.status}, no database update needed`);
        }
        
        return {
          success: true,
          status: session.status,
          payment_status: session.payment_status || null,
          customer_email: session.customer_details?.email || null,
          amount_total: session.amount_total,
          customer: session.customer,
          subscription: session.subscription
        }
      } catch (error) {
        logger.error(`Error checking session status: ${error}`)
        set.status = 500
        return {
          success: false,
          message: 'Failed to check session status'
        }
      }
    })
    
    // Get checkout session from client secret
    .get('/api/checkout-session', async ({ query, set }) => {
      try {
        const { clientSecret } = query as { clientSecret: string }
        
        if (!clientSecret) {
          logger.error('No client secret provided in checkout session request');
          set.status = 400
          return {
            success: false,
            message: 'Client secret is required'
          }
        }
        
        // Extract the session ID from the client secret (format: cs_<id>_secret_<secret>)
        const sessionId = clientSecret.split('_secret_')[0];
        if (!sessionId) {
          logger.error(`Could not extract session ID from client secret: ${clientSecret.substring(0, 10)}...`);
          set.status = 400;
          return {
            success: false,
            message: 'Invalid client secret format'
          };
        }
        
        logger.info(`Retrieving checkout session with ID: ${sessionId}`);
        
        // Retrieve the session from Stripe
        logger.info(`Calling Stripe API to retrieve session ${sessionId} with expanded subscription data`);
        const session = await stripe.checkout.sessions.retrieve(sessionId, {
          expand: ['subscription', 'subscription.items']
        });
        
        if (!session) {
          logger.error(`Could not find session with ID: ${sessionId}`);
          set.status = 404;
          return {
            success: false,
            message: 'Session not found'
          };
        }
        
        logger.info(`Session retrieved successfully. Status: ${session.status}, Customer: ${session.customer ? (session.customer as string).substring(0, 10) + '...' : 'none'}`);
        logger.info(`Session subscription data: ${session.subscription ? 'present' : 'missing'}`);
        
        // Get subscription item ID if it exists
        let subscriptionItemId = null;
        if (session.subscription && typeof session.subscription !== 'string') {
          const subscription = session.subscription;
          const items = subscription.items?.data || [];
          
          logger.info(`Found ${items.length} subscription items`);
          
          // Log item details for debugging
          items.forEach((item, index) => {
            logger.info(`Subscription item ${index + 1}:`);
            logger.info(`  ID: ${item.id}`);
            logger.info(`  Price: ${item.price ? item.price.id : 'unknown'}`);
            if (item.price && typeof item.price === 'object') {
              logger.info(`  Price Type: ${(item.price as any).type || 'unknown'}`);
              logger.info(`  Price Nickname: ${(item.price as any).nickname || 'unknown'}`);
            }
          });
          
          // Use a type-safe approach to identify the metered price item
          const meteredItem = items.find(item => {
            if (item.price && typeof item.price === 'object' && 'type' in item.price) {
              const priceType = (item.price as any).type;
              logger.info(`Checking item with price type: ${priceType}`);
              return priceType === 'metered';
            }
            return false;
          });
          
          if (meteredItem) {
            subscriptionItemId = meteredItem.id;
            logger.info(`Found metered subscription item: ${subscriptionItemId}`);
          } else {
            logger.info('No metered subscription item found');
            // Fallback: just use the last item if there are multiple items (often the metered item is second)
            if (items.length > 1) {
              subscriptionItemId = items[items.length - 1].id;
              logger.info(`Using last subscription item as fallback: ${subscriptionItemId}`);
            }
          }
        } else {
          logger.info(`Subscription data is not expanded or missing: ${typeof session.subscription}`);
        }
        
        const result = {
          success: true,
          customer: session.customer as string,
          subscription: session.subscription as string || null,
          subscription_item: subscriptionItemId
        };
        
        logger.info(`Returning checkout session data: ${JSON.stringify(result)}`);
        return result;
      } catch (error) {
        logger.error(`Error retrieving checkout session: ${error}`);
        set.status = 500;
        return {
          success: false,
          message: `Failed to retrieve checkout session: ${error instanceof Error ? error.message : 'Unknown error'}`
        }
      }
    })
    
    // Save company details during onboarding
    .post('/api/onboarding/company', async ({ body, set }) => {
      try {
        const { 
          email,
          companyName, 
          companyPhone, 
          companyWebsite, 
          primaryColor, 
          secondaryColor, 
          logo,
          firstName,
          lastName,
          phone
        } = body as { 
          email: string,
          companyName?: string,
          companyPhone?: string,
          companyWebsite?: string,
          primaryColor?: string,
          secondaryColor?: string,
          logo?: string,
          firstName?: string,
          lastName?: string,
          phone?: string
        };
        
        if (!email) {
          set.status = 400;
          return {
            success: false,
            message: 'Email is required'
          };
        }
        
        // Ensure email is decoded
        const decodedEmail = ensureEmailDecoded(email);
        logger.info(`Processing onboarding company update for email: ${decodedEmail}`);
        
        // Find the organization by user email
        let userInfo = await dbInstance.fetchOne<{ id: number, organization_id: number }>(
          'SELECT id, organization_id FROM users WHERE email = ?',
          [decodedEmail]
        );
        
        let organizationId;
        
        if (!userInfo) {
          // Create new organization first
          const slug = await generateUniqueSlug(dbInstance, companyName || 'New Organization');
          
          // Insert the organization
          const orgResult = await dbInstance.query<{ id: number }>(
            `INSERT INTO organizations (
              name, 
              slug,
              phone,
              website,
              primary_color,
              secondary_color,
              logo_data,
              onboarding_completed
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            RETURNING id`,
            [
              companyName || 'New Organization',
              slug,
              companyPhone || '',
              companyWebsite || '',
              primaryColor || '#6B46C1',
              secondaryColor || '#9F7AEA',
              logo || null,
              0 // Not completed yet
            ]
          );
          
          if (!orgResult || orgResult.length === 0) {
            throw new Error('Failed to create organization');
          }
          
          organizationId = orgResult[0].id;
          logger.info(`Created new organization with ID: ${organizationId}`);
          
          // Create the user with all available fields
          const userResult = await dbInstance.query<{ id: number }>(
            `INSERT INTO users (
              email,
              first_name,
              last_name,
              phone,
              organization_id,
              is_admin,
              is_agent
            ) VALUES (?, ?, ?, ?, ?, ?, ?)
            RETURNING id`,
            [
              decodedEmail,
              firstName || '',
              lastName || '',
              phone || '',
              organizationId,
              1, // Set as admin
              1  // Set as agent
            ]
          );
          
          if (!userResult || userResult.length === 0) {
            throw new Error('Failed to create user');
          }
          
          userInfo = {
            id: userResult[0].id,
            organization_id: organizationId
          };
          
          logger.info(`Created new user (${userInfo.id}) for organization (${organizationId})`);
          
          // Set success status
          set.status = 200;
          return {
            success: true,
            message: 'Organization and user created successfully',
            organizationId,
            userId: userInfo.id
          };
        } else {
          organizationId = userInfo.organization_id;
          
          // Update existing organization
          await dbInstance.execute(
            `UPDATE organizations SET 
              name = COALESCE(?, name),
              phone = COALESCE(?, phone),
              website = COALESCE(?, website),
              primary_color = COALESCE(?, primary_color),
              secondary_color = COALESCE(?, secondary_color),
              logo_data = COALESCE(?, logo_data)
            WHERE id = ?`,
            [
              companyName,
              companyPhone,
              companyWebsite,
              primaryColor,
              secondaryColor,
              logo,
              organizationId
            ]
          );
          
          // Update existing user if we have new information
          if (firstName || lastName || phone) {
            await dbInstance.execute(
              `UPDATE users SET 
                first_name = COALESCE(?, first_name),
                last_name = COALESCE(?, last_name),
                phone = COALESCE(?, phone)
              WHERE id = ?`,
              [firstName, lastName, phone, userInfo.id]
            );
          }
          
          logger.info(`Updated existing organization: ${organizationId} and user: ${userInfo.id}`);
          
          // Set success status
          set.status = 200;
          return {
            success: true,
            message: 'Organization and user updated successfully',
            organizationId,
            userId: userInfo.id
          };
        }
        
      } catch (error) {
        logger.error(`Error in company details endpoint: ${error}`);
        set.status = 500;
        return {
          success: false,
          message: 'Failed to process company details'
        };
      }
    })
    
    // Save licensing settings during onboarding
    .post('/api/onboarding/licensing', async ({ body, set }) => {
      try {
        const { 
          email,
          selectedCarriers,
          useSmartSend
        } = body as { 
          email: string,
          selectedCarriers: string[],
          useSmartSend?: boolean
        };
        
        if (!email) {
          set.status = 400;
          return {
            success: false,
            message: 'Email is required'
          };
        }
        
        // Ensure email is decoded
        const decodedEmail = ensureEmailDecoded(email);
        logger.info(`Processing onboarding licensing for email: ${decodedEmail}`);
        
        // Find the organization by user email
        const userInfo = await dbInstance.fetchOne<{ organization_id: number }>(
          'SELECT organization_id FROM users WHERE email = ?',
          [decodedEmail]
        );
        
        if (!userInfo) {
          logger.warn(`User not found for email: ${decodedEmail}`);
          set.status = 404;
          return {
            success: false,
            message: 'User not found - please start from signup'
          };
        }
        
        const organizationId = userInfo.organization_id;
        
        // Verify organization exists
        const orgExists = await dbInstance.fetchOne<{ id: number }>(
          'SELECT id FROM organizations WHERE id = ?',
          [organizationId]
        );
        
        if (!orgExists) {
          logger.warn(`Organization ${organizationId} not found for user ${decodedEmail}`);
          set.status = 404;
          return {
            success: false,
            message: 'Organization not found - please start from signup'
          };
        }
        
        // Get current org settings if any
        const currentSettings = await dbInstance.fetchOne<{ org_settings: string | null }>(
          'SELECT org_settings FROM organizations WHERE id = ?',
          [organizationId]
        );
        
        // Parse existing settings or create empty object
        let settings = {};
        if (currentSettings?.org_settings) {
          try {
            settings = JSON.parse(currentSettings.org_settings);
          } catch (e) {
            logger.warn(`Could not parse existing org_settings for org ${organizationId}: ${e}`);
          }
        }
        
        // Update with new carrier contracts
        settings = {
          ...settings,
          carrierContracts: selectedCarriers || [],
          smartSendEnabled: useSmartSend !== undefined ? useSmartSend : ((settings as any)['smartSendEnabled'] || false)
        };
        
        // Convert back to JSON string
        const updatedSettingsJson = JSON.stringify(settings);
        
        // Update the organization
        await dbInstance.execute(
          'UPDATE organizations SET org_settings = ? WHERE id = ?',
          [updatedSettingsJson, organizationId]
        );
        
        logger.info(`Updated licensing settings for organization ID: ${organizationId}`);
        
        set.status = 200
        return {
          success: true,
          message: 'Licensing settings updated successfully'
        };
        
      } catch (error) {
        logger.error(`Error updating licensing settings: ${error}`);
        set.status = 500;
        return {
          success: false,
          message: 'Failed to update licensing settings'
        };
      }
    })
    
    // Save agents during onboarding
    .post('/api/onboarding/agents', async ({ body, set }) => {
      try {
        const { 
          email,
          agents
        } = body as { 
          email: string,
          agents: Array<{
            firstName: string;
            lastName: string;
            email: string;
            phone: string;
            isAdmin: boolean;
          }>;
        };
        
        if (!email) {
          set.status = 400;
          return {
            success: false,
            message: 'Email is required'
          };
        }
        
        // Ensure owner email is decoded
        const decodedOwnerEmail = ensureEmailDecoded(email);
        logger.info(`Processing onboarding agents for owner email: ${decodedOwnerEmail}`);
        
        // Find the organization by user email
        const userInfo = await dbInstance.fetchOne<{ organization_id: number }>(
          'SELECT organization_id FROM users WHERE email = ?',
          [decodedOwnerEmail]
        );
        
        if (!userInfo) {
          logger.warn(`User not found for email: ${decodedOwnerEmail}`);
          set.status = 404;
          return {
            success: false,
            message: 'User not found - please start from signup'
          };
        }
        
        const organizationId = userInfo.organization_id;
        
        // Verify organization exists
        const orgExists = await dbInstance.fetchOne<{ id: number }>(
          'SELECT id FROM organizations WHERE id = ?',
          [organizationId]
        );
        
        if (!orgExists) {
          logger.warn(`Organization ${organizationId} not found for user ${decodedOwnerEmail}`);
          set.status = 404;
          return {
            success: false,
            message: 'Organization not found - please start from signup'
          };
        }
        
        // Process each agent
        let createdCount = 0;
        let skippedCount = 0;
        
        for (const agent of agents) {
          // Ensure agent email is decoded
          const decodedAgentEmail = ensureEmailDecoded(agent.email);
          
          // Skip if this is the owner (already in the database)
          if (decodedAgentEmail.toLowerCase() === decodedOwnerEmail.toLowerCase()) {
            skippedCount++;
            continue;
          }
          
          // Check if this email already exists
          const existingAgent = await dbInstance.fetchOne<{ count: number }>(
            'SELECT COUNT(*) as count FROM users WHERE LOWER(email) = LOWER(?)',
            [decodedAgentEmail]
          );
          
          if (existingAgent && existingAgent.count > 0) {
            skippedCount++;
            continue;
          }
          
          // Create the new user with decoded email
          await dbInstance.execute(
            `INSERT INTO users (
              first_name, 
              last_name, 
              email, 
              phone, 
              is_admin, 
              is_agent, 
              organization_id
            ) VALUES (?, ?, ?, ?, ?, ?, ?)`,
            [
              agent.firstName,
              agent.lastName,
              decodedAgentEmail,
              agent.phone || '',
              agent.isAdmin ? 1 : 0,
              1, // All users in this context are agents
              organizationId
            ]
          );
          
          createdCount++;
        }
        
        // Mark onboarding as complete since this is the final step
        await dbInstance.execute(
          'UPDATE organizations SET onboarding_completed = 1 WHERE id = ?',
          [organizationId]
        );
        
        logger.info(`Created ${createdCount} agents, skipped ${skippedCount}, and marked onboarding as complete for organization ID: ${organizationId}`);
        
        return {
          success: true,
          message: `Successfully added ${createdCount} agents and completed onboarding`,
          created: createdCount,
          skipped: skippedCount
        };
        
      } catch (error) {
        logger.error(`Error adding agents: ${error}`);
        set.status = 500;
        return {
          success: false,
          message: 'Failed to add agents'
        };
      }
    })
    .get('/api/subscription/pricing', async ({ set }) => {
      try {
        const publishableKey = process.env.STRIPE_PUBLISHABLE_KEY;
        const priceId = process.env.STRIPE_PRICE_ID;

        if (!publishableKey || !priceId) {
          set.status = 500;
          return {
            success: false,
            message: 'Stripe configuration missing'
          };
        }

        set.status = 200;
        return {
          success: true,
          publishableKey,
          priceId
        };
      } catch (error) {
        logger.error(`Error getting Stripe pricing: ${error}`);
        set.status = 500;
        return {
          success: false,
          message: 'Failed to get Stripe pricing'
        };
      }
    })
    // Simplified checkout endpoint that creates the account
    .post('/api/subscription/checkout', async ({ body, set }) => {
      try {
        logger.info(`Processing account creation request: ${JSON.stringify(body)}`);
        const { firstName, lastName, email, tierId = 'basic', 
                companyName, companyPhone, companyWebsite, 
                primaryColor, secondaryColor, logo } = body as { 
          firstName: string, 
          lastName: string, 
          email: string,
          tierId?: string,
          companyName?: string,
          companyPhone?: string,
          companyWebsite?: string,
          primaryColor?: string,
          secondaryColor?: string,
          logo?: string
        };
        
        logger.info(`User info: ${firstName} ${lastName} (${email})`);
        
        if (!firstName || !lastName || !email) {
          logger.warn(`Missing required user information. Received: firstName=${!!firstName}, lastName=${!!lastName}, email=${!!email}`);
          set.status = 400;
          return { 
            success: false,
            message: 'Missing required user information'
          };
        }
        
        // Check if email already exists
        const existingUser = await dbInstance.query<{ id: number, organization_id: number, count: number }>(
          'SELECT id, organization_id, COUNT(*) as count FROM users WHERE LOWER(email) = LOWER(?)',
          [email]
        );
        
        let organizationId;
        
        if (existingUser[0]?.count > 0) {
          // User exists, check if their organization has completed onboarding
          const orgInfo = await dbInstance.query<{ onboarding_completed: number }>(
            'SELECT onboarding_completed FROM organizations WHERE id = ?',
            [existingUser[0].organization_id]
          );
          
          if (orgInfo[0]?.onboarding_completed === 1) {
            logger.warn(`Email ${email} is already registered with completed onboarding`);
            set.status = 400;
            return { 
              success: false, 
              message: 'This email address is already registered with completed onboarding'
            };
          }
          
          // Use the existing organization if onboarding is not complete
          logger.info(`Found existing organization ${existingUser[0].organization_id} with incomplete onboarding`);
          organizationId = existingUser[0].organization_id;
          
          // Update organization details if provided
          if (companyName || companyPhone || companyWebsite || primaryColor || secondaryColor || logo) {
            const updates = [];
            const params = [];
            
            if (companyName) {
              updates.push('name = ?');
              params.push(companyName);
            }
            
            if (companyPhone) {
              updates.push('phone = ?');
              params.push(companyPhone);
            }
            
            if (companyWebsite) {
              updates.push('website = ?');
              params.push(companyWebsite);
            }
            
            if (primaryColor) {
              updates.push('primary_color = ?');
              params.push(primaryColor);
            }
            
            if (secondaryColor) {
              updates.push('secondary_color = ?');
              params.push(secondaryColor);
            }
            
            if (logo) {
              updates.push('logo_data = ?');
              params.push(logo);
            }
            
            if (updates.length > 0) {
              params.push(organizationId);
              await dbInstance.execute(
                `UPDATE organizations SET ${updates.join(', ')} WHERE id = ?`,
                params
              );
              logger.info(`Updated organization details for ID: ${organizationId}`);
            }
          }
        } else {
          // Create new organization for new user
          // Generate a unique slug from the name
          const organizationName = companyName || `${firstName}'s Organization`;
          const slug = await generateUniqueSlug(dbInstance, organizationName);
          
          logger.info(`Generated organization name: "${organizationName}" and slug: "${slug}"`);
          
          // Create new organization with optional fields if provided
          const orgFields = ['name', 'slug'];
          const orgValues = [organizationName, slug];
          const orgPlaceholders = ['?', '?'];
          
          if (companyPhone) {
            orgFields.push('phone');
            orgValues.push(companyPhone);
            orgPlaceholders.push('?');
          }
          
          if (companyWebsite) {
            orgFields.push('website');
            orgValues.push(companyWebsite);
            orgPlaceholders.push('?');
          }
          
          if (primaryColor) {
            orgFields.push('primary_color');
            orgValues.push(primaryColor);
            orgPlaceholders.push('?');
          }
          
          if (secondaryColor) {
            orgFields.push('secondary_color');
            orgValues.push(secondaryColor);
            orgPlaceholders.push('?');
          }
          
          if (logo) {
            orgFields.push('logo_data');
            orgValues.push(logo);
            orgPlaceholders.push('?');
          }
          
          const result = await dbInstance.query<{ id: number }>(
            `INSERT INTO organizations (${orgFields.join(', ')}) VALUES (${orgPlaceholders.join(', ')})`,
            orgValues
          );
          
          if (result.length > 0) {
            organizationId = result[0].id;
          } else {
            throw new Error('Failed to create organization');
          }
        }
        
        // Create new user
        const resultUser = await dbInstance.query<{ id: number }>(
          'INSERT INTO users (firstName, lastName, email, organization_id) VALUES (?, ?, ?, ?)',
          [firstName, lastName, email, organizationId]
        );
        
        if (resultUser.length > 0) {
          logger.info(`User ${firstName} ${lastName} (${email}) successfully created`);
          set.status = 200;
          return {
            success: true,
            message: 'Account created successfully'
          };
        } else {
          throw new Error('Failed to create user');
        }
      } catch (error) {
        logger.error(`Error processing account creation: ${error}`)
        set.status = 500
        return {
          success: false,
          message: 'Failed to process account creation'
        }
      }
    })

    // Debug endpoint to list users (development only)
    .get('/api/debug/users', async ({ set }) => {
      // Only allow in development environment
      if (process.env.NODE_ENV === 'production') {
        set.status = 404;
        return { error: 'Not found' };
      }
      
      try {
        logger.info('Debug endpoint called to list users');
        
        // Get all users with email addresses
        const users = await dbInstance.query<{ id: number, email: string }>(
          'SELECT id, email FROM users LIMIT 50'
        );
        
        logger.info(`Found ${users.length} users in the database`);
        
        // Map the results to include both the raw and decoded email
        const mappedUsers = users.map(user => {
          let decodedEmail;
          try {
            decodedEmail = user.email.includes('%') 
              ? decodeURIComponent(user.email) 
              : user.email;
          } catch (e: unknown) {
            decodedEmail = `[Decode error: ${e instanceof Error ? e.message : String(e)}]`;
          }
          
          return {
            id: user.id,
            email: user.email,
            decodedEmail: decodedEmail
          };
        });
        
        return mappedUsers;
      } catch (error) {
        logger.error(`Error in debug endpoint: ${error}`);
        set.status = 500;
        return { error: String(error) };
      }
    })

    // Debug endpoint to list and fix encoded emails (development only)
    .get('/api/debug/fix-emails', async ({ set }) => {
      // Only allow in development environment
      if (process.env.NODE_ENV === 'production') {
        set.status = 404;
        return { error: 'Not found' };
      }
      
      try {
        logger.info('Debug endpoint called to list and fix encoded emails');
        
        // Get all users with potentially encoded emails
        const users = await dbInstance.query<{ id: number, email: string }>(
          "SELECT id, email FROM users WHERE email LIKE '%\%%'"
        );
        
        logger.info(`Found ${users.length} users with potentially encoded emails`);
        
        // Process each user email
        const results = [];
        for (const user of users) {
          try {
            const originalEmail = user.email;
            const decodedEmail = ensureEmailDecoded(originalEmail);
            
            // Only update if there's a change
            if (decodedEmail !== originalEmail) {
              // Update the email in the database
              await dbInstance.execute(
                'UPDATE users SET email = ? WHERE id = ?',
                [decodedEmail, user.id]
              );
              
              results.push({
                id: user.id,
                originalEmail,
                fixedEmail: decodedEmail,
                status: 'fixed'
              });
              
              logger.info(`Fixed encoded email: ${originalEmail} -> ${decodedEmail} for user ID ${user.id}`);
            } else {
              results.push({
                id: user.id,
                email: originalEmail,
                status: 'no change needed'
              });
            }
          } catch (error) {
            results.push({
              id: user.id,
              email: user.email,
              status: 'error',
              message: String(error)
            });
            logger.error(`Error fixing email for user ${user.id}: ${error}`);
          }
        }
        
        return {
          success: true,
          processed: users.length,
          results
        };
      } catch (error) {
        logger.error(`Error in debug endpoint: ${error}`);
        set.status = 500;
        return { 
          success: false, 
          error: String(error)
        };
      }
    })

    // Debug endpoint to check a specific email (development only)
    .get('/api/debug/check-email', async ({ query, set }) => {
      // Only allow in development environment
      if (process.env.NODE_ENV === 'production') {
        set.status = 404;
        return { error: 'Not found' };
      }
      
      try {
        const { email } = query as { email: string };
        if (!email) {
          set.status = 400;
          return { error: 'Email parameter is required' };
        }
        
        const decodedEmail = ensureEmailDecoded(email);
        logger.info(`Debug endpoint checking email: ${email} -> ${decodedEmail}`);
        
        // Try various methods to find the user
        const exactMatch = await dbInstance.query<{ id: number, email: string }>(
          'SELECT id, email FROM users WHERE email = ?',
          [decodedEmail]
        );
        
        const lowercaseMatch = await dbInstance.query<{ id: number, email: string }>(
          'SELECT id, email FROM users WHERE LOWER(email) = LOWER(?)',
          [decodedEmail]
        );
        
        const likeMatch = await dbInstance.query<{ id: number, email: string }>(
          'SELECT id, email FROM users WHERE email LIKE ?',
          [`%${decodedEmail.split('@')[0]}%`]
        );
        
        // List all users for reference (limited to first 10)
        const allUsers = await dbInstance.query<{ id: number, email: string }>(
          'SELECT id, email FROM users LIMIT 10'
        );
        
        return {
          searchEmail: email,
          decodedEmail: decodedEmail,
          exactMatch: exactMatch.length > 0 ? exactMatch : null,
          lowercaseMatch: lowercaseMatch.length > 0 ? lowercaseMatch : null,
          likeMatch: likeMatch.length > 0 ? likeMatch : null,
          allUsers: allUsers
        };
      } catch (error) {
        logger.error(`Error in email debug endpoint: ${error}`);
        set.status = 500;
        return { error: String(error) };
      }
    })

    // Debug endpoint to test the resume data for a specific organization ID
    .get('/api/debug/test-resume', async ({ query, set }) => {
      // Only allow in development environment
      if (process.env.NODE_ENV === 'production') {
        set.status = 404;
        return { error: 'Not found' };
      }
      
      try {
        const { orgId } = query as { orgId: string };
        if (!orgId) {
          set.status = 400;
          return { error: 'orgId parameter is required' };
        }
        
        const organizationId = parseInt(orgId, 10);
        logger.info(`Debug endpoint testing resume data for organization ID: ${organizationId}`);
        
        // Get organization details directly by ID
        const orgInfo = await dbInstance.fetchOne<{
          id: number,
          name: string,
          onboarding_completed: number,
          website: string,
          phone: string,
          primary_color: string,
          secondary_color: string,
          logo_data: string | null,
          org_settings: string | null,
          payment_completed: number,
          stripe_customer_id: string | null,
          stripe_subscription_id: string | null
        }>(
          `SELECT id, name, onboarding_completed, website, phone, 
           primary_color, secondary_color, logo_data, org_settings,
           payment_completed, stripe_customer_id, stripe_subscription_id
           FROM organizations WHERE id = ?`,
          [organizationId]
        );
        
        if (!orgInfo) {
          set.status = 404;
          return { error: `Organization ID ${organizationId} not found` };
        }
        
        // Get a user from this organization (first one found)
        const userInfo = await dbInstance.fetchOne<{ 
          id: number, 
          first_name: string,
          last_name: string,
          email: string,
          phone: string
        }>(
          'SELECT id, first_name, last_name, email, phone FROM users WHERE organization_id = ? LIMIT 1',
          [organizationId]
        );
        
        if (!userInfo) {
          return {
            organization: orgInfo,
            error: 'No users found for this organization'
          };
        }
        
        // Get all agents/users for this organization
        const agents = await dbInstance.query<{
          id: number,
          first_name: string,
          last_name: string,
          email: string,
          phone: string,
          is_admin: number
        }>(
          `SELECT id, first_name, last_name, email, phone, is_admin 
           FROM users WHERE organization_id = ?`,
          [organizationId]
        );
        
        // Parse org settings if exists
        let orgSettings = {};
        if (orgInfo.org_settings) {
          try {
            orgSettings = JSON.parse(orgInfo.org_settings);
          } catch (e) {
            logger.warn(`Could not parse org_settings for org ${organizationId}: ${e}`);
          }
        }
        
        // Format the response in the same way as the resume endpoint
        return {
          success: true,
          onboardingComplete: orgInfo.onboarding_completed === 1,
          organization: {
            id: orgInfo.id,
            name: orgInfo.name,
            website: orgInfo.website || '',
            phone: orgInfo.phone || '',
            primaryColor: orgInfo.primary_color || '#6B46C1',
            secondaryColor: orgInfo.secondary_color || '#9F7AEA',
            logo: orgInfo.logo_data || null
          },
          user: {
            id: userInfo.id,
            firstName: userInfo.first_name,
            lastName: userInfo.last_name,
            email: userInfo.email,
            phone: userInfo.phone || ''
          },
          agents: agents.map(agent => ({
            firstName: agent.first_name,
            lastName: agent.last_name,
            email: agent.email,
            phone: agent.phone || '',
            isAdmin: agent.is_admin === 1
          })),
          carrierSettings: {
            selectedCarriers: (orgSettings as any)?.carrierContracts || [],
            useSmartSend: (orgSettings as any)?.smartSendEnabled || true
          },
          paymentStatus: {
            paymentCompleted: orgInfo.payment_completed === 1,
            stripeCustomerId: orgInfo.stripe_customer_id || null,
            stripeSubscriptionId: orgInfo.stripe_subscription_id || null
          },
          // Include the raw data for debugging
          raw: {
            orgInfo,
            userInfo,
            orgSettings
          }
        };
      } catch (error) {
        logger.error(`Error in resume test endpoint: ${error}`);
        set.status = 500;
        return { error: String(error) };
      }
    })

  // Ensure we're properly registering all routes with explicit paths
  logger.info(`Registered routes: ${app.routes.map(r => r.path).join(', ')}`);
  
  return app;
}