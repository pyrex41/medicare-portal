import { Elysia } from 'elysia'
import { logger } from '../logger'
import { Database } from '../database'
import { config } from '../config'
import { generateToken, getUserFromSession } from '../services/auth'
import { cookie } from '@elysiajs/cookie'

// Define types for onboarding data
interface OnboardingData {
  plan: {
    type: string
    price: number
    billingCycle: string
    extraAgents: number
    extraContacts: number
  }
  user: {
    firstName: string
    lastName: string
    email: string
    phone: string
    bookingLink: string
  }
  company: {
    agencyName: string
    website: string
    phone: string
    primaryColor: string
    secondaryColor: string
    logo?: string
  }
  licensing: {
    stateLicenses: string[]
    carrierContracts: string[]
    stateCarrierSettings: StateCarrierSetting[]
  }
  agents: any[] // Using any for flexibility, could be more specific
}

interface StateCarrierSetting {
  state: string
  carrier: string
  active: boolean
  targetGI: boolean
}

// Helper function for generating random slugs
const generateRandomSlug = () => {
  const characters = 'abcdefghijklmnopqrstuvwxyz0123456789';
  const length = 12;
  let result = '';
  for (let i = 0; i < length; i++) {
    result += characters.charAt(Math.floor(Math.random() * characters.length));
  }
  return result;
};

export function createOnboardingRoutes() {
  const dbInstance = new Database()

  return new Elysia()
    .use(cookie())
    // New endpoint for initial account creation without email requirement
    .post('/api/onboarding/initialize', async ({ body, set, setCookie }) => {
      try {
        const { planType } = body as { planType: string };
        
        logger.info(`Initializing onboarding with plan: ${planType}`);
        
        // Generate a completely random slug (12 characters)
        const slug = generateRandomSlug();
        
        // Create temporary session token
        const tempSessionId = generateToken();
        
        // Set session cookie for 24 hours
        setCookie('onboardingSession', tempSessionId, {
          httpOnly: true,
          maxAge: 60 * 60 * 24, // 24 hours
          path: '/'
        });
        
        // Create organization with minimal info
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
          ['New Organization', planType || 'basic', slug, 1, tempSessionId]
        );
        
        const orgId = Number(orgResult.lastInsertRowid);
        logger.info(`Created initial organization: ${orgId} with slug: ${slug}`);
        
        return {
          organizationId: orgId,
          slug: slug,
          sessionToken: tempSessionId,
          onboardingStep: 1
        };
        
      } catch (error) {
        logger.error(`Error initializing onboarding: ${error}`);
        set.status = 500;
        return { 
          error: 'Failed to initialize onboarding' 
        };
      }
    })
    
    // New endpoint to resume onboarding by email
    .post('/api/onboarding/resume-onboarding', async ({ body, set, setCookie }) => {
      try {
        const { email } = body as { email: string };
        
        logger.info(`Attempting to resume onboarding for email: ${email}`);
        
        if (!email || !email.trim()) {
          logger.warn('Attempt to resume onboarding with empty email');
          set.status = 400;
          return { 
            error: 'Email is required' 
          };
        }
        
        // Find user and organization by email
        const userData = await dbInstance.query<{ 
          organization_id: number, 
          slug: string, 
          onboarding_step: number,
          temp_session_id: string 
        }>(
          `SELECT u.organization_id, o.slug, o.onboarding_step, o.temp_session_id
           FROM users u
           JOIN organizations o ON u.organization_id = o.id
           WHERE LOWER(u.email) = LOWER(?) AND o.onboarding_completed = FALSE`,
          [email]
        );
        
        if (!userData || userData.length === 0) {
          logger.warn(`No in-progress onboarding found for email: ${email}`);
          set.status = 404;
          return { 
            error: 'No onboarding in progress for this email' 
          };
        }
        
        const { organization_id, slug, onboarding_step, temp_session_id } = userData[0];
        
        // Update session token if needed
        let sessionToken = temp_session_id;
        if (!sessionToken) {
          sessionToken = generateToken();
          await dbInstance.execute(
            'UPDATE organizations SET temp_session_id = ? WHERE id = ?',
            [sessionToken, organization_id]
          );
        }
        
        // Set session cookie
        setCookie('onboardingSession', sessionToken, {
          httpOnly: true,
          maxAge: 60 * 60 * 24, // 24 hours
          path: '/'
        });
        
        logger.info(`Resumed onboarding for organization: ${organization_id}, step: ${onboarding_step}`);
        
        return {
          organizationId: organization_id,
          slug: slug,
          sessionToken: sessionToken,
          onboardingStep: onboarding_step
        };
        
      } catch (error) {
        logger.error(`Error resuming onboarding: ${error}`);
        set.status = 500;
        return { 
          error: 'Failed to resume onboarding' 
        };
      }
    })
    
    // New endpoint for initial account creation
    .post('/api/organizations/init-onboarding', async ({ body, set }) => {
      try {
        const { planType, email } = body as { planType: string, email: string };
        
        logger.info(`Initializing onboarding for email: "${email}" with plan: ${planType}`);
        
        if (!email || !email.trim()) {
          logger.warn('Attempt to initialize onboarding with empty email');
          set.status = 400;
          return { 
            success: false, 
            message: 'Email is required' 
          };
        }
        
        // Check if email already exists
        const emailCheck = await dbInstance.query<{ count: number }>(
          'SELECT COUNT(*) as count FROM users WHERE LOWER(email) = LOWER(?)',
          [email]
        );
        
        logger.info(`Email check for "${email}": ${JSON.stringify(emailCheck)}`);
        
        if (emailCheck[0]?.count > 0) {
          logger.warn(`Email "${email}" is already in use`);
          set.status = 400;
          return { 
            success: false, 
            message: 'Email is already in use' 
          };
        }
        
        // Generate a completely random slug (12 characters)
        const slug = generateRandomSlug();
        logger.info(`Generated random slug: ${slug} for initial onboarding`);
        
        // Create temporary session token
        const tempSessionId = generateToken();
        
        // Create organization with minimal info
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
          ['New Organization', planType || 'basic', slug, 1, tempSessionId]
        );
        
        const orgId = Number(orgResult.lastInsertRowid);
        logger.info(`Created initial organization: ${orgId} with slug: ${slug}`);
        
        // Create user with minimal info - email only
        await dbInstance.execute(`
          INSERT INTO users (
            email, 
            first_name,
            last_name,
            is_admin, 
            is_agent, 
            organization_id, 
            created_at, 
            is_active
          ) VALUES (?, '', '', 1, 1, ?, datetime('now'), 0)`,
          [email, orgId]
        );
        
        logger.info(`Created initial user with email: "${email}" for org: ${orgId}`);
        
        return {
          success: true,
          organizationId: orgId,
          slug: slug,
          sessionToken: tempSessionId,
          onboardingStep: 1
        };
        
      } catch (error) {
        logger.error(`Error initializing onboarding: ${error}`);
        set.status = 500;
        return { 
          success: false, 
          message: 'Failed to initialize onboarding' 
        };
      }
    })
    
    // Update user details
    .put('/api/organizations/:orgSlug/update-user', async ({ params, body, request, set }) => {
      try {
        const { orgSlug } = params;
        const { firstName, lastName, phone } = body as { 
          firstName: string, 
          lastName: string, 
          phone: string
        };
        
        logger.info(`Updating user details for organization ${orgSlug}`);
        
        // First try to get user from session
        const currentUser = await getUserFromSession(request);
        
        // Find organization and its admin user
        const orgInfo = await dbInstance.query<{ 
          id: number, 
          admin_id: number,
          admin_email: string 
        }>(
          `SELECT o.id, u.id as admin_id, u.email as admin_email
           FROM organizations o
           JOIN users u ON u.organization_id = o.id AND u.is_admin = 1
           WHERE o.slug = ?
           LIMIT 1`,
          [orgSlug]
        );
        
        if (!orgInfo || orgInfo.length === 0) {
          logger.warn(`Organization not found: ${orgSlug}`);
          set.status = 404;
          return { 
            success: false, 
            message: 'Organization not found' 
          };
        }
        
        const orgId = orgInfo[0].id;
        const adminId = orgInfo[0].admin_id;
        
        // If we have a session user, verify they belong to this org
        if (currentUser && currentUser.organization_id !== orgId) {
          logger.warn(`User ${currentUser.id} attempted to update details for org ${orgId}`);
          set.status = 403;
          return {
            success: false,
            message: 'Unauthorized: User does not belong to this organization'
          };
        }
        
        // Update user info
        await dbInstance.execute(`
          UPDATE users 
          SET first_name = ?, last_name = ?, phone = ? 
          WHERE id = ? AND organization_id = ? AND is_admin = 1`,
          [firstName, lastName, phone, adminId, orgId]
        );
        
        // Update onboarding step
        await dbInstance.execute(
          'UPDATE organizations SET onboarding_step = 2 WHERE id = ?',
          [orgId]
        );
        
        logger.info(`Updated user details for org ${orgSlug} (ID: ${orgId}) - Name: ${firstName} ${lastName}`);
        
        return {
          success: true,
          message: 'User details updated successfully',
          onboardingStep: 2,
          userEmail: orgInfo[0].admin_email
        };
        
      } catch (error) {
        logger.error(`Error updating user details: ${error}`);
        set.status = 500;
        return { 
          success: false, 
          message: 'Failed to update user details' 
        };
      }
    })
    
    // Update company details
    .put('/api/organizations/:orgSlug/update-company', async ({ params, body, set }) => {
      try {
        const { orgSlug } = params;
        const { 
          agencyName, 
          website, 
          phone, 
          primaryColor, 
          secondaryColor,
          logo,
          tempSessionId 
        } = body as { 
          agencyName: string,
          website: string,
          phone: string,
          primaryColor: string,
          secondaryColor: string,
          logo?: string,
          tempSessionId: string
        };
        
        logger.info(`Updating company details for organization ${orgSlug}`);
        
        // Find organization by slug and session token
        const orgInfo = await dbInstance.query<{ id: number }>(
          'SELECT id FROM organizations WHERE slug = ? AND temp_session_id = ?',
          [orgSlug, tempSessionId]
        );
        
        if (!orgInfo || orgInfo.length === 0) {
          logger.warn(`Invalid session or organization slug: ${orgSlug}`);
          set.status = 401;
          return { 
            success: false, 
            message: 'Unauthorized: Invalid session' 
          };
        }
        
        const orgId = orgInfo[0].id;
        
        // Update organization info
        await dbInstance.execute(`
          UPDATE organizations 
          SET name = ?, website = ?, phone = ?, primary_color = ?, secondary_color = ?
          WHERE id = ?`,
          [agencyName, website, phone, primaryColor, secondaryColor, orgId]
        );
        
        // Store logo if provided
        if (logo) {
          // Check if brand settings already exist
          const brandCheck = await dbInstance.query<{ count: number }>(
            'SELECT COUNT(*) as count FROM brand_settings WHERE organization_id = ?',
            [orgId]
          );
          
          if (brandCheck[0]?.count > 0) {
            // Update existing brand settings
            await dbInstance.execute(`
              UPDATE brand_settings 
              SET brand_name = ?, primary_color = ?, secondary_color = ?, logo_data = ?
              WHERE organization_id = ?`,
              [agencyName, primaryColor, secondaryColor, logo, orgId]
            );
          } else {
            // Create new brand settings
            await dbInstance.execute(`
              INSERT INTO brand_settings (
                organization_id, brand_name, primary_color, secondary_color, logo_data
              ) VALUES (?, ?, ?, ?, ?)`,
              [orgId, agencyName, primaryColor, secondaryColor, logo]
            );
          }
        }
        
        // Update onboarding step
        await dbInstance.execute(
          'UPDATE organizations SET onboarding_step = 3 WHERE id = ?',
          [orgId]
        );
        
        logger.info(`Updated company details for org ${orgSlug} (ID: ${orgId}) - Name: ${agencyName}`);
        
        return {
          success: true,
          message: 'Company details updated successfully',
          onboardingStep: 3
        };
        
      } catch (error) {
        logger.error(`Error updating company details: ${error}`);
        set.status = 500;
        return { 
          success: false, 
          message: 'Failed to update company details' 
        };
      }
    })
    
    // Update licensing settings
    .put('/api/organizations/:orgSlug/update-licensing', async ({ params, body, set }) => {
      try {
        const { orgSlug } = params;
        const { 
          stateLicenses, 
          carrierContracts, 
          stateCarrierSettings,
          tempSessionId 
        } = body as { 
          stateLicenses: string[],
          carrierContracts: string[],
          stateCarrierSettings: StateCarrierSetting[],
          tempSessionId: string
        };
        
        logger.info(`Updating licensing settings for organization ${orgSlug}`);
        
        // Find organization by slug and session token
        const orgInfo = await dbInstance.query<{ id: number, tier: string }>(
          'SELECT id, subscription_tier as tier FROM organizations WHERE slug = ? AND temp_session_id = ?',
          [orgSlug, tempSessionId]
        );
        
        if (!orgInfo || orgInfo.length === 0) {
          logger.warn(`Invalid session or organization slug: ${orgSlug}`);
          set.status = 401;
          return { 
            success: false, 
            message: 'Unauthorized: Invalid session' 
          };
        }
        
        const orgId = orgInfo[0].id;
        
        // Store licensing information in agent_settings table
        try {
          // First delete any existing settings
          await dbInstance.execute(
            'DELETE FROM agent_settings WHERE organization_id = ?',
            [orgId]
          );
          
          // Insert new settings - using org-level settings
          await dbInstance.execute(`
            INSERT INTO agent_settings (
              organization_id,
              state_licenses,
              carrier_contracts,
              state_carrier_settings,
              settings_type
            ) VALUES (?, ?, ?, ?, 'organization')`,
            [
              orgId,
              JSON.stringify(stateLicenses),
              JSON.stringify(carrierContracts),
              JSON.stringify(stateCarrierSettings)
            ]
          );
          
          logger.info(`Saved licensing settings for org ${orgSlug} (ID: ${orgId}) - States: ${stateLicenses.length}, Carriers: ${carrierContracts.length}`);
        } catch (settingsError) {
          logger.error(`Error saving licensing settings: ${settingsError}`);
          // Continue execution even if this fails - non-critical
        }
        
        // Update onboarding step
        await dbInstance.execute(
          'UPDATE organizations SET onboarding_step = 4 WHERE id = ?',
          [orgId]
        );
        
        const isBasicPlan = orgInfo[0]?.tier === 'basic';
        const nextStep = isBasicPlan ? 5 : 4; // Skip agents step if basic plan
        
        return {
          success: true,
          message: 'Licensing settings updated successfully',
          onboardingStep: 4,
          nextStep: nextStep, // The frontend can use this to determine where to go next
          isBasicPlan: isBasicPlan
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
    
    // Add team members/agents
    .post('/api/organizations/:orgSlug/add-agents', async ({ params, body, set }) => {
      try {
        const { orgSlug } = params;
        const { agents, tempSessionId } = body as { 
          agents: Array<{
            firstName: string,
            lastName: string,
            email: string,
            phone: string,
            isAdmin: boolean,
            isAgent: boolean
          }>,
          tempSessionId: string
        };
        
        logger.info(`Adding agents for organization ${orgSlug}`);
        
        // Find organization by slug and session token
        const orgInfo = await dbInstance.query<{ id: number }>(
          'SELECT id FROM organizations WHERE slug = ? AND temp_session_id = ?',
          [orgSlug, tempSessionId]
        );
        
        if (!orgInfo || orgInfo.length === 0) {
          logger.warn(`Invalid session or organization slug: ${orgSlug}`);
          set.status = 401;
          return { 
            success: false, 
            message: 'Unauthorized: Invalid session' 
          };
        }
        
        const orgId = orgInfo[0].id;
        
        // Process agents in transaction to ensure all-or-nothing
        const addedAgents = await dbInstance.transaction(async (db) => {
          const addedEmails = [];
          
          for (const agent of agents) {
            // Check if email already exists
            const emailCheck = await db.query<{ count: number }>(
              'SELECT COUNT(*) as count FROM users WHERE LOWER(email) = LOWER(?)',
              [agent.email]
            );
            
            if (emailCheck[0]?.count > 0) {
              logger.warn(`Skip adding agent with email ${agent.email} - already exists`);
              continue; // Skip this agent, email already exists
            }
            
            // Insert new agent
            await db.execute(`
              INSERT INTO users (
                email,
                first_name,
                last_name,
                phone,
                is_admin,
                is_agent,
                organization_id,
                created_at,
                is_active
              ) VALUES (?, ?, ?, ?, ?, ?, ?, datetime('now'), 1)`,
              [
                agent.email,
                agent.firstName,
                agent.lastName,
                agent.phone || '',
                agent.isAdmin ? 1 : 0,
                agent.isAgent ? 1 : 0,
                orgId
              ]
            );
            
            logger.info(`Added agent: ${agent.firstName} ${agent.lastName} (${agent.email})`);
            addedEmails.push(agent.email);
          }
          
          return addedEmails;
        });
        
        // Update onboarding step
        await dbInstance.execute(
          'UPDATE organizations SET onboarding_step = 5 WHERE id = ?',
          [orgId]
        );
        
        return {
          success: true,
          message: `Added ${addedAgents.length} agents successfully`,
          addedAgents: addedAgents,
          onboardingStep: 5
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
    
    // Complete onboarding and set up subscription
    .post('/api/organizations/:orgSlug/complete-subscription', async ({ params, body, set }) => {
      try {
        const { orgSlug } = params;
        const { 
          tierId, 
          extraAgents, 
          extraContacts,
          tempSessionId
        } = body as {
          tierId: string,
          extraAgents: number,
          extraContacts: number,
          tempSessionId: string
        };
        
        logger.info(`Completing onboarding for organization ${orgSlug} with plan ${tierId}`);
        
        // Find organization by slug and session token
        const orgInfo = await dbInstance.query<{ id: number, email: string }>(
          `SELECT o.id, (SELECT email FROM users WHERE organization_id = o.id AND is_admin = 1 LIMIT 1) as email 
           FROM organizations o WHERE o.slug = ? AND o.temp_session_id = ?`,
          [orgSlug, tempSessionId]
        );
        
        if (!orgInfo || orgInfo.length === 0) {
          logger.warn(`Invalid session or organization slug: ${orgSlug}`);
          set.status = 401;
          return { 
            success: false, 
            message: 'Unauthorized: Invalid session' 
          };
        }
        
        const orgId = orgInfo[0].id;
        
        // Import the Stripe service here
        const { createOrUpdateSubscription } = await import('../services/stripe');
        
        try {
          // Get admin user's email for Stripe customer
          const userEmail = orgInfo[0]?.email;
          if (!userEmail) {
            throw new Error('Admin user email not found');
          }
          
          // Create or update the Stripe subscription
          const stripeResult = await createOrUpdateSubscription({
            tierId: tierId as 'basic' | 'pro' | 'enterprise',
            organizationId: orgId,
            email: userEmail,
            extraAgents: extraAgents || 0,
            extraContacts: extraContacts || 0
          });
          
          // Update organization with Stripe IDs and subscription tier
          await dbInstance.execute(`
            UPDATE organizations 
            SET subscription_tier = ?,
                stripe_customer_id = ?,
                stripe_subscription_id = ?,
                onboarding_completed = TRUE,
                onboarding_step = 6,
                extra_agents = ?,
                extra_contacts = ?,
                temp_session_id = NULL
            WHERE id = ?`,
            [
              tierId, 
              stripeResult.customerId, 
              stripeResult.subscriptionId, 
              extraAgents || 0,
              extraContacts || 0,
              orgId
            ]
          );
          
          // Activate the admin user
          await dbInstance.execute(`
            UPDATE users
            SET is_active = 1
            WHERE organization_id = ? AND is_admin = 1`,
            [orgId]
          );
          
          // Set up the database for the organization
          const { TursoService } = await import('../services/turso');
          const turso = new TursoService();
          
          try {
            const { url, token } = await turso.createOrganizationDatabase(orgId.toString());
            
            // Update organization with Turso database credentials
            await dbInstance.execute(
              'UPDATE organizations SET turso_db_url = ?, turso_auth_token = ? WHERE id = ?',
              [url, token, orgId]
            );
            
            logger.info(`Created Turso database for organization ${orgId}`);
          } catch (dbError) {
            logger.error(`Error creating database: ${dbError}`);
            // Continue execution even if this fails - can be set up later
          }
          
          logger.info(`Successfully completed onboarding for org ${orgSlug} (ID: ${orgId}) with plan ${tierId}`);
          
          // Return the client secret for frontend payment completion
          return {
            success: true,
            message: 'Subscription activated successfully',
            clientSecret: stripeResult.clientSecret,
            publishableKey: config.stripe.publishableKey,
            organizationId: orgId
          };
          
        } catch (stripeError) {
          logger.error(`Stripe subscription error: ${stripeError}`);
          set.status = 400;
          return {
            success: false,
            error: 'Failed to process subscription payment'
          };
        }
        
      } catch (error) {
        logger.error(`Error completing onboarding: ${error}`);
        set.status = 500;
        return { 
          success: false, 
          message: 'Failed to complete onboarding process' 
        };
      }
    })
    
    // Resume an in-progress onboarding
    .post('/api/organizations/resume-onboarding', async ({ body, set }) => {
      try {
        const { email, tempSessionId } = body as { 
          email: string,
          tempSessionId?: string 
        };
        
        if (!email) {
          logger.warn('Attempt to resume onboarding without email');
          set.status = 400;
          return {
            success: false,
            message: 'Email is required'
          };
        }
        
        logger.info(`Attempting to resume onboarding for email: "${email}"`);
        
        // Find organization by email and temporary session ID
        let query = `
          SELECT 
            o.id, 
            o.slug, 
            o.onboarding_step, 
            o.temp_session_id, 
            o.subscription_tier, 
            o.name as organization_name,
            u.email, 
            u.first_name, 
            u.last_name,
            u.phone
          FROM organizations o 
          JOIN users u ON u.organization_id = o.id 
          WHERE LOWER(u.email) = LOWER(?) 
            AND u.is_admin = 1
            AND o.onboarding_completed = FALSE
        `;
        
        const params: any[] = [email];
        
        // Add session ID check if provided
        if (tempSessionId) {
          query += ' AND o.temp_session_id = ?';
          params.push(tempSessionId);
        }
        
        // Limit to one result
        query += ' LIMIT 1';
        
        const result = await dbInstance.query(query, params);
        
        if (!result || result.length === 0) {
          logger.warn(`No in-progress onboarding found for email: ${email}`);
          set.status = 404;
          return {
            success: false,
            message: 'No in-progress onboarding found for this email'
          };
        }
        
        const onboarding = result[0];
        
        // If there's no session ID or it doesn't match, create a new one
        let sessionId = onboarding.temp_session_id;
        if (!sessionId) {
          sessionId = generateToken();
          await dbInstance.execute(
            'UPDATE organizations SET temp_session_id = ? WHERE id = ?',
            [sessionId, onboarding.id]
          );
          logger.info(`Created new session token for org ${onboarding.id}`);
        }
        
        logger.info(`Resumed onboarding for org ${onboarding.slug} (ID: ${onboarding.id}) at step ${onboarding.onboarding_step}`);
        
        return {
          success: true,
          message: 'Onboarding session found',
          organizationId: onboarding.id,
          slug: onboarding.slug,
          onboardingStep: onboarding.onboarding_step,
          sessionToken: sessionId,
          planType: onboarding.subscription_tier,
          organization: {
            name: onboarding.organization_name || 'New Organization'
          },
          user: {
            email: onboarding.email,
            firstName: onboarding.first_name || '',
            lastName: onboarding.last_name || '',
            phone: onboarding.phone || ''
          }
        };
        
      } catch (error) {
        logger.error(`Error resuming onboarding: ${error}`);
        set.status = 500;
        return { 
          success: false, 
          message: 'Failed to resume onboarding process' 
        };
      }
    })
    
    // Original complete-onboarding endpoint (kept for compatibility)
    .post('/api/organizations/complete-onboarding', async ({ body, set }) => {
      const data = body as any; // Use any temporarily for conversion
      
      try {
        const userEmail = data.user?.email || 'unknown';
        logger.info(`Starting complete onboarding process for "${userEmail}"`);
        logger.info(`Onboarding data received: ${JSON.stringify(data, null, 2)}`);
        
        // Adapt the data structure to handle both formats
        const adaptedData: OnboardingData = {
          plan: {
            type: data.plan?.type || data.subscription?.tierId || 'basic',
            price: data.plan?.price || 29,
            billingCycle: data.plan?.billingCycle || 'monthly',
            extraAgents: data.plan?.extraAgents || data.subscription?.extraAgents || 0,
            extraContacts: data.plan?.extraContacts || data.subscription?.extraContacts || 0
          },
          user: data.user,
          company: data.company,
          licensing: data.licensing,
          agents: data.agents || []
        };
        
        logger.info(`Processed onboarding data (user): ${JSON.stringify(adaptedData.user, null, 2)}`);
        
        // Use the transaction method instead of manual transaction management
        const result = await dbInstance.transaction(async (db) => {
          // Generate a completely random slug (12 characters)
          const generateRandomSlug = () => {
            const characters = 'abcdefghijklmnopqrstuvwxyz0123456789';
            const length = 12;
            let result = '';
            for (let i = 0; i < length; i++) {
              result += characters.charAt(Math.floor(Math.random() * characters.length));
            }
            return result;
          };
          
          const slug = generateRandomSlug();
          logger.info(`Generated random slug: ${slug}`);
            
          // 1. Create organization with slug included
          const orgResult = await db.execute(`
            INSERT INTO organizations (
              name, 
              website, 
              phone,
              primary_color,
              secondary_color,
              subscription_tier,
              created_at,
              onboarding_completed,
              slug,
              agent_limit,
              contact_limit,
              extra_agents,
              extra_contacts
            ) VALUES (?, ?, ?, ?, ?, ?, datetime('now'), TRUE, ?, ?, ?, ?, ?)`,
            [
              adaptedData.company.agencyName || 'New Organization', // Provide a default if empty
              adaptedData.company.website || '',
              adaptedData.company.phone || '',
              adaptedData.company.primaryColor || '#0A0F4F',
              adaptedData.company.secondaryColor || '#7B61FF',
              adaptedData.plan.type,
              slug,
              adaptedData.plan.type === 'basic' ? 1 : 3, // Basic: 1 agent, Pro: 3 agents
              adaptedData.plan.type === 'basic' ? 100 : 1000, // Basic: 100 contacts, Pro: 1000
              adaptedData.plan.extraAgents,
              adaptedData.plan.extraContacts
            ]
          )
          
          const organizationId = Number(orgResult.lastInsertRowid)
          logger.info(`Created organization: ${organizationId} (${adaptedData.company.agencyName}) with slug: ${slug}`)
          
          // 2. Create admin user
          logger.info(`About to create admin user with email: "${adaptedData.user.email}"`);
          logger.info(`SQL: INSERT INTO users (email, first_name, last_name, phone, is_admin, is_agent, organization_id, created_at) VALUES ('${adaptedData.user.email}', '${adaptedData.user.firstName}', '${adaptedData.user.lastName}', '${adaptedData.user.phone || ''}', 1, 1, ${organizationId}, datetime('now'))`);
          
          // First check if the email already exists (for debugging)
          const emailCheckResult = await db.query<{ count: number }>(
            'SELECT COUNT(*) as count FROM users WHERE LOWER(email) = LOWER(?)',
            [adaptedData.user.email]
          );
          logger.info(`Pre-check if email exists: ${JSON.stringify(emailCheckResult)} for email "${adaptedData.user.email}"`);
          
          // Check for similar emails for debugging purposes
          if (adaptedData.user.email.includes('@')) {
            const emailParts = adaptedData.user.email.split('@');
            const emailUsername = emailParts[0];
            const emailDomain = emailParts[1];
            
            // Find similar emails for debugging
            const similarEmails = await db.query<{ email: string }>(
              "SELECT email FROM users WHERE email LIKE ? OR email LIKE ?",
              [`%${emailUsername}%@${emailDomain}`, `%@${emailDomain}`]
            );
            
            logger.info(`Similar emails found in database: ${JSON.stringify(similarEmails)}`);
          }
          
          // Proceed with insert
          const userResult = await db.execute(`
            INSERT INTO users (
              email,
              first_name,
              last_name,
              phone,
              is_admin,
              is_agent,
              organization_id,
              created_at
            ) VALUES (?, ?, ?, ?, 1, 1, ?, datetime('now'))`,
            [
              adaptedData.user.email,
              adaptedData.user.firstName,
              adaptedData.user.lastName,
              adaptedData.user.phone || '',
              organizationId
            ]
          )
          
          const userId = Number(userResult.lastInsertRowid)
          logger.info(`Created admin user: ${userId} (${adaptedData.user.email})`)
          
          // Update user with booking link if provided
          if (adaptedData.user.bookingLink) {
            await db.execute(`
              UPDATE users SET booking_link = ? WHERE id = ?`,
              [adaptedData.user.bookingLink, userId]
            )
          }
          
          // Add logo to brand settings if provided
          if (adaptedData.company.logo) {
            await db.execute(`
              INSERT INTO brand_settings (
                organization_id,
                brand_name,
                primary_color,
                secondary_color,
                logo_data
              ) VALUES (?, ?, ?, ?, ?)`,
              [
                organizationId,
                adaptedData.company.agencyName,
                adaptedData.company.primaryColor || '#0A0F4F',
                adaptedData.company.secondaryColor || '#7B61FF',
                adaptedData.company.logo
              ]
            )
          }
          
          // We're not creating agent_settings here since we're using org-level settings
          // Licensing information will be handled at the org level
          
          // 7. If additional agents were added, process them
          if (adaptedData.agents && adaptedData.agents.length > 0) {
            logger.info(`Processing ${adaptedData.agents.length} additional agents`)
            
            for (const agent of adaptedData.agents) {
              const agentResult = await db.execute(`
                INSERT INTO users (
                  email,
                  first_name,
                  last_name,
                  phone,
                  is_admin,
                  is_agent,
                  organization_id,
                  created_at
                ) VALUES (?, ?, ?, ?, ?, 1, ?, datetime('now'))`,
                [
                  agent.email,
                  agent.firstName,
                  agent.lastName,
                  agent.phone || '',
                  agent.isAdmin ? 1 : 0,
                  organizationId
                ]
              )
              
              logger.info(`Created additional agent: ${agentResult.lastInsertRowid} (${agent.email})`)
            }
          }
          
          return {
            organizationId,
            organizationSlug: slug
          }
        })
        
        // Transaction was successful
        return {
          success: true,
          message: "Organization created successfully",
          data: result
        }
        
      } catch (error) {
        logger.error(`Error in complete onboarding: ${error}`)
        
        set.status = 500
        return {
          success: false,
          message: `Failed to complete onboarding: ${error instanceof Error ? error.message : 'Unknown error'}`
        }
      }
    })

    // New endpoint to finalize onboarding
    .post('/api/organizations/finalize-onboarding', async ({ body, set }) => {
      try {
        const { orgSlug, sessionToken } = body as { 
          orgSlug: string,
          sessionToken: string
        };
        
        logger.info(`Finalizing onboarding for organization ${orgSlug}`);
        
        // Find organization by slug and session token
        const orgInfo = await dbInstance.query<{ id: number }>(
          'SELECT id FROM organizations WHERE slug = ? AND temp_session_id = ?',
          [orgSlug, sessionToken]
        );
        
        if (!orgInfo || orgInfo.length === 0) {
          logger.warn(`Invalid session or organization slug: ${orgSlug}`);
          set.status = 401;
          return { 
            success: false, 
            message: 'Unauthorized: Invalid session' 
          };
        }
        
        const orgId = orgInfo[0].id;
        
        // Mark onboarding as completed and clear temp session
        await dbInstance.execute(`
          UPDATE organizations 
          SET onboarding_completed = TRUE,
              onboarding_step = 6,
              temp_session_id = NULL
          WHERE id = ?`,
          [orgId]
        );
        
        // Activate the admin user
        await dbInstance.execute(`
          UPDATE users
          SET is_active = 1
          WHERE organization_id = ? AND is_admin = 1`,
          [orgId]
        );
        
        logger.info(`Successfully finalized onboarding for org ${orgSlug} (ID: ${orgId})`);
        
        return {
          success: true,
          message: 'Onboarding completed successfully'
        };
        
      } catch (error) {
        logger.error(`Error finalizing onboarding: ${error}`);
        set.status = 500;
        return { 
          success: false, 
          message: 'Failed to finalize onboarding process' 
        };
      }
    })
}

// Function to clean up old organizations that haven't completed onboarding
export async function cleanupOldOrganizations() {
  const dbInstance = new Database();
  
  try {
    logger.info('Running cleanup job for old organizations');
    
    // Find organizations older than 7 days
    const oldOrgs = await dbInstance.query<{ id: number }>(
      `SELECT id FROM organizations 
       WHERE created_at < datetime('now', '-7 days') 
       AND onboarding_completed = FALSE`
    );
    
    if (!oldOrgs || oldOrgs.length === 0) {
      logger.info('No old organizations to clean up');
      return;
    }
    
    logger.info(`Found ${oldOrgs.length} old organizations to check for cleanup`);
    
    for (const org of oldOrgs) {
      // Check if the organization has users
      const userCount = await dbInstance.query<{ count: number }>(
        'SELECT COUNT(*) as count FROM users WHERE organization_id = ?',
        [org.id]
      );
      
      // Check if the organization has a contact database
      const contactDbCount = await dbInstance.query<{ count: number }>(
        'SELECT COUNT(*) as count FROM contact_databases WHERE organization_id = ?',
        [org.id]
      );
      
      // If no users and no contact database, delete the organization
      if (userCount[0]?.count === 0 && contactDbCount[0]?.count === 0) {
        logger.info(`Deleting old organization: ${org.id}`);
        await dbInstance.execute(
          'DELETE FROM organizations WHERE id = ?',
          [org.id]
        );
      }
    }
    
    logger.info('Completed cleanup job for old organizations');
  } catch (error) {
    logger.error(`Error in cleanup job: ${error}`);
  }
}

export default createOnboardingRoutes
