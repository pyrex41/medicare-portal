import { Elysia } from 'elysia';
import { Database } from '../database';
import { TursoService } from '../services/turso';
import { z } from 'zod';
import { sendMagicLink } from '../services/email';
import { generateToken, getUserFromSession } from '../services/auth';
import { logger } from '../logger';
import { AuthService } from '../services/auth';

// Update the validation schema to include slug rules
const signupSchema = z.object({
  organizationName: z.string()
    .min(1, "Organization name is required")
    .max(100, "Organization name must be less than 100 characters")
    .regex(/^[a-zA-Z0-9\s\-_.]+$/, "Organization name can only contain letters, numbers, spaces, hyphens, dots, and underscores"),
  adminFirstName: z.string().min(1, "First name is required"),
  adminLastName: z.string().min(1, "Last name is required"),
  adminEmail: z.string().email("Invalid email address"),
});

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

export const organizationRoutes = new Elysia({ prefix: '/api' })
  .post('/organizations/signup', async ({ body, set }) => {
    const db = new Database();
    const turso = new TursoService();
    const auth = new AuthService();

    try {
      logger.info(`Attempting to create organization with data: ${JSON.stringify(body)}`);
      const data = signupSchema.parse(body);
      
      // Generate unique slug
      const slug = await generateUniqueSlug(db, data.organizationName);
      logger.info(`Generated unique slug: ${slug}`);
      
      // Check if email is already registered in any organization
      const existingUser = await db.query<{ count: number }>(
        'SELECT COUNT(*) as count FROM users WHERE email = ?',
        [data.adminEmail]
      );

      logger.info(`Existing user check result: ${JSON.stringify(existingUser)}`);

      if (existingUser[0]?.count > 0) {
        logger.warn(`Email ${data.adminEmail} is already registered`);
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
            data.adminEmail,
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

      // Generate and send magic link outside the transaction
      logger.info('Generating magic link');
      const magicLink = await auth.createMagicLink(
        data.adminEmail, 
        slug,
        {
          redirectUrl: '/choose-plan',
          orgId,
          name: `${data.adminFirstName} ${data.adminLastName}`
        }
      );

      logger.info(`Magic link generated successfully: ${magicLink}`);
      await sendMagicLink({
        email: data.adminEmail,
        magicLink,
        name: `${data.adminFirstName} ${data.adminLastName}`
      });

      logger.info('Magic link email sent successfully');
      set.status = 201;
      return { 
        success: true,
        message: 'Please check your email to verify your account'
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
      const decodedEmail = decodeURIComponent(params.email);
      
      // Basic email format validation
      if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(decodedEmail)) {
        return {
          available: false,
          message: 'Invalid email format'
        };
      }

      const existingUser = await db.query<{ count: number }>(
        'SELECT COUNT(*) as count FROM users WHERE email = ?',
        [decodedEmail]
      );

      const count = existingUser[0]?.count || 0;

      if (count > 0) {
        return {
          available: false,
          message: 'This email address is already registered'
        };
      }

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
    const db = new Database();
    try {
      const tiersResult = await db.query<{
        id: string,
        name: string,
        agent_limit: number,
        contact_limit: number,
        price_monthly: number,
        features: string
      }>('SELECT id, name, agent_limit, contact_limit, price_monthly, features FROM subscription_tiers');

      const tiers = tiersResult.map(tier => ({
        id: tier.id,
        name: tier.name,
        price: `$${(tier.price_monthly / 100).toFixed(0)}/mo`,
        agentLimit: tier.agent_limit,
        contactLimit: tier.contact_limit,
        features: JSON.parse(tier.features)
      }));

      return { success: true, tiers };
    } catch (error) {
      logger.error(`Error fetching subscription tiers: ${error}`);
      set.status = 500;
      return { success: false, error: 'Failed to fetch subscription tiers' };
    }
  })
  .post('/organizations/:orgSlug/subscription', async ({ params: { orgSlug }, body, request, set }) => {
    try {
      const db = new Database();

      // Get current user from session to determine their org
      const currentUser = await getUserFromSession(request)
      if (!currentUser) {
        set.status = 401
        return {
          success: false,
          error: 'You must be logged in to perform this action'
        }
      }

      // Add more detailed logging
      logger.info(`Updating subscription - orgSlug: ${orgSlug}, userId: ${currentUser.organization_id}, body: ${JSON.stringify(body)}`);

      // First verify this user belongs to the organization they're trying to update
      const orgResult = await db.fetchAll(
        'SELECT id FROM organizations WHERE slug = ?',
        [orgSlug]
      )

      if (!orgResult || orgResult.length === 0) {
        set.status = 404
        return {
          success: false,
          error: 'Organization not found'
        }
      }

      const organizationId = orgResult[0][0]

      // Verify user has permission for this org
      if (organizationId !== currentUser.organization_id) {
        logger.error(`User from org ${currentUser.organization_id} attempted to update org ${organizationId}`)
        set.status = 403
        return {
          success: false,
          error: 'You do not have permission to update this organization'
        }
      }

      // Type assertion for body
      const { tierId } = body as { tierId: string }
      
      // Update subscription using the verified organization ID
      await db.execute(
        `UPDATE organizations 
         SET subscription_tier = ?
         WHERE id = ?`,
        [tierId, organizationId]
      )

      // Set up the Turso database after subscription is saved
      const baseUrl = process.env.PUBLIC_URL || 'http://localhost:5173';
      const setupDbResponse = await fetch(`${baseUrl}/api/organizations/${orgSlug}/setup-database`, {
        method: 'POST',
        headers: {
          'Cookie': request.headers.get('cookie') || ''
        }
      });

      if (!setupDbResponse.ok) {
        logger.error(`Failed to set up database for org ${organizationId}`);
      } else {
        logger.info(`Successfully set up database for org ${organizationId}`);
      }

      logger.info(`Successfully updated subscription for org ${organizationId} to tier ${tierId}`)

      return {
        success: true,
        message: 'Subscription updated successfully'
      }

    } catch (e) {
      logger.error(`Error updating subscription: ${e}`)
      set.status = 500
      return {
        success: false,
        error: String(e)
      }
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

      // Update organization with Turso database credentials
      await db.execute(
        'UPDATE organizations SET turso_db_url = ?, turso_auth_token = ? WHERE id = ?',
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
  }); 