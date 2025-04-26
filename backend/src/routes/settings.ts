import { Elysia } from 'elysia';
import { validateSession } from '../services/auth';
import { Database } from '../database';
import { type User, type BaseSettings } from '../types';
import { logger } from '../logger';
import { cookie } from '@elysiajs/cookie';

interface StateCarrierSetting {
  state: string;
  carrier: string;
  active: boolean;
  targetGI: boolean;
}

interface SettingsBody {
    settings?: BaseSettings;
    inheritOrgSettings?: boolean;
}

interface AgentSettingsResponse {
  orgSettings: BaseSettings;
  agentSettings: {
    inheritOrgSettings: boolean;
    settings: BaseSettings;
  } | null;
  canEditOrgSettings: boolean;
}

const defaultSettings: BaseSettings = {
    stateLicenses: [],
    carrierContracts: [],
    stateCarrierSettings: [],
    allowAgentSettings: false,
    emailSendBirthday: false,
    emailSendPolicyAnniversary: false,
    emailSendAep: false,
    smartSendEnabled: false,
    brandName: "",
    logo: null,
    orgSignature: false
};

// Helper function to generate default state/carrier settings
function generateDefaultStateCarrierSettings(states: string[], carriers: string[]): StateCarrierSetting[] {
  return states.flatMap(state => 
    carriers.map(carrier => ({
      state,
      carrier,
      active: true,  // Default to active
      targetGI: false  // Default to no GI
    }))
  );
}

export const settingsRoutes = new Elysia()
  .get('/api/settings', async ({ cookie }) => {
    logger.info('GET /api/settings - Starting');
    
    const user = await validateSession(cookie.session.toString());
    if (!user?.id) {
        return { success: false, error: 'No authenticated user' };
    }

    const db = new Database();

    try {
        // Get organization settings, logo, and name
        const orgRow = await db.fetchOne<{ org_settings: string | null, logo_data: string | null, name: string, org_signature: boolean, phone: string | null, redirect_url: string | null }>(
            'SELECT org_settings, logo_data, name, org_signature, phone, redirect_url FROM organizations WHERE id = ?',
            [user.organization_id]
        );
        
        logger.info(`Retrieved org settings row`);

        // Parse the JSON string into an object
        let orgSettings: BaseSettings;
        try {
            orgSettings = orgRow?.org_settings 
                ? { ...defaultSettings, ...JSON.parse(orgRow.org_settings) }
                : { ...defaultSettings };

            // If brandName is not set in settings, use the organization name
            if (!orgSettings.brandName && orgRow?.name) {
                orgSettings.brandName = orgRow.name;
            }

            // Add phone and redirectUrl from database columns if available
            if (orgRow?.phone) {
                orgSettings.phone = orgRow.phone;
            }
            
            if (orgRow?.redirect_url) {
                orgSettings.redirectUrl = orgRow.redirect_url;
            }

            // If we have states and carriers but no settings array, generate them
            if (orgSettings.stateLicenses.length > 0 && 
                orgSettings.carrierContracts.length > 0 && 
                !Array.isArray(orgSettings.stateCarrierSettings)) {
                
                logger.info('Generating default state/carrier settings');
                orgSettings.stateCarrierSettings = generateDefaultStateCarrierSettings(
                    orgSettings.stateLicenses,
                    orgSettings.carrierContracts
                );
            }
        } catch (parseError) {
            logger.error(`Error parsing org settings: ${parseError}`);
            orgSettings = { ...defaultSettings };
        }

        // Get agent settings if they exist
        const agentSettingsRow = await db.fetchOne<{ inherit_org_settings: boolean, settings: string | null }>(
            'SELECT inherit_org_settings, settings FROM agent_settings WHERE agent_id = ?',
            [user.id]
        );

        // Parse agent settings
        let agentSettings = null;
        if (agentSettingsRow) {
            try {
                const parsedSettings = agentSettingsRow.settings 
                    ? { ...defaultSettings, ...JSON.parse(agentSettingsRow.settings) }
                    : { ...defaultSettings };

                agentSettings = {
                    inheritOrgSettings: agentSettingsRow.inherit_org_settings,
                    settings: parsedSettings
                };
            } catch (parseError) {
                logger.error(`Error parsing agent settings: ${parseError}`);
            }
        }

        const canEditOrgSettings = user.is_admin;

        const response = {
            success: true,
            name: orgRow?.name || null,
            orgSettings,
            logo: orgRow?.logo_data || null,
            orgSignature: orgRow?.org_signature || false,
            agentSettings,
            canEditOrgSettings
        };

        logger.info(`Sending response`);
        return response;

    } catch (error) {
        logger.error(`Error fetching settings: ${error}`);
        return {
            success: false,
            error: 'Failed to load settings'
        };
    }
  })

  .put('/api/settings/:scope', async ({ cookie, body, params }) => {
    const { scope } = params;
    logger.info(`PUT /api/settings/${scope} - Starting`);
    
    const user = await validateSession(cookie?.session?.toString() || '');
    if (!user?.id) {
        return { success: false, error: 'No authenticated user' };
    }

    const db = new Database();

    try {
        // Get organization's subscription tier
        const orgRow = await db.fetchOne<{ subscription_tier: string }>(
            'SELECT subscription_tier FROM organizations WHERE id = ?',
            [user.organization_id]
        );

        const isBasicTier = orgRow?.subscription_tier === 'basic';
        const typedBody = body as SettingsBody;

        if (scope === 'org') {
            logger.info('Updating organization settings');
            logger.info(`Organization settings body: ${JSON.stringify(typedBody, null, 2)}`);
            
            // Extract logo from settings if it exists
            const settingsObj = typedBody.settings || typedBody;
            const name = 'name' in settingsObj ? settingsObj.name : null;
            const logo = 'logo' in settingsObj ? settingsObj.logo : null;
            const orgSignature = 'orgSignature' in settingsObj ? settingsObj.orgSignature : false;
            const phone = 'phone' in settingsObj ? settingsObj.phone : null;
            const redirectUrl = 'redirectUrl' in settingsObj ? settingsObj.redirectUrl : null;
            const settingsWithoutLogo = { ...settingsObj };
            if ('logo' in settingsWithoutLogo) {
                delete (settingsWithoutLogo as any).logo;
            }
            
            // Update organization settings and logo separately
            await db.execute(
                'UPDATE organizations SET org_settings = ?, logo_data = ?, name = ?, org_signature = ?, phone = ?, redirect_url = ? WHERE id = ?',
                [JSON.stringify(settingsWithoutLogo), logo || null, name || null, orgSignature, 
                 orgSignature ? phone : null, orgSignature ? redirectUrl : null, user.organization_id]
            );

            // For basic tier, also update the admin agent's settings
            if (isBasicTier) {
                logger.info('Basic tier detected - syncing settings to admin agent');
                
                // Get the admin agent's ID
                const adminAgentRow = await db.fetchOne<{ id: number }>(
                    'SELECT id FROM users WHERE organization_id = ? AND is_admin = 1 AND is_agent = 1 LIMIT 1',
                    [user.organization_id]
                );

                if (adminAgentRow) {
                    // Update agent settings with organization settings and set inherit_org_settings to true
                    await db.execute(
                        `INSERT INTO agent_settings (agent_id, inherit_org_settings, settings)
                         VALUES (?, true, ?)
                         ON CONFLICT (agent_id) DO UPDATE
                         SET inherit_org_settings = true, settings = ?`,
                        [adminAgentRow.id, JSON.stringify(settingsWithoutLogo), JSON.stringify(settingsWithoutLogo)]
                    );
                }
            }
        } else if (scope === 'agent') {
            // For basic tier, don't allow direct agent settings updates
            if (isBasicTier) {
                return {
                    success: false,
                    error: 'Agent settings cannot be modified directly in basic tier - update organization settings instead'
                };
            }

            logger.info('Updating agent settings');
            logger.info(`Agent settings body: ${JSON.stringify(typedBody, null, 2)}`);

            try {
                await db.execute(
                    `INSERT INTO agent_settings (agent_id, inherit_org_settings, settings)
                     VALUES (?, ?, ?)
                     ON CONFLICT (agent_id) DO UPDATE
                     SET inherit_org_settings = ?, settings = ?`,
                    [
                        user.id,
                        typedBody.inheritOrgSettings,
                        JSON.stringify(typedBody.settings),
                        typedBody.inheritOrgSettings,
                        JSON.stringify(typedBody.settings)
                    ]
                );
                logger.info('Agent settings updated successfully');
            } catch (dbError) {
                logger.error(`Database execute error details: ${JSON.stringify(dbError, null, 2)}`);
                throw dbError;
            }
        }

        return {
            success: true,
            settings: typedBody.settings || typedBody
        };
    } catch (error) {
        logger.error(`Error updating settings: ${error}`);
        logger.error(`Error stack: ${(error as Error).stack}`);
        return {
            success: false,
            error: 'Failed to update settings'
        };
    }
  })

  // Update the GI recommendations endpoint to use Elysia style
  .get('/api/settings/gi-recommendations', async ({ cookie }) => {
    const user = await validateSession(cookie.session.toString());
    if (!user?.id) {
      return { success: false, error: 'No authenticated user' };
    }

    const db = new Database();

    try {
      const recommendations = await db.fetchAll(
        `SELECT state, carrier
         FROM guaranteed_issue_recommendations
         ORDER BY carrier, state`
      );

      // Transform the results to match the expected format
      const formattedRecommendations = recommendations.map((rec: { state: string; carrier: string }) => ({
        state: rec.state,
        carrier: rec.carrier,
        active: true,
        targetGI: true
      }));

      return formattedRecommendations;

    } catch (error) {
      logger.error(`Error fetching GI recommendations: ${error}`);
      return { success: false, error: 'Failed to fetch GI recommendations' };
    }
  })

  // Fetch the logo for the organization
  .get('/api/settings/logo', async ({ cookie }) => {
    const user = await validateSession(cookie.session.toString());
    if (!user?.id) {
      return { success: false, error: 'No authenticated user' };
    }

    const db = new Database();

    try {
      const logo = await db.fetchOne<{ logo_data: string | null }>(
        'SELECT logo_data FROM organizations WHERE id = ?',
        [user.organization_id]
      );

      if (!logo) {
        return { success: false, error: 'Organization not found' };
      }

      return {
        success: true,
        logo: logo.logo_data
      };

    } catch (error) {
      logger.error(`Error fetching organization logo: ${error}`);
      return { success: false, error: 'Failed to fetch organization logo' };
    }
  })
  .get('/api/settings/:orgId/logo', async ({ params }) => {
    const { orgId } = params;
    const db = new Database();

    try {
      const logo = await db.fetchOne<{ logo_data: string | null }>(
        'SELECT logo_data FROM organizations WHERE id = ?',
        [orgId]
      );

      if (!logo) {
        return { success: false, error: 'Organization logo not found' }; 
      }

      return {
        success: true,
        logo: logo.logo_data
      };
    } catch (error) {
      logger.error(`Error fetching organization logo: ${error}`);
      return { success: false, error: 'Failed to fetch organization logo' };
    }
  })
  

  // Update carriers endpoint to use Elysia style
  .get('/api/settings/carriers', async ({ cookie }) => {
    const user = await validateSession(cookie.session.toString());
    if (!user?.id) {
      return { success: false, error: 'No authenticated user' };
    }

    const db = new Database();

    try {
      const carriers = await db.fetchAll(
        `SELECT name
         FROM carriers
         ORDER BY name`
      );

      return carriers;

    } catch (error) {
      logger.error(`Error fetching carriers: ${error}`);
      return { success: false, error: 'Failed to fetch carriers' };
    }
  })

  .get('/api/settings/carriers-with-aliases', async ({ cookie }) => {
    const user = await validateSession(cookie.session.toString());
    if (!user?.id) {
      return { success: false, error: 'No authenticated user' };
    }

    const db = new Database();

    try {
      const carriers = await db.fetchAll(
        `SELECT name, aliases
         FROM carriers
         ORDER BY name`
      );

      return carriers.map((carrier: { name: string, aliases: string | null }) => ({
        name: carrier.name,
        aliases: carrier.aliases ? JSON.parse(carrier.aliases) : []
      }));

    } catch (error) {
      logger.error(`Error fetching carriers with aliases: ${error}`);
      return { success: false, error: 'Failed to fetch carriers with aliases' };
    }
  }); 