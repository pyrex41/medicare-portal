import { Elysia } from 'elysia';
import { validateSession } from '../services/auth';
import { Database } from '../database';
import { User } from '../types';
import { logger } from '../logger';

interface StateCarrierSetting {
  state: string;
  carrier: string;
  active: boolean;
  targetGI: boolean;
}

interface BaseSettings {
  stateLicenses: string[];
  carrierContracts: string[];
  stateCarrierSettings: StateCarrierSetting[];
  allowAgentSettings: boolean;
  emailSendBirthday: boolean;
  emailSendPolicyAnniversary: boolean;
  emailSendAep: boolean;
  smartSendEnabled: boolean;
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
  allowAgentSettings: true,
  emailSendBirthday: true,
  emailSendPolicyAnniversary: true,
  emailSendAep: true,
  smartSendEnabled: false
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
    
    const user = await validateSession(cookie.session);
    if (!user?.id) {
        return { success: false, error: 'No authenticated user' };
    }

    const db = new Database();

    try {
        // Get organization settings
        const orgSettingsRow = await db.fetchOne<{ org_settings: string | null }>(
            'SELECT org_settings FROM organizations WHERE id = ?',
            [user.organization_id]
        );
        
        logger.info(`Retrieved org settings row`);

        // Parse the JSON string into an object
        let orgSettings: BaseSettings;
        try {
            orgSettings = orgSettingsRow?.org_settings 
                ? { ...defaultSettings, ...JSON.parse(orgSettingsRow.org_settings) }
                : { ...defaultSettings };

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
            orgSettings,  // Now it's already an object, not a string
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
    logger.info(`PUT /api/settings/${scope} called`);
    logger.info(`Body type: ${typeof body}`);
    logger.info(`Body content: ${JSON.stringify(body, null, 2)}`);
    
    if (body.settings) {
        logger.info(`Settings type: ${typeof body.settings}`);
        logger.info(`Settings content: ${JSON.stringify(body.settings, null, 2)}`);
    }

    const user = await validateSession(cookie.session);
    logger.info(`User validation result: ${user ? JSON.stringify(user) : 'no user found'}`);
    
    if (!user?.id) {
        return { success: false, error: 'No authenticated user' };
    }

    const db = new Database();

    try {
        if (scope === 'org') {
            // Verify admin role for org settings
            if (!user.is_admin) {
                logger.warn(`User ${user.id} attempted to modify org settings but is not an admin`);
                return { success: false, error: 'Only admins can modify organization settings' };
            }

            // If states or carriers changed, regenerate the settings array
            const settings = body.settings || body;
            if (settings.stateCarrierSettings.length === 0) {
                settings.stateCarrierSettings = generateDefaultStateCarrierSettings(
                    settings.stateLicenses,
                    settings.carrierContracts
                );
            }

            let settingsToSave;
            try {
                settingsToSave = JSON.stringify(settings);
                logger.info(`Settings to save (type): ${typeof settingsToSave}`);
                logger.info(`Settings to save (value): ${settingsToSave}`);
                
                // Verify it's valid JSON
                JSON.parse(settingsToSave);
                logger.info('Settings validated as valid JSON');
            } catch (jsonError) {
                logger.error(`JSON processing error: ${jsonError}`);
                throw jsonError;
            }

            try {
                logger.info(`Executing DB update for org ${user.organization_id}`);
                logger.info(`First parameter type: ${typeof settingsToSave}`);
                logger.info(`First parameter value: ${settingsToSave}`);
                logger.info(`Second parameter type: ${typeof user.organization_id}`);
                logger.info(`Second parameter value: ${user.organization_id}`);

                await db.execute(
                    'UPDATE organizations SET org_settings = ? WHERE id = ?',
                    [settingsToSave, user.organization_id]
                );
                logger.info('Organization settings updated successfully');
            } catch (dbError) {
                logger.error(`Database execute error details: ${JSON.stringify(dbError, null, 2)}`);
                logger.error(`Database error name: ${dbError.name}`);
                logger.error(`Database error message: ${dbError.message}`);
                if (dbError.stack) {
                    logger.error(`Database error stack: ${dbError.stack}`);
                }
                throw dbError;
            }
        } else if (scope === 'agent') {
            logger.info('Updating agent settings');
            logger.info(`Agent settings body: ${JSON.stringify(body, null, 2)}`);

            try {
                await db.execute(`
                    INSERT INTO agent_settings (agent_id, inherit_org_settings, settings)
                    VALUES (?, ?, ?)
                    ON CONFLICT (agent_id) DO UPDATE
                    SET inherit_org_settings = ?, settings = ?`,
                    [
                        user.id,
                        body.inheritOrgSettings,
                        JSON.stringify(body.settings),
                        body.inheritOrgSettings,
                        JSON.stringify(body.settings)
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
            settings: body.settings || body
        };
    } catch (error) {
        logger.error(`Error updating settings: ${error}`);
        logger.error(`Error stack: ${error.stack}`);
        return {
            success: false,
            error: 'Failed to update settings'
        };
    }
  })

  // Update the GI recommendations endpoint to use Elysia style
  .get('/api/settings/gi-recommendations', async ({ cookie }) => {
    const user = await validateSession(cookie.session);
    if (!user?.id) {
      return { success: false, error: 'No authenticated user' };
    }

    const db = new Database();

    try {
      const recommendations = await db.fetchAll<{ state: string; carrier: string }>(
        `SELECT state, carrier
         FROM guaranteed_issue_recommendations
         ORDER BY carrier, state`
      );

      // Transform the results to match the expected format
      const formattedRecommendations = recommendations.map(rec => ({
        state: rec.state,
        carrier: rec.carrier,
        active: true,
        targetGI: true
      }));

      return formattedRecommendations;

    } catch (error) {
      logger.error('Error fetching GI recommendations:', error);
      return { success: false, error: 'Failed to fetch GI recommendations' };
    }
  })

  // Update carriers endpoint to use Elysia style
  .get('/api/settings/carriers', async ({ cookie }) => {
    const user = await validateSession(cookie.session);
    if (!user?.id) {
      return { success: false, error: 'No authenticated user' };
    }

    const db = new Database();

    try {
      const carriers = await db.fetchAll<{ name: string }>(
        `SELECT name
         FROM carriers
         ORDER BY name`
      );

      return carriers;

    } catch (error) {
      logger.error('Error fetching carriers:', error);
      return { success: false, error: 'Failed to fetch carriers' };
    }
  }); 