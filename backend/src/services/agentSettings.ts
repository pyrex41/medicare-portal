import { db } from '../database';
import { NotFoundError } from '../errors';
import { logger } from '../logger';

export interface StateCarrierSetting {
  active: boolean;
  targetGI: boolean;
}

export interface AgentSettings {
  id: number;
  agentId: number;
  stateLicenses: string[];
  carrierContracts: string[];
  stateCarrierSettings: {
    [state: string]: {
      [carrier: string]: StateCarrierSetting;
    };
  };
  emailSendBirthday: boolean;
  emailSendPolicyAnniversary: boolean;
  emailSendAep: boolean;
  smartSendEnabled: boolean;
}

export async function getAgentSettings(agentId: number): Promise<AgentSettings> {
  try {
    const result = await db.oneOrNone(
      'SELECT * FROM agent_settings WHERE agent_id = $1',
      [agentId]
    )

    if (!result) {
      // Return default settings instead of creating them
      return {
        id: 0,
        agentId: agentId,
        stateLicenses: [],
        carrierContracts: [],
        stateCarrierSettings: {},
        emailSendBirthday: false,
        emailSendPolicyAnniversary: false,
        emailSendAep: false,
        smartSendEnabled: false
      }
    }

    return {
      id: result.id,
      agentId: result.agent_id,
      stateLicenses: result.state_licenses || [],
      carrierContracts: result.carrier_contracts || [],
      stateCarrierSettings: result.state_carrier_settings || {},
      emailSendBirthday: result.email_send_birthday || false,
      emailSendPolicyAnniversary: result.email_send_policy_anniversary || false,
      emailSendAep: result.email_send_aep || false,
      smartSendEnabled: result.smart_send_enabled || false,
    }
  } catch (error) {
    logger.error(`Error fetching agent settings: ${error}`)
    // Return default settings on error
    return {
      id: 0,
      agentId: agentId,
      stateLicenses: [],
      carrierContracts: [],
      stateCarrierSettings: {},
      emailSendBirthday: false,
      emailSendPolicyAnniversary: false,
      emailSendAep: false,
      smartSendEnabled: false
    }
  }
}

async function createDefaultSettings(agentId: number): Promise<AgentSettings> {
  const result = await db.one(
    `INSERT INTO agent_settings 
     (agent_id, state_licenses, carrier_contracts, state_carrier_settings)
     VALUES ($1, $2, $3, $4)
     RETURNING *`,
    [agentId, [], [], {}]
  );

  return {
    id: result.id,
    agentId: result.agent_id,
    stateLicenses: result.state_licenses,
    carrierContracts: result.carrier_contracts,
    stateCarrierSettings: result.state_carrier_settings,
    emailSendBirthday: result.email_send_birthday,
    emailSendPolicyAnniversary: result.email_send_policy_anniversary,
    emailSendAep: result.email_send_aep,
    smartSendEnabled: result.smart_send_enabled,
  };
}

export async function updateAgentSettings(
  agentId: number,
  settings: Partial<AgentSettings>
): Promise<AgentSettings> {
  const result = await db.oneOrNone(
    `UPDATE agent_settings
     SET state_licenses = COALESCE($1, state_licenses),
         carrier_contracts = COALESCE($2, carrier_contracts),
         state_carrier_settings = COALESCE($3, state_carrier_settings),
         email_send_birthday = COALESCE($4, email_send_birthday),
         email_send_policy_anniversary = COALESCE($5, email_send_policy_anniversary),
         email_send_aep = COALESCE($6, email_send_aep),
         smart_send_enabled = COALESCE($7, smart_send_enabled)
     WHERE agent_id = $8
     RETURNING *`,
    [
      settings.stateLicenses,
      settings.carrierContracts,
      settings.stateCarrierSettings,
      settings.emailSendBirthday,
      settings.emailSendPolicyAnniversary,
      settings.emailSendAep,
      settings.smartSendEnabled,
      agentId,
    ]
  );

  if (!result) {
    throw new NotFoundError('Agent settings not found');
  }

  return {
    id: result.id,
    agentId: result.agent_id,
    stateLicenses: result.state_licenses,
    carrierContracts: result.carrier_contracts,
    stateCarrierSettings: result.state_carrier_settings,
    emailSendBirthday: result.email_send_birthday,
    emailSendPolicyAnniversary: result.email_send_policy_anniversary,
    emailSendAep: result.email_send_aep,
    smartSendEnabled: result.smart_send_enabled,
  };
} 