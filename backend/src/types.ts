import { Database } from './database';
import { cookie } from '@elysiajs/cookie';

export interface ContactCreate {
  first_name: string
  last_name: string
  email: string
  phone_number: string
  state: string
  contact_owner_id?: number | null
  current_carrier?: string | null
  effective_date: string
  birth_date: string
  tobacco_user: boolean
  gender: string
  zip_code: string
  plan_type?: string | null
  agent_id?: number | null
}

export interface ContactCreateTemp {
  first_name: string
  last_name: string
  email: string
  phone_number: string
  state: string
  effective_date: string
  birth_date: string
  tobacco_user: boolean
  gender: string
  zip_code: string
  contact_owner_id?: number | null
}

export interface AgentCreate {
  first_name: string
  last_name: string
  email: string
  phone: string
}

export interface User {
  id: number;
  email: string;
  organization_id: number;
  organizationId?: number;
  is_admin: boolean;
  is_agent: boolean;
  is_active: boolean;
  first_name: string;
  last_name: string;
  phone: string;
  organization_name?: string;
}

export interface BaseSettings {
  stateLicenses: string[];
  carrierContracts: string[];
  stateCarrierSettings: any[];
  allowAgentSettings: boolean;
  emailSendBirthday: boolean;
  emailSendPolicyAnniversary: boolean;
  emailSendAep: boolean;
  smartSendEnabled: boolean;
  brandName?: string;
  logo?: string | null;
  orgSignature?: boolean;
  phone?: string;
  redirectUrl?: string;
  signature?: string;
  forceOrgSenderDetails?: boolean;
  // New outreach settings
  contactOutreachDelayYears?: number;
  outreachTypes?: {
    birthday: boolean;
    enrollmentAnniversary: boolean;
    scheduleIncrease: boolean;
    aep: boolean;
  };
  failedUnderwritingOutreach?: {
    enabled: boolean;
    frequency: string;
    timing: string;
  };
}

export interface UserContext {
  store: {
    db: Database;
  };
  user: User;
  set: {
    status?: number;
    headers?: Record<string, string>;
  };
  cookie: ReturnType<typeof cookie>;
  query: Record<string, string | undefined>;
  body: unknown;
} 