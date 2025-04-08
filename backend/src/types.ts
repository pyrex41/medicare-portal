export interface ContactCreate {
  first_name: string
  last_name: string
  email: string
  current_carrier: string
  plan_type: string
  effective_date: string
  birth_date: string
  tobacco_user: boolean
  gender: string
  state: string
  zip_code: string
  agent_id?: number | null
  contact_owner_id?: number | null
  phone_number: string
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
  is_admin: boolean;
  is_agent: boolean;
  is_active: boolean;
  first_name: string;
  last_name: string;
  phone: string;
  organization_name?: string;
} 