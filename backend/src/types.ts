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
  role: 'admin' | 'agent';
  is_active: boolean;
  last_login: string | null;
  created_at: string;
  organization_name?: string;
} 