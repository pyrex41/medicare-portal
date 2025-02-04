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