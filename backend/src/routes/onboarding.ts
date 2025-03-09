import { Elysia } from 'elysia'
import { logger } from '../logger'
import { Database } from '../database'
import { config } from '../config'

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

export function createOnboardingRoutes() {
  const dbInstance = new Database()

  return new Elysia()
    .post('/api/organizations/complete-onboarding', async ({ body, set }) => {
      const data = body as any; // Use any temporarily for conversion
      
      try {
        logger.info(`Starting complete onboarding process for ${data.user.email}`)
        
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
}

export default createOnboardingRoutes
