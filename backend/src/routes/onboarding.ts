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
    contactOutreachDelayYears: number
    outreachTypes: {
      birthday: boolean
      enrollmentAnniversary: boolean
      scheduleIncrease: boolean
      aep: boolean
    }
    failedUnderwritingOutreach: {
      enabled: boolean
      frequency: string
      timing: string
    }
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
      const data = body as OnboardingData
      
      try {
        logger.info(`Starting complete onboarding process for ${data.user.email}`)
        
        // Get database client
        const client = dbInstance.getClient()
        
        // Start a transaction
        await client.execute('BEGIN')
        
        try {
          // 1. Create organization
          const orgResult = await client.execute({
            sql: `INSERT INTO organizations (
              name, 
              website, 
              phone,
              primary_color,
              secondary_color,
              created_at
            ) VALUES (?, ?, ?, ?, ?, datetime('now'))`,
            args: [
              data.company.agencyName,
              data.company.website || '',
              data.company.phone || '',
              data.company.primaryColor || '#0A0F4F',
              data.company.secondaryColor || '#7B61FF'
            ]
          })
          
          const organizationId = Number(orgResult.lastInsertRowid)
          logger.info(`Created organization: ${organizationId} (${data.company.agencyName})`)
          
          // Generate a unique slug from the organization name
          const slug = data.company.agencyName
            .toLowerCase()
            .replace(/[^a-z0-9]/g, '-')
            .replace(/-+/g, '-')
            .replace(/^-|-$/g, '') + '-' + Math.floor(Math.random() * 1000)
            
          // Update organization with the slug
          await client.execute({
            sql: `UPDATE organizations SET slug = ? WHERE id = ?`,
            args: [slug, organizationId]
          })
          
          // Create organization settings with licensing and outreach data
          const orgSettings = {
            stateLicenses: data.licensing.stateLicenses,
            carrierContracts: data.licensing.carrierContracts,
            stateCarrierSettings: data.licensing.stateCarrierSettings,
            allowAgentSettings: true,
            emailSendBirthday: true,
            emailSendPolicyAnniversary: true,
            emailSendAep: true,
            smartSendEnabled: false,
            contactOutreachDelayYears: data.licensing.contactOutreachDelayYears,
            outreachTypes: data.licensing.outreachTypes,
            failedUnderwritingOutreach: data.licensing.failedUnderwritingOutreach,
            brandName: data.company.agencyName,
            primaryColor: data.company.primaryColor,
            secondaryColor: data.company.secondaryColor,
            logo: data.company.logo || null
          }
          
          // Update organization with settings
          await client.execute({
            sql: `UPDATE organizations SET org_settings = ? WHERE id = ?`,
            args: [JSON.stringify(orgSettings), organizationId]
          })
          
          // 2. Create admin user
          const userResult = await client.execute({
            sql: `INSERT INTO users (
              email,
              first_name,
              last_name,
              phone,
              is_admin,
              is_agent,
              organization_id,
              created_at,
              settings
            ) VALUES (?, ?, ?, ?, 1, 1, ?, datetime('now'), ?)`,
            args: [
              data.user.email,
              data.user.firstName,
              data.user.lastName,
              data.user.phone || '',
              organizationId,
              JSON.stringify({
                bookingLink: data.user.bookingLink || ''
              })
            ]
          })
          
          const userId = Number(userResult.lastInsertRowid)
          logger.info(`Created admin user: ${userId} (${data.user.email})`)
          
          // 3. Create subscription
          await client.execute({
            sql: `INSERT INTO subscriptions (
              organization_id,
              plan_type,
              billing_cycle,
              price,
              agent_limit,
              contact_limit,
              status,
              created_at
            ) VALUES (?, ?, ?, ?, ?, ?, 'active', datetime('now'))`,
            args: [
              organizationId,
              data.plan.type,
              data.plan.billingCycle,
              data.plan.price,
              data.plan.extraAgents + (data.plan.type === 'basic' ? 1 : 3), // Basic: 1 agent, Pro: 3 agents + extras
              data.plan.extraContacts + (data.plan.type === 'basic' ? 100 : 1000) // Basic: 100 contacts, Pro: 1000 + extras
            ]
          })
          
          logger.info(`Created subscription for org ${organizationId}: ${data.plan.type}`)
          
          // 4. Add state licenses
          for (const state of data.licensing.stateLicenses) {
            await client.execute({
              sql: `INSERT INTO user_licenses (user_id, state, created_at)
              VALUES (?, ?, datetime('now'))`,
              args: [userId, state]
            })
          }
          
          // 5. Add carrier contracts
          for (const carrier of data.licensing.carrierContracts) {
            await client.execute({
              sql: `INSERT INTO user_carriers (user_id, carrier, created_at)
              VALUES (?, ?, datetime('now'))`,
              args: [userId, carrier]
            })
          }
          
          // 6. Create organization-specific database
          // In a real implementation, this would create a separate database or schema
          logger.info(`Would create org-specific database for ${organizationId}`)
          
          // 7. If additional agents were added, process them
          if (data.agents && data.agents.length > 0) {
            logger.info(`Processing ${data.agents.length} additional agents`)
            
            // Process each agent - implementation would be added here
          }
          
          // Commit the transaction
          await client.execute('COMMIT')
          
          // Return success response
          return {
            success: true,
            message: "Organization created successfully",
            data: {
              organizationId,
              organizationSlug: slug
            }
          }
        } catch (error) {
          // Roll back the transaction on error
          await client.execute('ROLLBACK')
          throw error
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
