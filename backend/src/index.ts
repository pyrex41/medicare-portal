import { Elysia, t } from 'elysia'
import { cors } from '@elysiajs/cors'
import { Database } from './database'
import { logger } from './logger'
import type { ContactCreate, AgentCreate } from './types'
import { readFileSync } from 'fs'
import { staticPlugin } from '@elysiajs/static'
import { parse as csvParse } from 'csv-parse/sync'
import { Readable } from 'stream'
import { Buffer } from 'buffer'
import { createAuthRoutes } from './routes/auth'
import { settingsRoutes } from './routes/settings'
import { organizationRoutes } from './routes/organizations'
import { createBrandRoutes } from './routes/brand'
import { quotesRoutes } from './routes/quotes'
import { createOnboardingRoutes } from './routes/onboarding'
import { createWaitlistRoutes } from './routes/waitlist'
import { createSignupRoutes, checkEmailHandler } from './routes/signup'
import { errorHandler } from './middleware/error'
import { getUserFromSession } from './services/auth'
import { join } from 'path'
import { existsSync } from 'fs'
import { EmailService } from './services/email'
import * as cron from 'node-cron'
import { eligibilityRoutes } from './routes/eligibility'
import { generateQuoteId } from './utils/quoteId'
import { createSelfServiceRoutes } from './routes/self-service'
import { scheduleRoutes } from './routes/schedule'
import { contactsRoutes } from './routes/contacts'
import * as fs from 'fs/promises'
import * as path from 'path'
import * as os from 'os'


// At the top of the file, add interface for ZIP data
export interface ZipInfo {
  state: string;
  counties: string[];
}

// Update ZIP_DATA declaration
export let ZIP_DATA: Record<string, ZipInfo> = {}
try {
  const dataPath = path.join(process.cwd(), 'data', 'zipData.json');
  logger.info(`Loading ZIP data from: ${dataPath}`);
  ZIP_DATA = JSON.parse(readFileSync(dataPath, 'utf-8'))
  logger.info('Successfully loaded ZIP data');
} catch (e) {
  logger.error(`Error loading ZIP data: ${e}`)
}

// Add with the other type imports
type NewAgentRequest = {
  firstName: string
  lastName: string
  email: string
  phone: string
  is_admin: boolean
  is_agent: boolean
  carriers: string[]
  stateLicenses: string[]
}

type AgentUpdate = {
  firstName: string
  lastName: string
  email: string
  phone: string
  is_admin: boolean
  is_agent: boolean
  carriers: string[]
  stateLicenses: string[]
}

interface DbRow {
  id: number;
  first_name: string;
  last_name: string;
  email: string;
  phone: string | null;
  is_admin: number;
  is_agent: number;
  settings: string | null;
}

// Add at the top with other interfaces
interface ContactRow {
  id: number;
  first_name: string;
  last_name: string;
  email: string;
  current_carrier: string;
  plan_type: string;
  effective_date: string;
  birth_date: string;
  tobacco_user: number;
  gender: string;
  state: string;
  zip_code: string;
  agent_id: number | null;
  last_emailed: string | null;
  phone_number: string;
}

interface CarrierRow {
  name: string;
  aliases: string | null;
}

// Add this helper function before startServer
function standardizePhoneNumber(phone: string): { isValid: boolean; standardized: string } {
  const digits = phone.replace(/\D/g, '').slice(0, 10);
  return {
    isValid: digits.length === 10,
    standardized: digits
  };
}

// Add this helper function near the other validation functions
function validateEmail(email: string): boolean {
  // RFC 5322 compliant email regex
  const emailRegex = /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/;
  return emailRegex.test(email.trim());
}

// Add this helper function near the other validation functions
function validateISODate(dateStr: string, allowFuture: boolean = false): { isValid: boolean; isoDate: string | null } {
  try {
    const trimmed = dateStr.trim();
    
    // Try to parse the date - will throw if invalid
    const date = new Date(trimmed);
    
    // Check if date is invalid
    if (isNaN(date.getTime())) {
      return { isValid: false, isoDate: null };
    }
    
    // Convert to ISO format (YYYY-MM-DD)
    const isoDate = date.toISOString().split('T')[0];
    
    // Verify the date is not in the future (unless allowed)
    if (!allowFuture && date > new Date()) {
      return { isValid: false, isoDate: null };
    }
    
    return { isValid: true, isoDate };
  } catch (e) {
    return { isValid: false, isoDate: null };
  }
}

// Add this helper function near the other validation functions
async function validateCarrier(carrier: string, db: Database): Promise<{ isValid: boolean; standardizedName: string; wasConverted: boolean }> {
  try {
    // Trim and standardize input
    const trimmedCarrier = carrier.trim();
    logger.info(`Validating carrier: "${trimmedCarrier}"`);
    
    // Create a new instance of the central database
    const centralDb = new Database();
    
    // Get all carriers with their aliases from the central database
    const result = await centralDb.execute<CarrierRow>(
      'SELECT name, aliases FROM carriers'
    );
    
    logger.info(`Found ${result.rows.length} carriers in database`);
    
    // Check each carrier and its aliases
    for (const row of result.rows) {
      logger.info(`Checking against carrier: "${row.name}", aliases: ${row.aliases || '[]'}`);
      
      // Check exact name match (case insensitive)
      if (row.name.toLowerCase() === trimmedCarrier.toLowerCase()) {
        logger.info(`Found exact match with carrier: ${row.name}`);
        return { isValid: true, standardizedName: row.name, wasConverted: false };
      }
      
      // Check aliases if they exist
      if (row.aliases) {
        const aliases = JSON.parse(row.aliases);
        logger.info(`Checking aliases for ${row.name}: ${JSON.stringify(aliases)}`);
        if (Array.isArray(aliases) && aliases.some(alias => alias.toLowerCase() === trimmedCarrier.toLowerCase())) {
          logger.info(`Found match in aliases for carrier: ${row.name}`);
          return { isValid: true, standardizedName: row.name, wasConverted: false };
        }
      }
    }
    
    // If no match found, keep the original carrier name but mark as converted
    logger.info(`No matching carrier found for: "${trimmedCarrier}", keeping original name`);
    return { isValid: true, standardizedName: trimmedCarrier, wasConverted: true };
  } catch (e) {
    logger.error(`Error validating carrier: ${e}`);
    return { isValid: true, standardizedName: carrier.trim(), wasConverted: true };
  }
}

// Interface for validation result
interface ValidationResult {
  isValid: boolean;
  error?: string;
  value?: any;
}

// Validation functions
const validateRow = async (row: any, rowNum: number, carrierMap: Map<string, string>): Promise<ValidationResult> => {
  try {
    logger.info(`Starting validation for row ${rowNum}:`)
    logger.info(`Row data: ${JSON.stringify(row)}`)

    // Required fields check with detailed logging
    const requiredFields = [
      'First Name', 'firstName',
      'Last Name', 'lastName', 
      'Email', 'email',
      'Current Carrier', 'currentCarrier',
      'Plan Type', 'planType',
      'Effective Date', 'effectiveDate',
      'Birth Date', 'birthDate',
      'Tobacco User', 'tobaccoUser',
      'Gender', 'gender',
      'ZIP Code', 'zipCode',
      'Phone Number', 'phoneNumber'
    ]

    logger.info(`Checking required fields: ${requiredFields.join(', ')}`)
    const missingValues = requiredFields.filter(field => {
      const value = row[field]?.trim()
      const isMissing = !value
      if (isMissing) {
        logger.warn(`Missing required field "${field}" in row ${rowNum}`)
      }
      return isMissing
    })

    // Group the missing fields by their base name (e.g. both "First Name" and "firstName" count as one missing field)
    const missingFieldGroups = new Set<string>()
    for (let i = 0; i < missingValues.length; i += 2) {
      if (missingValues[i] && missingValues[i+1]) {
        missingFieldGroups.add(missingValues[i])
      }
    }

    if (missingFieldGroups.size > 0) {
      logger.warn(`Row ${rowNum} missing required fields: ${Array.from(missingFieldGroups).join(', ')}`)
      return {
        isValid: false,
        error: `Missing values for: ${Array.from(missingFieldGroups).join(', ')}`
      }
    }

    // Email validation with logging
    const email = (row['Email'] || row['email'] || '').trim().toLowerCase()
    logger.info(`Validating email: "${email}"`)
    if (!validateEmail(email)) {
      logger.warn(`Row ${rowNum} has invalid email format: ${email}`)
      return {
        isValid: false,
        error: `Invalid email format: ${email}`
      }
    }

    // Phone validation with logging
    logger.info(`Validating phone number: "${row['Phone Number'] || row['phoneNumber']}"`)
    const phoneResult = standardizePhoneNumber(row['Phone Number'] || row['phoneNumber'])
    if (!phoneResult.isValid) {
      logger.warn(`Row ${rowNum} has invalid phone number: ${row['Phone Number'] || row['phoneNumber']}`)
      return {
        isValid: false,
        error: `Invalid phone number: ${row['Phone Number'] || row['phoneNumber']}. Must be exactly 10 digits.`
      }
    }

    // ZIP validation with logging
    const zipCode = (row['ZIP Code'] || row['zipCode'] || '').trim()
    logger.info(`Validating ZIP code: "${zipCode}"`)

    // First check if ZIP code exists and has the right format
    if (!zipCode) {
      logger.warn(`Row ${rowNum} is missing ZIP code`)
      return {
        isValid: false,
        error: `Missing ZIP code. This field is required for determining state coverage and eligibility.`
      }
    }

    // Check if ZIP code is in the correct format (5 digits)
    if (!/^\d{5}$/.test(zipCode)) {
      logger.warn(`Row ${rowNum} has invalid ZIP code format: ${zipCode}`)
      return {
        isValid: false,
        error: `Invalid ZIP code format: ${zipCode}. Must be exactly 5 digits.`
      }
    }

    // Check if ZIP code exists in our database
    const zipInfo = ZIP_DATA[zipCode]
    if (!zipInfo) {
      logger.warn(`Row ${rowNum} has invalid ZIP code: ${zipCode} (not found in database)`)
      return {
        isValid: false,
        error: `Invalid ZIP code: ${zipCode}. This ZIP code is not recognized in our database.`
      }
    }

    // If we get here, we have a valid ZIP code with state information
    logger.info(`Valid ZIP code ${zipCode} maps to state: ${zipInfo.state}`)

    // Gender validation with logging
    const gender = (row['Gender'] || row['gender'] || '').trim().toUpperCase()
    logger.info(`Validating gender: "${gender}"`)
    if (!['M', 'F'].includes(gender)) {
      logger.warn(`Row ${rowNum} has invalid gender: ${gender}`)
      return {
        isValid: false,
        error: `Invalid gender: ${gender}. Must be 'M' or 'F'`
      }
    }

    // Carrier validation with logging
    const carrierInput = (row['Current Carrier'] || row['currentCarrier'] || '').trim().toLowerCase()
    logger.info(`Validating carrier: "${carrierInput}"`)
    const standardizedCarrier = carrierMap.get(carrierInput) || carrierInput
    const wasCarrierConverted = !carrierMap.has(carrierInput)
    if (wasCarrierConverted) {
      logger.warn(`Row ${rowNum} has non-standard carrier: ${carrierInput} (will be kept as-is)`)
    }

    // Date validations with logging
    logger.info(`Validating effective date: "${row['Effective Date'] || row['effectiveDate']}"`)
    const effectiveDateResult = validateISODate(row['Effective Date'] || row['effectiveDate'], true)
    if (!effectiveDateResult.isValid) {
      logger.warn(`Row ${rowNum} has invalid effective date: ${row['Effective Date'] || row['effectiveDate']}`)
      return {
        isValid: false,
        error: `Invalid effective date format: ${row['Effective Date'] || row['effectiveDate']}. Please use YYYY-MM-DD or MM-DD-YYYY format.`
      }
    }

    logger.info(`Validating birth date: "${row['Birth Date'] || row['birthDate']}"`)
    const birthDateResult = validateISODate(row['Birth Date'] || row['birthDate'])
    if (!birthDateResult.isValid) {
      logger.warn(`Row ${rowNum} has invalid birth date: ${row['Birth Date'] || row['birthDate']}`)
      return {
        isValid: false,
        error: `Invalid birth date format: ${row['Birth Date'] || row['birthDate']}. Please use YYYY-MM-DD or MM-DD-YYYY format.`
      }
    }

    // If all validations pass, return processed data
    logger.info(`Row ${rowNum} passed all validations successfully`)
    return {
      isValid: true,
      value: {
        data: [
          (row['First Name'] || row['firstName'] || '').trim(),
          (row['Last Name'] || row['lastName'] || '').trim(),
          email,
          standardizedCarrier,
          (row['Plan Type'] || row['planType'] || '').trim(),
          effectiveDateResult.isoDate,
          birthDateResult.isoDate,
          ['yes', 'true', '1', 'y'].includes((row['Tobacco User'] || row['tobaccoUser'] || '').trim().toLowerCase()),
          gender,
          zipInfo.state,
          zipCode,
          phoneResult.standardized,
          null // agentId - will be set later
        ],
        carrierConverted: wasCarrierConverted ? {
          Row: rowNum,
          ...row,
          OriginalCarrier: row['Current Carrier'] || row['currentCarrier']
        } : null
      }
    }
  } catch (e) {
    // Log the full error details
    logger.error(`Unexpected error in row ${rowNum}:`)
    logger.error(`Error message: ${e instanceof Error ? e.message : String(e)}`)
    logger.error(`Row data: ${JSON.stringify(row)}`)
    if (e instanceof Error && e.stack) {
      logger.error(`Stack trace: ${e.stack}`)
    }
    return {
      isValid: false,
      error: `Unexpected error processing row ${rowNum}. Please check all fields are in the correct format. Error: ${e instanceof Error ? e.message : String(e)}`
    }
  }
};

const startServer = async () => {
  try {
    // Log environment information at startup
    logger.info(`Environment: NODE_ENV = "${process.env.NODE_ENV}"`)
    logger.info(`Current working directory: ${process.cwd()}`)
    logger.info(`Is production mode: ${process.env.NODE_ENV === 'production'}`)
    
    // Log available environment variables (without values for security)
    logger.info(`Available environment variables: ${Object.keys(process.env).join(', ')}`)
    
    // Try loading directly from process.env
    if (!process.env.TURSO_DATABASE_URL || !process.env.TURSO_AUTH_TOKEN) {
      logger.warn('Critical environment variables missing. Check Replit Secrets are properly set:')
      logger.warn('Required: TURSO_DATABASE_URL, TURSO_AUTH_TOKEN')
      logger.warn('Available env vars: ' + Object.keys(process.env).join(', '))
    } else {
      logger.info('Required environment variables found')
    }
    
    const db = new Database()
    logger.info('Database initialized successfully')

    const app = new Elysia()
      // Add error handler
      .use(errorHandler)
      // Add CORS middleware
      .use(cors({
        // In development, allow the Vite dev server origin
        origin: process.env.NODE_ENV === 'development' 
          ? 'http://localhost:5173'
          : false, // Disable CORS in production
        methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
        allowedHeaders: ['Content-Type', 'Cookie'],  // Add Cookie to allowed headers
        credentials: true,
        preflight: true
      }))
      // Add SPA route auth bypass handler
      .onRequest(({ request }) => {
        const url = new URL(request.url);
        const path = url.pathname;
        
        // Log all requests (combined from the other handler)
        const method = request.method;
        logger.info(`⮕ ${method} ${path}`);
        
        // Bypass auth for all SPA routes (non-API paths with no file extension)
        if ((!path.startsWith('/api/') && !path.includes('.')) || 
            path.startsWith('/compare/') ||
            path.startsWith('/quote/') ||
            path.startsWith('/eligibility') ||
            path.startsWith('/schedule')) {
          
          logger.info(`[Auth Bypass] Setting bypass header for SPA route: ${path}`);
          // Modify the request headers to include X-Bypass-Auth
          const newHeaders = new Headers(request.headers);
          newHeaders.set('X-Bypass-Auth', 'true');
          
          // Create a new request with the modified headers
          Object.defineProperty(request, 'headers', {
            value: newHeaders,
            writable: true
          });
        }
      })
      // Add explicit OPTIONS handler for preflight
      .options('/api/contacts/:id', ({ set }) => {
        set.headers = {
          'Access-Control-Allow-Origin': 'http://localhost:5173',
          'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, OPTIONS',
          'Access-Control-Allow-Headers': 'Content-Type, Authorization',
          'Access-Control-Allow-Credentials': 'true'
        }
        return new Response(null, { status: 204 })
      })
      // Log all responses
      .onResponse((context) => {
        const { request: { method }, path, set } = context
        logger.info(`⬅ ${method} ${path} ${set.status}`)
      })
      // Enhanced error handling
      .onError(({ code, error, request }: {
        code: string;
        error: Error;
        request: { url: string; method: string };
      }) => {
        const path = new URL(request.url).pathname
        const errorMessage = `❌ ${request.method} ${path} - ${error.message}`
        logger.error(errorMessage)

        return new Response(JSON.stringify({
          success: false,
          error: error.message
        }), { 
          status: code === 'NOT_FOUND' ? 404 : 500,
          headers: {
            'Content-Type': 'application/json'
          }
        })
      })
      // Add health check endpoint
      .get('/health', () => ({ status: 'OK' }))
      // Register public routes that shouldn't be auth-protected
      .get('/api/signup/check-email/:email', checkEmailHandler)
      // GET /api/contacts is now handled by the contactsRoutes module
      .get('/api/contacts/check-email/:email', async ({ params: { email }, request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString())
          
          const result = await orgDb.fetchOne(
            'SELECT 1 FROM contacts WHERE LOWER(TRIM(email)) = LOWER(TRIM(?))',
            [email]
          )

          return {
            exists: result !== null
          }
        } catch (e) {
          logger.error(`Error checking email existence: ${e}`)
          throw new Error(String(e))
        }
      })
      .get('/api/contacts/:id', async ({ params: { id }, request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          logger.info(`GET /api/contacts/${id} - Fetching contact for org ${user.organization_id}`)
          
          // Get org-specific database
          const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString())
          
          // Fetch the contact
          const result = await orgDb.fetchOne<ContactRow>(
            'SELECT * FROM contacts WHERE id = ?',
            [id]
          )

          if (!result) {
            logger.info(`GET /api/contacts/${id} - Contact not found`)
            return new Response('Contact not found', { status: 404 })
          }

          // Return the contact with mapped fields
          return {
            id: result.id,
            first_name: result.first_name,
            last_name: result.last_name,
            email: result.email,
            current_carrier: result.current_carrier,
            plan_type: result.plan_type,
            effective_date: result.effective_date,
            birth_date: result.birth_date,
            tobacco_user: Boolean(result.tobacco_user),
            gender: result.gender,
            state: result.state,
            zip_code: result.zip_code,
            agent_id: result.agent_id,
            last_emailed: result.last_emailed,
            phone_number: result.phone_number || ''
          }
        } catch (e) {
          logger.error(`Error fetching contact: ${e}`)
          throw new Error(String(e))
        }
      })
      .post('/api/contacts', async ({ body, request }: { body: ContactCreate, request: Request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          const contact = body
          logger.info(`Attempting to create contact for org ${user.organization_id}: ${contact.first_name} ${contact.last_name}`)
          
          // Get org-specific database
          const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString())

          // Check for existing email
          const existingContact = await orgDb.fetchOne(
            'SELECT 1 FROM contacts WHERE LOWER(TRIM(email)) = LOWER(TRIM(?))',
            [contact.email]
          )

          if (existingContact) {
            throw new Error('A contact with this email already exists')
          }
          
          // Map contact_owner_id to agent_id if it exists and agent_id is not set
          if (contact.contact_owner_id && !contact.agent_id) {
            contact.agent_id = contact.contact_owner_id
            logger.info(`Mapped contact_owner_id: ${contact.contact_owner_id} to agent_id`)
          }
          
          const query = `
            INSERT INTO contacts (
              first_name, last_name, email, current_carrier, plan_type,
              effective_date, birth_date, tobacco_user, gender,
              state, zip_code, agent_id, phone_number
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
          `
          
          const params = [
            contact.first_name,
            contact.last_name,
            contact.email,
            contact.current_carrier,
            contact.plan_type,
            contact.effective_date,
            contact.birth_date,
            contact.tobacco_user,
            contact.gender,
            contact.state,
            contact.zip_code,
            contact.agent_id || null,
            contact.phone_number || ''
          ]

          logger.info(`Executing query with params: ${JSON.stringify(params)}`)
          await orgDb.execute(query, params)

          // Fetch the newly created contact
          const result = await orgDb.fetchOne<ContactRow>(
            'SELECT * FROM contacts WHERE email = ? ORDER BY id DESC LIMIT 1',
            [contact.email]
          )

          if (!result) {
            throw new Error('Failed to fetch created contact')
          }

          // Match response format to schema
          return {
            id: result.id,
            first_name: result.first_name,
            last_name: result.last_name,
            email: result.email,
            current_carrier: result.current_carrier,
            plan_type: result.plan_type,
            effective_date: result.effective_date,
            birth_date: result.birth_date,
            tobacco_user: Boolean(result.tobacco_user),
            gender: result.gender,
            state: result.state,
            zip_code: result.zip_code,
            agent_id: result.agent_id,
            last_emailed: result.last_emailed,
            phone_number: result.phone_number || ''
          }
        } catch (e) {
          logger.error(`Error creating contact: ${e}`)
          throw new Error(String(e))
        }
      })
      .put('/api/contacts/:id', async ({ params: { id }, body, request }: { body: ContactCreate, request: Request }) => {
        try {
          // Get user and org info
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          // Get org-specific database
          const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString())

          const contact = body as ContactCreate
          logger.info(`PUT /api/contacts/${id} - Updating contact for org ${user.organization_id}`)

          // Get state from ZIP code
          const zipInfo = ZIP_DATA[contact.zip_code]
          if (!zipInfo) {
            throw new Error(`Invalid ZIP code: ${contact.zip_code}`)
          }

          // First update the contact
          const updateQuery = /* sql */ `
            UPDATE contacts SET 
              first_name = ?,
              last_name = ?,
              email = ?,
              current_carrier = ?,
              plan_type = ?,
              effective_date = ?,
              birth_date = ?,
              tobacco_user = ?,
              gender = ?,
              state = ?,
              zip_code = ?,
              phone_number = ?
            WHERE id = ?
          `

          const updateParams = [
            contact.first_name,
            contact.last_name,
            contact.email,
            contact.current_carrier,
            contact.plan_type,
            contact.effective_date,
            contact.birth_date,
            contact.tobacco_user,
            contact.gender,
            zipInfo.state, // Use state from ZIP code
            contact.zip_code,
            contact.phone_number || '',
            id
          ]

          // Execute the update
          await orgDb.execute(updateQuery, updateParams)

          // Then fetch the updated contact
          const result = await orgDb.fetchOne<ContactRow>(
            'SELECT * FROM contacts WHERE id = ?',
            [id]
          )

          if (!result) {
            throw new Error(`Contact ${id} not found after update`)
          }

          logger.info(`Successfully updated contact ${id} in org ${user.organization_id}`)

          // Return the updated contact
          return {
            id: result.id,
            first_name: result.first_name,
            last_name: result.last_name,
            email: result.email,
            current_carrier: result.current_carrier,
            plan_type: result.plan_type,
            effective_date: result.effective_date,
            birth_date: result.birth_date,
            tobacco_user: Boolean(result.tobacco_user),
            gender: result.gender,
            state: result.state,
            zip_code: result.zip_code,
            agent_id: result.agent_id,
            last_emailed: result.last_emailed,
            phone_number: result.phone_number
          }
        } catch (e) {
          logger.error(`Error updating contact: ${e}`)
          throw new Error(String(e))
        }
      })
      // Add DELETE endpoint for contacts
      .delete('/api/contacts', async ({ request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          // Parse contact IDs from the request
          const url = new URL(request.url)
          const ids = url.searchParams.get('ids')
          if (!ids) {
            throw new Error('No contact IDs provided')
          }

          const contactIds = ids.split(',').map(id => parseInt(id.trim(), 10))
          
          logger.info(`DELETE /api/contacts - Attempting to delete ${contactIds.length} contacts for org ${user.organization_id}`)
          
          // Get org-specific database
          const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString())

          // Create placeholders for SQL IN clause
          const placeholders = contactIds.map(() => '?').join(',')
          
          const query = `
            DELETE FROM contacts 
            WHERE id IN (${placeholders})
            RETURNING id
          `

          const result = await orgDb.execute(query, contactIds)
          const deletedIds = result.rows?.map(row => row.id) || []

          logger.info(`DELETE /api/contacts - Successfully deleted ${deletedIds.length} contacts from org ${user.organization_id}`)

          return {
            success: true,
            deleted_ids: deletedIds,
            message: `Successfully deleted ${deletedIds.length} contacts`
          }
        } catch (e) {
          logger.error(`Error deleting contacts: ${e}`)
          throw new Error(String(e))
        }
      })
      // Add endpoint for reassigning contacts to a different agent
      .put('/api/contacts/reassign', async ({ request, body }: { request: Request, body: { contact_ids: number[], agent_id: number | null } }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          const { contact_ids, agent_id } = body
          if (!contact_ids || !Array.isArray(contact_ids) || contact_ids.length === 0) {
            throw new Error('Invalid or empty contact_ids array')
          }

          logger.info(`PUT /api/contacts/reassign - Reassigning ${contact_ids.length} contacts to agent ${agent_id} for org ${user.organization_id}`)
          
          // Get org-specific database
          const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString())

          // Create placeholders for SQL IN clause
          const placeholders = contact_ids.map(() => '?').join(',')
          
          const query = `
            UPDATE contacts 
            SET agent_id = ?
            WHERE id IN (${placeholders})
            RETURNING id
          `

          const params = [agent_id, ...contact_ids]
          const result = await orgDb.execute(query, params)
          const updatedIds = result.rows?.map(row => row.id) || []

          logger.info(`PUT /api/contacts/reassign - Successfully reassigned ${updatedIds.length} contacts to agent ${agent_id}`)

            return {
            success: true,
            updated_ids: updatedIds,
            message: `Successfully reassigned ${updatedIds.length} contacts to agent ${agent_id}`
          }
        } catch (e) {
          logger.error(`Error reassigning contacts: ${e}`)
          throw new Error(String(e))
        }
      })
      // Add file upload endpoint
      .post('/api/contacts/upload', async ({ request, body, set }: { request: Request, body: { file: File, overwrite_duplicates?: boolean | string, duplicateStrategy?: string, agent_id?: string }, set: any }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          // Extract file and overwrite flag from form data
          const formData = body
          const file = formData.file
          
          // Support both naming conventions - overwrite_duplicates (old) and duplicateStrategy (new)
          let overwriteDuplicates = false
          if (formData.overwrite_duplicates !== undefined) {
            // Convert string 'false'/'true' to boolean
            overwriteDuplicates = formData.overwrite_duplicates === 'true' || formData.overwrite_duplicates === true
          } else if (formData.duplicateStrategy !== undefined) {
            // Support the new 'duplicateStrategy' parameter
            overwriteDuplicates = formData.duplicateStrategy === 'overwrite'
          }

          logger.info(`Processing ${file.name} with overwriteDuplicates=${overwriteDuplicates}`)

          // Save file to temp directory
          const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'import-'));
          const tempFilePath = path.join(tempDir, 'contacts.csv');
          await fs.writeFile(tempFilePath, Buffer.from(await file.arrayBuffer()));

          logger.info(`CSV file saved to ${tempFilePath}, starting import`);

          // Start bulk import process
          const importPromise = Database.bulkImportContacts(
            user.organization_id.toString(),
            tempFilePath,
            overwriteDuplicates
          );

          importPromise
            .then(() => fs.rm(tempDir, { recursive: true, force: true }))
            .catch((error: Error) => {
              logger.error(`Background import failed: ${error}`);
              fs.rm(tempDir, { recursive: true, force: true }).catch(() => {});
            });

          return {
            success: true,
            message: 'Started import of contacts',
            errors: []
          };
        } catch (error) {
          const err = error as Error;
          logger.error(`Error processing CSV upload: ${err}`);
          throw err;
        }
      })
      // Add error handler
      .use(errorHandler)
      // Add explicit debug log for auth routes
      .use(app => {
        logger.info('Registering auth routes...')
        return app.use(createAuthRoutes())
      })
      // Add signup routes
      .use(app => {
        logger.info('Registering signup routes...')
        return app.use(createSignupRoutes())
      })
      // Add settings routes
      .use(settingsRoutes)
      // Add organization routes
      .use(organizationRoutes)
      // Add brand routes
      .use(createBrandRoutes())
      // Add quotes routes
      .use(quotesRoutes)
      // Add onboarding routes
      .use(createOnboardingRoutes())
      // Add eligibility routes
      .use(eligibilityRoutes)
      // Add self-service routes
      .use(createSelfServiceRoutes())
      // Add schedule routes
      .use(scheduleRoutes)
      // Add contacts routes
      .use(contactsRoutes)
      // Add waitlist routes
      .use(createWaitlistRoutes())  // Waitlist routes use their own database connection
      // Serve backend static files from public directory
      .use(staticPlugin({
        assets: './public', 
        prefix: '/'
      }))
      // In production, serve the frontend static files
      .use(process.env.NODE_ENV === 'production' 
        ? async (app) => {
            logger.info(`[Static Files] Running in production mode: NODE_ENV = "${process.env.NODE_ENV}"`)
            const distPath = join(process.cwd(), '../dist');
            logger.info(`[Static Files] Serving from: ${distPath} (exists: ${existsSync(distPath)})`);
            
            // List directory contents for debugging
            try {
              const fs = require('fs');
              const distContents = fs.readdirSync(distPath);
              logger.info(`[Static Files] dist directory contents: ${JSON.stringify(distContents)}`);
              
              // Check if index.html exists
              const indexPath = join(distPath, 'index.html');
              logger.info(`[Static Files] index.html path: ${indexPath} (exists: ${existsSync(indexPath)})`);
              
              // If it exists, log its contents for debugging
              if (existsSync(indexPath)) {
                const indexContent = fs.readFileSync(indexPath, 'utf-8');
                logger.info(`[Static Files] index.html first 100 chars: ${indexContent.substring(0, 100)}...`);
              }
            } catch (error) {
              logger.error(`[Static Files] Error inspecting dist directory: ${error}`);
            }
            
            // Try different approach for static files
            app.get('/*', async ({ request, set }) => {
              const url = new URL(request.url);
              const path = url.pathname;

              // Log the browser type from the user agent
              const userAgent = request.headers.get('user-agent') || 'unknown';
             
              logger.info(`[Static Route Handler] Handling request for: ${path}`);
              
              // Skip API routes
              if (path.startsWith('/api/')) {
                logger.info(`[Static Route Handler] Skipping API route: ${path}`);
                return;
              }
              
              // Try to serve the file directly from dist directory
              try {
                const filePath = join(distPath, path === '/' ? 'index.html' : path.slice(1));
                logger.info(`[Static Route Handler] Trying file path: ${filePath} (exists: ${existsSync(filePath)})`);
                
                if (existsSync(filePath)) {
                  logger.info(`[Static Route Handler] File exists, serving: ${filePath}`);
                  const ext = filePath.substring(filePath.lastIndexOf('.') + 1);
                  const mimeTypes: Record<string, string> = {
                    'html': 'text/html',
                    'js': 'application/javascript',
                    'css': 'text/css',
                    'json': 'application/json',
                    'png': 'image/png',
                    'jpg': 'image/jpeg',
                    'svg': 'image/svg+xml',
                    'ico': 'image/x-icon',
                    'csv': 'text/csv',
                  };
                  
                  return new Response(Bun.file(filePath), {
                    headers: { 'Content-Type': mimeTypes[ext] || 'application/octet-stream' }
                  });
                } else if (path.match(/^\/compare\/[^\/]+$/)) {
                  // Special case for Compare route with path parameters
                  logger.info(`[Static Route Handler] Serving index.html for Compare route with path parameter: ${path}`);
                  logger.info(`[Static Route Handler] About to create response with explicit 200 status code`);
                  
                  // Create response with explicit status
                  set.status = 200
                  const response = new Response(Bun.file(join(distPath, 'index.html')), {
                    headers: { 
                      'Content-Type': 'text/html',
                      'X-Bypass-Auth': 'true' // Add a header to indicate this should bypass auth
                    },
                    status: 200 // Explicitly set 200 status code
                  });
                  
                  logger.info(`[Static Route Handler] Created response with status: ${response.status}`);
                  return response;
                } else if (path.match(/^\/quote\/[^\/]+$/)) {
                  // Special case for Quote route with path parameters
                  logger.info(`[Static Route Handler] Serving index.html for Quote route with path parameter: ${path}`);
                  logger.info(`[Static Route Handler] About to create response with explicit 200 status code`);
                  
                  // Create response with explicit status
                  set.status = 200
                  const response = new Response(Bun.file(join(distPath, 'index.html')), {
                    headers: { 
                      'Content-Type': 'text/html',
                      'X-Bypass-Auth': 'true' // Add a header to indicate this should bypass auth
                    },
                    status: 200 // Explicitly set 200 status code
                  });
                  
                  logger.info(`[Static Route Handler] Created response with status: ${response.status}`);
                  return response;
                } else if (path.match(/^\/eligibility(\?.*)?$/)) {
                  // Special case for Eligibility route with query parameters
                  logger.info(`[Static Route Handler] Serving index.html for Eligibility route: ${path}`);
                  logger.info(`[Static Route Handler] About to create response with explicit 200 status code`);
                  
                  // Create response with explicit status
                  set.status = 200
                  const response = new Response(Bun.file(join(distPath, 'index.html')), {
                    headers: { 
                      'Content-Type': 'text/html',
                      'X-Bypass-Auth': 'true' // Add a header to indicate this should bypass auth
                    },
                    status: 200 // Explicitly set 200 status code
                  });
                  
                  logger.info(`[Static Route Handler] Created response with status: ${response.status}`);
                  return response;
                } else if (path.match(/^\/schedule(\?.*)?$/)) {
                  // Special case for Schedule route with query parameters
                  logger.info(`[Static Route Handler] Serving index.html for Schedule route: ${path}`);
                  logger.info(`[Static Route Handler] About to create response with explicit 200 status code`);
                  
                  // Create response with explicit status
                  set.status = 200
                  const response = new Response(Bun.file(join(distPath, 'index.html')), {
                    headers: { 
                      'Content-Type': 'text/html',
                      'X-Bypass-Auth': 'true' // Add a header to indicate this should bypass auth
                    },
                    status: 200 // Explicitly set 200 status code
                  });
                  
                  logger.info(`[Static Route Handler] Created response with status: ${response.status}`);
                  return response;
                } else if (path !== '/' && !path.includes('.')) {
                  // This is likely a SPA route, serve index.html
                  logger.info(`[Static Route Handler] Likely SPA route, serving index.html for: ${path}`);
                  set.status = 200
                  return new Response(Bun.file(join(distPath, 'index.html')), {
                    headers: { 'Content-Type': 'text/html' },
                    status: 200 // Explicitly set 200 status code
                  });
                } else if (path === '/') {
                  // Explicitly handle root path
                  logger.info(`[Static Route Handler] Handling root path, serving index.html`);
                  const indexPath = join(distPath, 'index.html');
                  
                  if (existsSync(indexPath)) {
                    logger.info(`[Static Route Handler] Root: index.html exists, serving it`);
                    set.status = 200
                    return new Response(Bun.file(indexPath), {
                      headers: { 'Content-Type': 'text/html' },
                      status: 200 // Explicitly set 200 status code
                    });
                  } else {
                    logger.error(`[Static Route Handler] Root: index.html doesn't exist at ${indexPath}`);
                    return new Response('index.html not found', { status: 404 });
                  }
                }
                
                // If we get here, the file doesn't exist
                logger.warn(`[Static Route Handler] No matching file found for: ${path}`);
                return new Response('Not found', { status: 404 });
              } catch (error) {
                logger.error(`[Static Route Handler] Error serving file for ${path}: ${error}`);
                return new Response(`Server error: ${error}`, { status: 500 });
              }
            });

            return app;
          }
        : (app) => app
      )
      // Add this endpoint within the app definition
      .post('/api/agents', async ({ body, request, set }) => {
        try {
          // Get current user from session to determine their org
          const currentUser = await getUserFromSession(request)
          if (!currentUser) {
            set.status = 401
            return {
              success: false,
              error: 'You must be logged in to perform this action'
            }
          }

          // Check if user is an admin
          if (!currentUser.is_admin) {
            set.status = 403
            return {
              success: false,
              error: 'Only administrators can create new agents'
            }
          }

          const newAgent = body as NewAgentRequest
          logger.info(`Creating new agent: ${newAgent.email} (org: ${currentUser.organization_id})`)

          // Ensure that the new user has at least one role
          if (!newAgent.is_admin && !newAgent.is_agent) {
            logger.warn(`Agent created without any roles. Defaulting to is_agent=true for: ${newAgent.email}`)
            newAgent.is_agent = true
          }

          // Get the libSQL client
          const client = db.getClient()
          
          // Check if the organization has reached its agent limit
          const orgLimitResult = await client.execute({
            sql: `
              SELECT 
                o.agent_limit, 
                COUNT(u.id) as current_agent_count
              FROM 
                organizations o
              LEFT JOIN 
                users u ON o.id = u.organization_id AND (u.is_agent = 1 OR u.is_admin = 1) AND u.is_active = 1
              WHERE 
                o.id = ?
              GROUP BY 
                o.id
            `,
            args: [currentUser.organization_id]
          })
          
          if (orgLimitResult.rows.length > 0) {
            const { agent_limit, current_agent_count } = orgLimitResult.rows[0]
            
            if (Number(current_agent_count) >= Number(agent_limit)) {
              logger.warn(`Organization ${currentUser.organization_id} has reached its agent limit (${agent_limit}). Cannot create new agent.`)
              set.status = 403
              return {
                success: false,
                error: `You have reached your plan's agent limit (${agent_limit}). Please upgrade your plan to add more agents.`
              }
            }
            
            logger.info(`Organization has ${current_agent_count}/${agent_limit} agents (before adding new agent)`)
          }
          
          // Get organization settings to inherit carriers and state licenses
          const orgSettingsResult = await client.execute({
            sql: `SELECT org_settings FROM organizations WHERE id = ?`,
            args: [currentUser.organization_id]
          })
          
          let orgSettings = {
            stateLicenses: [],
            carrierContracts: [],
            stateCarrierSettings: []
          }
          
          if (orgSettingsResult.rows.length > 0 && orgSettingsResult.rows[0].org_settings) {
            try {
              const parsedSettings = JSON.parse(orgSettingsResult.rows[0].org_settings as string)
              orgSettings = {
                stateLicenses: parsedSettings.stateLicenses || [],
                carrierContracts: parsedSettings.carrierContracts || [],
                stateCarrierSettings: parsedSettings.stateCarrierSettings || []
              }
              logger.info(`Inherited org settings: ${orgSettings.carrierContracts.length} carriers, ${orgSettings.stateLicenses.length} state licenses`)
            } catch (e) {
              logger.error(`Error parsing org settings: ${e}`)
            }
          }

          // First create the user
          const userResult = await client.execute({
            sql: `INSERT INTO users (
              email, 
              first_name, 
              last_name, 
              phone,
              organization_id,
              is_admin,
              is_agent,
              is_active
            ) VALUES (?, ?, ?, ?, ?, ?, ?, 1)
            RETURNING id`,
            args: [
              newAgent.email,
              newAgent.firstName,
              newAgent.lastName,
              newAgent.phone,
              currentUser.organization_id,
              newAgent.is_admin ? 1 : 0,
              newAgent.is_agent ? 1 : 0
            ]
          })

          const userId = userResult.rows[0].id
          logger.info(`Created new agent with ID: ${userId}`)

          // Then create agent settings - automatically inherit from organization
          await client.execute({
            sql: `INSERT INTO agent_settings (
              agent_id,
              settings
            ) VALUES (?, ?)`,
            args: [
              userId,
              JSON.stringify({
                stateLicenses: orgSettings.stateLicenses,
                carrierContracts: orgSettings.carrierContracts,
                stateCarrierSettings: orgSettings.stateCarrierSettings,
                emailSendBirthday: false,
                emailSendPolicyAnniversary: false,
                emailSendAep: false,
                smartSendEnabled: false
              })
            ]
          })

          logger.info(`Initialized settings for agent: ${userId} with inherited org settings`)

          return {
            success: true,
            message: 'Agent created successfully',
            id: userId
          }

        } catch (e) {
          logger.error(`Error creating agent: ${e}`)
          set.status = 500
          return {
            success: false,
            error: String(e)
          }
        }
      })
      // Add an alias endpoint for POST /api/agents/create to match frontend expectations
      .post('/api/agents/create', async ({ body, request, set }) => {
        try {
          // Log the request to the alias endpoint
          logger.info(`POST /api/agents/create - Using the same implementation as /api/agents`)
          
          // Get current user from session to determine their org
          const currentUser = await getUserFromSession(request)
          if (!currentUser) {
            set.status = 401
            return {
              success: false,
              error: 'You must be logged in to perform this action'
            }
          }

          // Check if user is an admin
          if (!currentUser.is_admin) {
            set.status = 403
            return {
              success: false,
              error: 'Only administrators can create new agents'
            }
          }

          const newAgent = body as NewAgentRequest
          logger.info(`Creating new agent via /api/agents/create: ${newAgent.email} (org: ${currentUser.organization_id})`)
          
          // Ensure that the new user has at least one role
          if (!newAgent.is_admin && !newAgent.is_agent) {
            logger.warn(`Agent created without any roles. Defaulting to is_agent=true for: ${newAgent.email}`)
            newAgent.is_agent = true
          }

          // Get the libSQL client
          const client = db.getClient()
          
          // Get organization settings to inherit carriers and state licenses
          const orgSettingsResult = await client.execute({
            sql: `SELECT org_settings FROM organizations WHERE id = ?`,
            args: [currentUser.organization_id]
          })
          
          let orgSettings = {
            stateLicenses: [],
            carrierContracts: [],
            stateCarrierSettings: []
          }
          
          if (orgSettingsResult.rows.length > 0 && orgSettingsResult.rows[0].org_settings) {
            try {
              const parsedSettings = JSON.parse(orgSettingsResult.rows[0].org_settings as string)
              orgSettings = {
                stateLicenses: parsedSettings.stateLicenses || [],
                carrierContracts: parsedSettings.carrierContracts || [],
                stateCarrierSettings: parsedSettings.stateCarrierSettings || []
              }
              logger.info(`Inherited org settings: ${orgSettings.carrierContracts.length} carriers, ${orgSettings.stateLicenses.length} state licenses`)
            } catch (e) {
              logger.error(`Error parsing org settings: ${e}`)
            }
          }

          // First create the user
          const userResult = await client.execute({
            sql: `INSERT INTO users (
              email, 
              first_name, 
              last_name, 
              phone,
              organization_id,
              is_admin,
              is_agent,
              is_active
            ) VALUES (?, ?, ?, ?, ?, ?, ?, 1)
            RETURNING id`,
            args: [
              newAgent.email,
              newAgent.firstName,
              newAgent.lastName,
              newAgent.phone,
              currentUser.organization_id,
              newAgent.is_admin ? 1 : 0,
              newAgent.is_agent ? 1 : 0
            ]
          })

          const userId = userResult.rows[0].id
          logger.info(`Created new agent with ID: ${userId}`)

          // Then create agent settings - automatically inherit from organization
          await client.execute({
            sql: `INSERT INTO agent_settings (
              agent_id,
              settings
            ) VALUES (?, ?)`,
            args: [
              userId,
              JSON.stringify({
                stateLicenses: orgSettings.stateLicenses,
                carrierContracts: orgSettings.carrierContracts,
                stateCarrierSettings: orgSettings.stateCarrierSettings,
                emailSendBirthday: false,
                emailSendPolicyAnniversary: false,
                emailSendAep: false,
                smartSendEnabled: false
              })
            ]
          })

          logger.info(`Initialized settings for agent: ${userId} with inherited org settings`)

          return {
            success: true,
            message: 'Agent created successfully',
            id: userId
          }

        } catch (e) {
          logger.error(`Error creating agent via /api/agents/create: ${e}`)
          set.status = 500
          return {
            success: false,
            error: String(e)
          }
        }
      })
      // Add this GET endpoint within the app definition, near the POST /api/agents endpoint
      .get('/api/agents', async ({ request, set }) => {
        try {
          const currentUser = await getUserFromSession(request)
          if (!currentUser) {
            set.status = 401
            return {
              success: false,
              error: 'You must be logged in to perform this action'
            }
          }

          // Remove admin check - allow any authenticated user to fetch agents for their organization
          // All users should be able to see the agent list for assignment purposes
          logger.info(`GET /api/agents - Fetching agents for org ${currentUser.organization_id}`)

          // Get the libSQL client
          const client = db.getClient()

          // Fetch all agents (users) from the organization along with their settings
          const result = await client.execute({
            sql: `
              SELECT 
                u.id,
                u.first_name,
                u.last_name,
                u.email,
                u.phone,
                u.is_admin,
                u.is_agent,
                a.settings
              FROM users u
              LEFT JOIN agent_settings a ON u.id = a.agent_id
              WHERE u.organization_id = ?
              AND u.is_active = 1
              ORDER BY u.first_name, u.last_name
            `,
            args: [currentUser.organization_id]
          })

          logger.info(`GET /api/agents - Found ${result.rows.length} agents`)

          // Map the database results to the expected format with camelCase field names
          const agents = result.rows.map((row: any) => {
            const settings = row.settings ? JSON.parse(row.settings) : {
              stateLicenses: [],
              carrierContracts: [],
              stateCarrierSettings: []
            }

            return {
              id: String(row.id),
              firstName: row.first_name,
              lastName: row.last_name,
              email: row.email,
              phone: row.phone || '',
              isAdmin: Boolean(row.is_admin),
              isAgent: Boolean(row.is_agent),
              carriers: settings.carrierContracts || [],
              stateLicenses: settings.stateLicenses || []
            }
          })

          logger.info(`GET /api/agents - Returning ${agents.length} agents`)
          return agents

        } catch (e) {
          logger.error(`Error fetching agents: ${e}`)
          set.status = 500
          return {
            success: false,
            error: String(e)
          }
        }
      })
      .post('/api/agents/set_default_agent', async ({ body, request, set }) => {
        try {
          const currentUser = await getUserFromSession(request)
          if (!currentUser) {
            set.status = 401
            return {
              success: false,
              error: 'You must be logged in to perform this action'
            }
          }

          const { agentId } = body  
          logger.info(`Setting default agent to ${agentId} for user ${currentUser.id}`)

          // Get the libSQL client
          const client = db.getClient()

          // Update the default agent for the organization
          await client.execute({
            sql: `UPDATE organizations SET default_agent_id = ? WHERE id = ?`,
            args: [agentId, currentUser.organization_id]
          })

          logger.info(`Default agent set to ${agentId} for organization ${currentUser.organization_id}`)
          
          set.status = 200
          return {
            success: true,
            message: 'Default agent set successfully'
          }
        } catch (e) {
          logger.error(`Error setting default agent: ${e}`) 
          set.status = 500
          return {
            success: false,
            error: String(e)
          }
        }
      })
      // Update PUT endpoint for updating agent details - moved here to be with other agent endpoints
      .put('/api/agents/:id', async ({ params, body, request, set }: {
        params: { id: string },
        body: AgentUpdate,
        request: Request,
        set: any
      }) => {
        console.log('DEBUG: PUT handler hit', { params, path: request.url })
        logger.info(`Starting update for agent ${params.id}`)
        logger.info(`Request body: ${JSON.stringify(body, null, 2)}`)
        
        try {
          const currentUser = await getUserFromSession(request)
          if (!currentUser) {
            logger.error('Authentication failed: No user in session')
            set.status = 401
            return {
              success: false,
              error: 'You must be logged in to perform this action'
            }
          }

          // Allow users to update their own details or admins to update any agent
          if (!currentUser.is_admin && currentUser.id.toString() !== params.id) {
            logger.error(`Authorization failed: User ${currentUser.id} is not an admin and trying to update another user`)
            set.status = 403
            return {
              success: false,
              error: 'Only administrators can update other agents'
            }
          }
          
          // Security protection: NEVER allow ANY user to remove their own admin status,
          // even if they are an admin themselves
          if (currentUser.id.toString() === params.id) {
            // If this is a self-update and user is trying to change admin status
            if (body.is_admin !== Boolean(currentUser.is_admin)) {
              // If they're trying to REMOVE admin status
              if (Boolean(currentUser.is_admin) && !body.is_admin) {
                logger.warn(`Security protection: Admin user ${currentUser.id} attempted to remove their own admin status`)
                // Prevent admin from removing their own admin status - keep it as is
                body.is_admin = true;
              }
              // Note: We still allow non-admins to be promoted by an admin
            }
          }

          const agent = body
          logger.info(`Updating agent ${params.id} - Name: ${agent.firstName} ${agent.lastName}, Phone: ${agent.phone}`)

          // Get the libSQL client
          const client = db.getClient()

          // Determine if this is a self-update by a non-admin
          const isSelfUpdate = currentUser.id.toString() === params.id && !currentUser.is_admin
          
          // Create dynamic SQL that excludes is_admin for self-updates
          let sql, args
          if (isSelfUpdate) {
            // For self-updates, exclude is_admin from the update
            sql = `UPDATE users 
                  SET first_name = ?, 
                      last_name = ?, 
                      email = ?, 
                      phone = ?,
                      is_agent = ?
                  WHERE id = ? AND organization_id = ?
                  RETURNING *`
            args = [
              agent.firstName,
              agent.lastName,
              agent.email,
              agent.phone,
              agent.is_agent ? 1 : 0,
              params.id,
              currentUser.organization_id
            ]
            logger.info(`Self-update detected: excluding admin status from update for user ${params.id}`)
          } else {
            // For admin updates or other users, include all fields
            sql = `UPDATE users 
                  SET first_name = ?, 
                      last_name = ?, 
                      email = ?, 
                      phone = ?,
                      is_admin = ?,
                      is_agent = ?
                  WHERE id = ? AND organization_id = ?
                  RETURNING *`
            args = [
              agent.firstName,
              agent.lastName,
              agent.email,
              agent.phone,
              agent.is_admin ? 1 : 0,
              agent.is_agent ? 1 : 0,
              params.id,
              currentUser.organization_id
            ]
          }

          // Execute the update with the appropriate SQL and args
          const userUpdateResult = await client.execute({
            sql,
            args
          })

          logger.info(`User update result: ${JSON.stringify(userUpdateResult.rows, null, 2)}`)

          if (!userUpdateResult.rows || userUpdateResult.rows.length === 0) {
            logger.error('User update failed: No rows affected')
            throw new Error('User update failed - no rows affected')
          }

          logger.info('User details updated successfully')

          // Update agent settings
          const settings = {
            stateLicenses: agent.stateLicenses,
            carrierContracts: agent.carriers,
            stateCarrierSettings: [],
            emailSendBirthday: false,
            emailSendPolicyAnniversary: false,
            emailSendAep: false,
            smartSendEnabled: false
          }

          logger.info(`Agent settings to update: ${JSON.stringify(settings, null, 2)}`)

          const settingsUpdateResult = await client.execute({
            sql: `INSERT INTO agent_settings (
              agent_id,
              settings
            ) VALUES (?, ?)
            ON CONFLICT (agent_id) 
            DO UPDATE SET settings = EXCLUDED.settings
            RETURNING *`,
            args: [
              params.id,
              JSON.stringify(settings)
            ]
          })

          logger.info(`Settings update result: ${JSON.stringify(settingsUpdateResult.rows, null, 2)}`)

          if (!settingsUpdateResult.rows || settingsUpdateResult.rows.length === 0) {
            logger.error('Settings update failed: No rows affected')
            throw new Error('Settings update failed - no rows affected')
          }

          logger.info('Settings updated successfully')

          const updatedUser = userUpdateResult.rows[0]
          const updatedSettings = JSON.parse(settingsUpdateResult.rows[0].settings)

          return {
            success: true,
            message: 'Agent updated successfully',
            agent: {
              id: updatedUser.id.toString(),
              firstName: updatedUser.first_name,
              lastName: updatedUser.last_name,
              email: updatedUser.email,
              phone: updatedUser.phone || '',
              is_admin: Boolean(updatedUser.is_admin),
              is_agent: Boolean(updatedUser.is_agent),
              carriers: updatedSettings.carrierContracts,
              stateLicenses: updatedSettings.stateLicenses
            }
          }

        } catch (error: unknown) {
          const dbError = error as Error
          logger.error(`Database error: ${dbError.message}`)
          set.status = 500
          return {
            success: false,
            error: dbError.message
          }
        }
      })
      // Add DELETE endpoint for agent deletion with contact reassignment
      .delete('/api/agents/:id', async ({ params, request, set, query }: {
        params: { id: string },
        request: Request,
        set: any,
        query: { reassignTo?: string }
      }) => {
        try {
          const currentUser = await getUserFromSession(request)
          if (!currentUser) {
            set.status = 401
            return {
              success: false,
              error: 'You must be logged in to perform this action'
            }
          }

          // Only admins can delete agents
          if (!currentUser.is_admin) {
            set.status = 403
            return {
              success: false,
              error: 'Only administrators can delete agents'
            }
          }

          const agentId = params.id
          
          // Prevent users from deleting themselves
          if (String(currentUser.id) === agentId) {
            set.status = 403
            return {
              success: false,
              error: 'You cannot delete your own account'
            }
          }
          
          const reassignToAgentId = query.reassignTo

          // Get the main database client for user/agent operations
          const client = db.getClient()

          // Also get the organization-specific database for contact operations
          const orgDb = await Database.getOrgDb(currentUser.organization_id.toString())

          // First handle contact operations in org database
          try {
            // Using the transaction method instead of direct SQL commands
            await orgDb.transaction(async (orgTx) => {
            // If reassignToAgentId is provided, reassign contacts to the new agent
            if (reassignToAgentId && reassignToAgentId !== agentId) {
              logger.info(`Reassigning contacts from agent ${agentId} to agent ${reassignToAgentId}`)
              
                // Update contacts in the org-specific database
                await orgTx.execute(
                  `UPDATE contacts
                  SET agent_id = ?
                   WHERE agent_id = ?`,
                  [reassignToAgentId, agentId]
                )
            } else {
              // Set agent_id to NULL for contacts associated with this agent
              logger.info(`Setting contacts from agent ${agentId} to have no assigned agent`)
              
                // Update contacts in the org-specific database
                await orgTx.execute(
                  `UPDATE contacts
                  SET agent_id = NULL
                   WHERE agent_id = ?`,
                  [agentId]
                )
              }
            })
            
            logger.info(`Successfully updated contacts for agent ${agentId}`)
            
            // Now handle agent operations in the main database
            await db.transaction(async (tx) => {
              // First, check if the user has any other related records that need to be deleted

            // Delete agent's settings
              await tx.execute(
                `DELETE FROM agent_settings
                 WHERE agent_id = ?`,
                [agentId]
              )
              
              // Check for any related records in other tables that might reference this user
              // For example, delete from sessions table if it exists
              await tx.execute(
                `DELETE FROM sessions
                 WHERE user_id = ?`,
                [agentId]
              )
              
              // IMPORTANT: Add any other related tables that might have foreign keys to users
              
              // Finally, completely delete the user record instead of just marking as inactive
              await tx.execute(
                `DELETE FROM users
                 WHERE id = ?`,
                [agentId]
              )
            })
            
            logger.info(`Successfully deleted agent ${agentId}`)

            return {
              success: true,
              message: 'Agent deleted successfully'
            }
          } catch (error) {
            logger.error(`Error in agent deletion: ${error}`)
            throw error;
          }
        } catch (e) {
          logger.error(`Error deleting agent: ${e}`)
          set.status = 500
          return {
            success: false,
            error: 'An error occurred while deleting the agent'
          }
        }
      })
      // Add this endpoint within the app definition
      .get('/api/me', async ({ request, set }) => {
        try {
          const currentUser = await getUserFromSession(request)
          logger.info(`GET /api/me - Current user from session: ${JSON.stringify(currentUser)}`)
          
          if (!currentUser) {
            set.status = 401
            return {
              success: false,
              error: 'Not authenticated'
            }
          }

          // Get user details including agent settings if they exist
          const client = db.getClient()
          const userDetails = await client.execute({
            sql: `
              SELECT 
                u.id,
                u.email,
                u.first_name as firstName,
                u.last_name as lastName,
                u.is_admin,
                u.is_agent,
                u.phone,
                u.organization_id,
                o.slug as organization_slug,
                o.subscription_tier,
                a.settings as agentSettings
              FROM users u
              JOIN organizations o ON u.organization_id = o.id
              LEFT JOIN agent_settings a ON a.agent_id = u.id
              WHERE u.id = ?
            `,
            args: [currentUser.id]
          })

          logger.info(`GET /api/me - Raw user details from DB: (omitted)`)

          if (!userDetails.rows[0]) {
            set.status = 404
            return {
              success: false,
              error: 'User not found'
            }
          }

          const user = userDetails.rows[0]
          const response = {
            success: true,
            user: {
              id: user.id,
              email: user.email,
              firstName: user.firstName,
              lastName: user.lastName,
              is_admin: Boolean(user.is_admin),
              is_agent: Boolean(user.is_agent),
              phone: user.phone || '',
              organization_id: user.organization_id,
              organization_slug: user.organization_slug,
              subscription_tier: user.subscription_tier,
              agentSettings: user.agentSettings ? JSON.parse(user.agentSettings) : null
            }
          }
          logger.info(`GET /api/me - Sending response`)
          return response

        } catch (e) {
          logger.error(`Error fetching current user: ${e}`)
          set.status = 500
          return {
            success: false,
            error: String(e)
          }
        }
      })
      // Add development endpoints for easy session management
      .get('/api/dev/session/:redirect', async ({ params, set }) => {
        // Only allow in development
        if (process.env.NODE_ENV === 'production') {
          set.status = 404
          return { error: 'Not found' }
        }

        try {
          // Get most recent session from database
          const client = db.getClient()
          const result = await client.execute({
            sql: `
              SELECT s.id 
              FROM sessions s
              JOIN users u ON s.user_id = u.id
              WHERE u.is_active = 1
              ORDER BY s.created_at DESC 
              LIMIT 1
            `
          })

          if (!result.rows[0]) {
            set.status = 404
            return { error: 'No sessions found' }
          }

          // Convert the numeric ID to a string
          const sessionId = String(result.rows[0].id)
          logger.info(`Setting session cookie: ${sessionId} for redirect to: ${params.redirect}`)

          // Set the session cookie
          set.headers['Set-Cookie'] = `session=${sessionId}; Path=/; HttpOnly; SameSite=Lax`

          // Handle redirect
          const redirectPath = params.redirect === 'add-agent' ? 'agents/add' : params.redirect
          set.redirect = `/${redirectPath}`
          return { success: true }
        } catch (e) {
          logger.error(`Error in dev session endpoint: ${e}`)
          set.status = 500
          return { error: String(e) }
        }
      })
      // Add development endpoints for easy session management
      .get('/api/dev/session/login', async ({ set }) => {
        // Only allow in development
        if (process.env.NODE_ENV === 'production') {
          set.status = 404
          return { error: 'Not found' }
        }

        try {
          // Get most recent session from database
          const client = db.getClient()
          const result = await client.execute({
            sql: `
              SELECT s.id 
              FROM sessions s
              JOIN users u ON s.user_id = u.id
              WHERE u.is_active = 1
              ORDER BY s.created_at DESC 
              LIMIT 1
            `
          })

          if (!result.rows[0]) {
            set.status = 404
            return { error: 'No sessions found' }
          }

          // Convert the numeric ID to a string
          const sessionId = String(result.rows[0].id)
          logger.info(`Setting session cookie: ${sessionId} for login page`)

          // Set the session cookie
          set.headers['Set-Cookie'] = `session=${sessionId}; Path=/; HttpOnly; SameSite=Lax`
          return { success: true }
        } catch (e) {
          logger.error(`Error in dev session endpoint: ${e}`)
          set.status = 500
          return { error: String(e) }
        }
      })
      // Add ZIP lookup endpoint
      .get('/api/zip-lookup/:zipCode', ({ params: { zipCode } }) => {
        try {
          const zipInfo = ZIP_DATA[zipCode]
          if (!zipInfo) {
            return {
              success: false,
              error: `Invalid ZIP code: ${zipCode}`
            }
          }
          return {
            success: true,
            ...zipInfo
          }
        } catch (e) {
          logger.error(`Error looking up ZIP code ${zipCode}: ${e}`)
          return {
            success: false,
            error: String(e)
          }
        }
      })

      .get('/api/contacts/email-tracking/:contactId', async ({ params, request }) => {
        try {
          logger.info(`Fetching email tracking records for contact ID: ${params.contactId}`);
          
          const user = await getUserFromSession(request);
          if (!user) {
            logger.warn(`Authentication failed when fetching email tracking for contact ID: ${params.contactId}`);
            return {
              success: false,
              message: 'Authentication required'
            };
          }

          const contactId = Number(params.contactId);
          if (isNaN(contactId)) {
            logger.warn(`Invalid contact ID provided: ${params.contactId}`);
            return {
              success: false,
              message: 'Invalid contact ID'
            };
          }

          logger.info(`User requesting email tracking for contact ID: ${contactId}`);
          const orgDb = await Database.getOrgDb(user.organization_id.toString());
          logger.info(`Fetching email tracking records from organization database`);

          const trackingRecords = await orgDb.fetchAll(
            `SELECT email_type,scheduled_date,send_status,send_mode FROM email_send_tracking 
             WHERE contact_id = ? 
             ORDER BY created_at DESC`,
            [contactId.toString()]
          );
          
          logger.info(`Found ${trackingRecords.length} email tracking records for contact ID: ${contactId}`);

          return {
            success: true,
            trackingRecords
          };
        } catch (error) {
          logger.error(`Error fetching email tracking records: ${error}`);
          return {
            success: false,
            message: 'Failed to fetch email tracking records',
            error: String(error)
          };
        }
      })
      // Send quote email to contact
      .post('/api/contacts/:contactId/send-quote-email', async ({ params, request, body }) => {
        try {
          const user = await getUserFromSession(request);
          if (!user) {
            return {
              success: false,
              message: 'Authentication required'
            };
          }

          const contactId = Number(params.contactId);
          if (isNaN(contactId)) {
            return {
              success: false,
              message: 'Invalid contact ID'
            };
          }

          // Get org-specific database
          const orgDb = await Database.getOrgDb(user.organization_id.toString());

          // Fetch contact details
          const contact = await orgDb.fetchOne<{id: number, first_name: string, last_name: string, email: string, plan_type: string, phone_number: string}>(
            'SELECT id, first_name, last_name, email, plan_type, phone_number FROM contacts WHERE id = ?',
            [contactId]
          );

          if (!contact) {
            return {
              success: false,
              message: 'Contact not found'
            };
          }

          logger.info(`Contact: ${JSON.stringify(contact)}`);

          // Use the proper generateQuoteId function
          const quoteId = generateQuoteId(user.organization_id, contactId);
          
          // Calculate base URL
          const baseUrl = process.env.PUBLIC_URL || 'http://localhost:5173';
          let quoteUrl = `${baseUrl}/compare?id=${quoteId}&planType=${contact.plan_type}`;

          // Add organization ID to URL
          quoteUrl += `&orgId=${user.organization_id}`;

          // Send the email via SendGrid
          const emailService = new EmailService();
          
          // Fetch organization data
          const organization = await db.fetchOne<{
            name: string;
            logo_data: string;
            primary_color: string;
            phone: string;
            website: string;
          }>(
            'SELECT name, logo_data, primary_color, phone, website FROM organizations WHERE id = ?',
            [user.organization_id]
          );
          
          // Log more details about the logo data
          if (organization?.logo_data) {
            const logoDataPrefix = organization.logo_data.substring(0, 50);
            logger.info(`Logo data prefix: ${logoDataPrefix}...`);
          }
          
          // Log the presence of logo data
          logger.info(`Sending quote email for org ${user.organization_id} with logo: ${organization?.logo_data ? 'Present' : 'Missing'}`);
          
          const result = await emailService.sendQuoteEmail({
            email: contact.email,
            firstName: contact.first_name,
            lastName: contact.last_name,
            quoteUrl,
            planType: contact.plan_type,
            organization: organization || undefined,
            phone: contact.phone_number
          });

          // Record in email tracking table
          const now = new Date().toISOString();
          const batchId = `manual-${Date.now()}-${contactId}`;
          
          await emailService.recordEmailSend(orgDb, {
            orgId: user.organization_id,
            contactId: contactId,
            emailType: 'quote_email',
            sendStatus: 'sent',
            sendMode: 'production',
            batchId: batchId,
            messageId: result.messageId
          });

          const trackingRecord = await orgDb.fetchOne<{id: number, email_type: string, scheduled_date: string, send_status: string, send_mode: string}>(
            'SELECT id, email_type, scheduled_date, send_status, send_mode FROM email_send_tracking WHERE batch_id = ?',
            [batchId]
          );

          // Update last_emailed timestamp
          await orgDb.execute(
            'UPDATE contacts SET last_emailed = CURRENT_TIMESTAMP WHERE id = ?',
            [contactId]
          );

          return {
            success: true,
            message: 'Quote email sent successfully',
            trackingRecords: trackingRecord ? {
              id: trackingRecord.id,
              email_type: trackingRecord.email_type,
              scheduled_date: trackingRecord.scheduled_date,
              send_status: trackingRecord.send_status,
              send_mode: trackingRecord.send_mode
            } : null
          };
        } catch (error) {
          logger.error(`Error sending quote email: ${error}`);
          return {
            success: false,
            message: 'Failed to send quote email',
            error: String(error)
          };
        }
      })
      
      .post('/api/contact-request', async ({ body, request }: { body: { name: string; email: string; type: string; quoteId?: string }, request: Request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          const contactRequest = body as { name: string, email: string, type: string, quoteId: string }
          const { name, email, type, quoteId } = contactRequest
          logger.info(`Processing contact request for ${email} (type: ${type})`)
          
          // Get org-specific database
          const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString())

          // Check for existing contact
          const existingContact = await orgDb.fetchOne<{ id: number }>(
            'SELECT id FROM contacts WHERE LOWER(TRIM(email)) = LOWER(TRIM(?))',
            [email]
          )

          let eventTargetId: number
          let isLead = false

          if (existingContact) {
            // Use existing contact
            logger.info(`Found existing contact with ID ${existingContact.id}`)
            eventTargetId = existingContact.id
          } else {
            // Create new lead
            logger.info(`No existing contact found, creating lead for ${email}`)
            const leadResult = await orgDb.execute(
              `INSERT INTO leads (name, email) VALUES (?, ?) RETURNING id`,
              [name, email]
            )
            
            if (!leadResult.rows?.[0]?.id) {
              throw new Error('Failed to create lead')
            }
            
            eventTargetId = leadResult.rows[0].id
            isLead = true
          }

          // Record the event
          const metadata = {
            quoteId,
            requestType: type
          }

          const eventQuery = isLead
            ? `INSERT INTO contact_events (lead_id, event_type, metadata) VALUES (?, 'followup_request', ?)`
            : `INSERT INTO contact_events (contact_id, event_type, metadata) VALUES (?, 'followup_request', ?)`

          await orgDb.execute(eventQuery, [eventTargetId, JSON.stringify(metadata)])
          logger.info(`Recorded followup request event for ${isLead ? 'lead' : 'contact'} ${eventTargetId}`)

          // Return success response
          return {
            success: true,
            message: 'Contact request recorded successfully'
          }

        } catch (e: unknown) {
          logger.error(`Error processing contact request: ${e instanceof Error ? e.message : String(e)}`)
          throw new Error(e instanceof Error ? e.message : String(e))
        }
      })
      .get('/api/contacts/:id/eligibility', async ({ params: { id }, request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          logger.info(`GET /api/contacts/${id}/eligibility - Fetching eligibility results`)
          
          // Get org-specific database
          const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString())
          
          // Get most recent eligibility answers for this contact
          const result = await orgDb.fetchOne(
            `SELECT answers 
             FROM eligibility_answers 
             WHERE contact_id = ? 
             ORDER BY created_at DESC 
             LIMIT 1`,
            [id]
          ) as { answers: string } | null

          if (!result) {
            return {
              status: "incomplete",
              answers: null
            }
          }

          // Parse answers JSON and determine status
          const answers = JSON.parse(result.answers)
          const allTrue = Object.values(answers).every(value => value === true)

          return {
            status: allTrue ? "pass" : "flagged",
            answers: result.answers  // Return the raw JSON string instead of the parsed object
          }

        } catch (e) {
          logger.error(`Error fetching eligibility results: ${e}`)
          throw new Error(String(e))
        }
      })
      // Add new endpoint to get follow-up requests
      .get('/api/contacts/:id/follow-ups', async ({ params: { id }, request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          logger.info(`GET /api/contacts/${id}/follow-ups - Fetching follow-up requests`)
          
          // Get org-specific database
          const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString())
          
          // Get follow-up requests from contact_events table
          const result = await orgDb.execute(
            `SELECT 
              event_type,
              metadata,
              created_at
             FROM contact_events 
             WHERE contact_id = ? 
               AND event_type = 'followup_request'
             ORDER BY created_at DESC`,
            [id]
          )

          // Map results to a more friendly format
          const followUps = result.rows?.map((row: { metadata: string, created_at: string }) => {
            const metadata = JSON.parse(row.metadata)
            return {
              type: metadata.requestType,
              quoteId: metadata.quoteId,
              createdAt: row.created_at
            }
          }) || []

          return followUps

        } catch (e) {
          logger.error(`Error fetching follow-up requests: ${e instanceof Error ? e.message : String(e)}`)
          throw new Error(e instanceof Error ? e.message : String(e))
        }
      })
      // Add profile update endpoint
      .put('/api/profile', async ({ request, body, set }) => {
        try {
          const currentUser = await getUserFromSession(request)
          if (!currentUser) {
            set.status = 401
            return {
              success: false,
              error: 'Not authenticated'
            }
          }

          const { firstName, lastName, phone } = body as { 
            firstName: string;
            lastName: string;
            phone: string;
          }

          // Get the libSQL client
          const client = db.getClient()

          // Update only allowed profile fields
          const result = await client.execute({
            sql: `UPDATE users 
                  SET first_name = ?, 
                      last_name = ?, 
                      phone = ?
                  WHERE id = ?
                  RETURNING *`,
            args: [firstName, lastName, phone, currentUser.id]
          })

          if (!result.rows || result.rows.length === 0) {
            set.status = 404
            return {
              success: false,
              error: 'User not found'
            }
          }

          return {
            success: true,
            message: 'Profile updated successfully'
          }

        } catch (error) {
          logger.error(`Error updating profile: ${error}`)
          set.status = 500
          return {
            success: false,
            error: String(error)
          }
        }
      })
      .get('/api/agents/:id/contacts', async ({ params, request }) => {
        try {
          const currentUser = await getUserFromSession(request)
          if (!currentUser?.organization_id) {
            throw new Error('No organization ID found in session')
          }
          
          logger.info(`GET /api/agents/${params.id}/contacts - Fetching contacts for agent ${params.id}`)
          
          // Get organization-specific database
          const orgDb = await Database.getOrInitOrgDb(currentUser.organization_id.toString())
          
          // Fetch all contacts for the agent
          const result = await orgDb.fetchAll('SELECT * FROM contacts WHERE agent_id = ?', [params.id])
          
          logger.info(`GET /api/agents/${params.id}/contacts - Found ${result.length} contacts`)
          
          // Map the database results to the expected format with camelCase field names
          const contacts = result.map((contact: { 
            id: number;
            first_name: string;
            last_name: string;
            email: string;
            current_carrier: string;
            plan_type: string;
            effective_date: string;
            birth_date: string;
            tobacco_user: number;
            gender: string;
            state: string;
            zip_code: string;
            agent_id: number | null;
            last_emailed: string | null;
            phone_number: string;
          }) => ({
            id: contact.id,
            first_name: contact.first_name,
            last_name: contact.last_name,
            email: contact.email,
            current_carrier: contact.current_carrier,
            plan_type: contact.plan_type,
            effective_date: contact.effective_date,
            birth_date: contact.birth_date,
            tobacco_user: Boolean(contact.tobacco_user),
            gender: contact.gender,
            state: contact.state,
            zip_code: contact.zip_code,
            agent_id: contact.agent_id,
            last_emailed: contact.last_emailed,
            phone_number: contact.phone_number || ''
          }))
          
          return {
            success: true,
            contacts: contacts
          }
        } catch (e: unknown) {
          logger.error(`Error fetching contacts for agent ${params.id}: ${e instanceof Error ? e.message : String(e)}`)
          return {
            success: false,
            error: String(e)
          }
        }
      })
      .get('/api/contact-requests', async ({ request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          logger.info(`GET /api/contact-requests - Fetching follow-up requests`)
          
          // Get org-specific database
          const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString())
          
          // Fetch all follow-up requests
          const result = await orgDb.fetchAll('SELECT * FROM contact_events WHERE event_type = ?', ['followup_request'])
          
          logger.info(`GET /api/contact-requests - Found ${result.length} follow-up requests`)
          
          // Map the database results to the expected format with camelCase field names
          const followUps = result.map(followUp => ({
            id: followUp.id,
            event_type: followUp.event_type,
            metadata: JSON.parse(followUp.metadata),
            created_at: followUp.created_at
          }))
          
          return {
            success: true,
            followUps: followUps
          }
        } catch (e) {
          logger.error(`Error fetching follow-up requests: ${e}`)
          return {
            success: false,
            error: String(e)
          }
        }
      })
      .get('/api/contacts/:id/follow-ups', async ({ params: { id }, request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          logger.info(`GET /api/contacts/${id}/follow-ups - Fetching follow-up requests`)
          
          // Get org-specific database
          const orgDb = await Database.getOrInitOrgDb(user.organization_id.toString())
          
          // Get follow-up requests from contact_events table
          const result = await orgDb.execute(
            `SELECT 
              event_type,
              metadata,
              created_at
             FROM contact_events 
             WHERE contact_id = ? 
               AND event_type = 'followup_request'
             ORDER BY created_at DESC`,
            [id]
          )

          // Map results to a more friendly format
          const followUps = result.rows?.map((row: { metadata: string, created_at: string }) => {
            const metadata = JSON.parse(row.metadata)
            return {
              type: metadata.requestType,
              quoteId: metadata.quoteId,
              createdAt: row.created_at
            }
          }) || []

          return followUps

        } catch (e) {
          logger.error(`Error fetching follow-up requests: ${e instanceof Error ? e.message : String(e)}`)
          throw new Error(e instanceof Error ? e.message : String(e))
        }
      })
      .listen(8000)

    logger.info('Server started on port 8000')

    // Schedule the cleanup job to run daily at midnight
    cron.schedule('0 0 * * *', () => {
      logger.info('Running scheduled cleanup job for old organizations');
      // cleanupOldOrganizations().catch(error => {
      //   logger.error(`Error in scheduled cleanup job: ${error}`);
      // });
    });
    
    logger.info('Scheduled daily cleanup job for old organizations');

    return app
  } catch (error) {
    logger.error(`Error starting server: ${error}`)
    throw error
  }
}

startServer()