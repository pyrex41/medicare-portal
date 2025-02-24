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
import { errorHandler } from './middleware/error'
import { getUserFromSession } from './services/auth'

// At the top of the file, add interface for ZIP data
interface ZipInfo {
  state: string;
  // Add other ZIP info properties as needed
}

// Update ZIP_DATA declaration
let ZIP_DATA: Record<string, ZipInfo> = {}
try {
  ZIP_DATA = JSON.parse(readFileSync('../zipData.json', 'utf-8'))
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
function validateISODate(dateStr: string): { isValid: boolean; isoDate: string | null } {
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
    
    // Verify the date is not in the future
    if (date > new Date()) {
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

const startServer = async () => {
  try {
    const db = new Database()
    logger.info('Database initialized successfully')

    const app = new Elysia()
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
      // Log all requests
      .onRequest(({ request: { method, url, headers } }) => {
        const path = new URL(url).pathname
        logger.info(`⮕ ${method} ${path}`)
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
      .get('/api/contacts', async ({ request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          logger.info(`GET /api/contacts - Attempting to fetch contacts for org ${user.organization_id}`)
          
          // Get org-specific database
          const orgDb = await Database.getOrgDb(user.organization_id.toString())
          
          // First get all unique carriers and states for filter options
          const [carrierResults, stateResults] = await Promise.all([
            orgDb.fetchAll('SELECT DISTINCT current_carrier FROM contacts WHERE current_carrier IS NOT NULL ORDER BY current_carrier'),
            orgDb.fetchAll('SELECT DISTINCT state FROM contacts WHERE state IS NOT NULL ORDER BY state')
          ])

          const allCarriers = carrierResults.map((row: any[]) => row[0])
          const allStates = stateResults.map((row: any[]) => row[0])
          
          // Get search query and filters from URL params
          const url = new URL(request.url)
          const searchQuery = url.searchParams.get('search') || ''
          const carriers = (url.searchParams.get('carriers') || '').split(',').filter(Boolean)
          const states = (url.searchParams.get('states') || '').split(',').filter(Boolean)
          
          // Build the SQL query with search and filter conditions
          let conditions = []
          let params = []

          // Add search condition if search query exists
          if (searchQuery) {
            conditions.push(`(
              LOWER(first_name) LIKE ? OR 
              LOWER(last_name) LIKE ? OR 
              LOWER(email) LIKE ? OR
              LOWER(current_carrier) LIKE ? OR
              LOWER(state) LIKE ?
            )`)
            const searchTerm = `%${searchQuery.toLowerCase()}%`
            params.push(searchTerm, searchTerm, searchTerm, searchTerm, searchTerm)
          }

          // Add carrier filter if carriers are specified
          if (carriers.length > 0) {
            conditions.push(`current_carrier IN (${carriers.map(() => '?').join(',')})`)
            params.push(...carriers)
          }

          // Add state filter if states are specified
          if (states.length > 0) {
            conditions.push(`state IN (${states.map(() => '?').join(',')})`)
            params.push(...states)
          }

          // Construct the final query
          const query = `
            SELECT * FROM contacts 
            ${conditions.length > 0 ? 'WHERE ' + conditions.join(' AND ') : ''}
            ORDER BY id DESC
          `
          
          // Execute query with params
          const contacts = await orgDb.fetchAll(query, params)

          if (!contacts || !Array.isArray(contacts)) {
            logger.warn('GET /api/contacts - No contacts found or invalid response')
            return {
              contacts: [],
              filterOptions: {
                carriers: allCarriers,
                states: allStates
              }
            }
          }

          logger.info(`GET /api/contacts - Successfully fetched ${contacts.length} contacts from org database`)

          const mappedContacts = contacts.map(contact => ({
            id: contact[0],
            first_name: contact[1],
            last_name: contact[2],
            email: contact[3],
            current_carrier: contact[4],
            plan_type: contact[5],
            effective_date: contact[6],
            birth_date: contact[7],
            tobacco_user: Boolean(contact[8]),
            gender: contact[9],
            state: contact[10],
            zip_code: contact[11],
            agent_id: contact[12],
            last_emailed: contact[13],
            phone_number: contact[14] || ''
          }))

          logger.info(`GET /api/contacts - Returning ${mappedContacts.length} contacts with ${allCarriers.length} carriers and ${allStates.length} states`)
          return {
            contacts: mappedContacts,
            filterOptions: {
              carriers: allCarriers,
              states: allStates
            }
          }
        } catch (e) {
          logger.error(`Error in GET /api/contacts: ${e}`)
          throw new Error(String(e))
        }
      })
      .get('/api/contacts/check-email/:email', async ({ params: { email }, request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          const orgDb = await Database.getOrgDb(user.organization_id.toString())
          
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
          const orgDb = await Database.getOrgDb(user.organization_id.toString())
          
          // Fetch the contact
          const result = await orgDb.fetchOne<ContactRow>(
            'SELECT * FROM contacts WHERE id = ?',
            [id]
          )

          if (!result) {
            throw new Error(`Contact ${id} not found`)
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
          const orgDb = await Database.getOrgDb(user.organization_id.toString())

          // Check for existing email
          const existingContact = await orgDb.fetchOne(
            'SELECT 1 FROM contacts WHERE LOWER(TRIM(email)) = LOWER(TRIM(?))',
            [contact.email]
          )

          if (existingContact) {
            throw new Error('A contact with this email already exists')
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
      .put('/api/contacts/:id', async ({ params: { id }, body, request }) => {
        try {
          // Get user and org info
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          // Get org-specific database
          const orgDb = await Database.getOrgDb(user.organization_id.toString())

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
              agent_id = ?,
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
            contact.agent_id || null,
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
      .delete('/api/contacts', async ({ body, request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          const contactIds = body as number[]
          logger.info(`DELETE /api/contacts - Attempting to delete contacts with IDs: ${contactIds} for org ${user.organization_id}`)
          
          // Get org-specific database
          const orgDb = await Database.getOrgDb(user.organization_id.toString())

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
      // Add file upload endpoint
      .post('/api/contacts/upload', async ({ body, request }) => {
        try {
          // Get user and org info
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          // Get org-specific database
          const orgDb = await Database.getOrgDb(user.organization_id.toString())

          // Extract file and overwrite flag from form data
          const formData = body as { file: File, overwrite_duplicates: boolean | string }
          const file = formData.file
          // Convert string 'false'/'true' to boolean
          const overwriteDuplicates = formData.overwrite_duplicates === 'true'

          logger.info(`Initial overwriteDuplicates value: ${overwriteDuplicates}, type: ${typeof overwriteDuplicates}, raw value: ${formData.overwrite_duplicates}`)

          logger.info(`POST /api/contacts/upload - Processing CSV upload with overwriteDuplicates=${overwriteDuplicates}`)

          // Read file contents
          const fileContents = await file.text()
          
          // Parse CSV
          const records = csvParse(fileContents, {
            columns: true,
            skip_empty_lines: true
          })

          // Required fields in desired order
          const requiredFields = [
            'First Name',
            'Last Name',
            'Email',
            'Current Carrier',
            'Plan Type',
            'Effective Date',
            'Birth Date',
            'Tobacco User',
            'Gender',
            'ZIP Code',
            'Phone Number'
          ]

          // Validate headers
          const headers = Object.keys(records[0] || {})
          const missingFields = requiredFields.filter(field => !headers.includes(field))

          // Add this: Get the email column index from headers
          const emailColumnIndex = headers.indexOf('Email')

          if (missingFields.length > 0) {
            return {
              success: false,
              message: `Missing required columns: ${missingFields.join(', ')}`,
              error_csv: null,
              total_rows: 0,
              error_rows: 0,
              valid_rows: 0
            }
          }

          const validRows: any[] = []
          const errorRows: any[] = []
          const paramsList: any[] = []
          const convertedCarrierRows: any[] = []

          // Get existing emails for duplicate checking
          let existingEmails = new Set<string>()
          const emailResults = await orgDb.fetchAll("SELECT email FROM contacts")
          existingEmails = new Set(emailResults.map((row: any[]) => row[0].trim().toLowerCase()))

          logger.info(`Found ${existingEmails.size} existing emails in database`)

          // Validate each row
          for (const [index, row] of records.entries()) {
            const rowNum = index + 2 // Account for header row and 0-based index
            
            // Check for missing values
            const missingValues = requiredFields.filter(field => !row[field]?.trim())
            if (missingValues.length > 0) {
              errorRows.push({
                Row: rowNum,
                ...row,
                Error: `Missing values for: ${missingValues.join(', ')}`
              })
              continue
            }

            // Validate email format
            const email = row['Email'].trim().toLowerCase()
            if (!validateEmail(email)) {
              errorRows.push({
                Row: rowNum,
                ...row,
                Error: `Invalid email format: ${row['Email']}`
              })
              continue
            }

            // Validate phone number
            const phoneResult = standardizePhoneNumber(row['Phone Number']);
            if (!phoneResult.isValid) {
              errorRows.push({
                Row: rowNum,
                ...row,
                Error: `Invalid phone number: ${row['Phone Number']}. Must be exactly 10 digits.`
              })
              continue
            }

            // Validate ZIP code
            const zipCode = row['ZIP Code'].trim()
            const zipInfo = ZIP_DATA[zipCode]
            if (!zipInfo) {
              errorRows.push({
                Row: rowNum,
                ...row,
                Error: `Invalid ZIP code: ${zipCode}`
              })
              continue
            }

            // Validate gender
            const gender = row['Gender'].trim().toUpperCase()
            if (!['M', 'F'].includes(gender)) {
              errorRows.push({
                Row: rowNum,
                ...row,
                Error: `Invalid gender: ${gender}. Must be 'M' or 'F'`
              })
              continue
            }

            // Check for duplicate email
            logger.info(`Checking row ${rowNum} email: ${email}`)
            logger.info(`Overwrite duplicates is set to: ${overwriteDuplicates}`)
            if (existingEmails.has(email)) {
              logger.info(`Found duplicate email: ${email}`)
              const notOverwrite = !overwriteDuplicates
              logger.info(`Debug - overwriteDuplicates: ${overwriteDuplicates}, !overwriteDuplicates: ${notOverwrite}`)
              if (!overwriteDuplicates) {
                logger.info(`Adding duplicate email to error rows since overwrite is disabled`)
                errorRows.push({
                  Row: rowNum,
                  ...row,
                  Error: `Email already exists: ${row['Email']}`
                })
                logger.info('Skipping duplicate email')
                continue
              }
              logger.info(`Allowing duplicate email since overwrite is enabled`)
            }

            // Add carrier validation
            const carrierResult = await validateCarrier(row['Current Carrier'], orgDb);
            let carrierNote = null;
            if (carrierResult.wasConverted) {
              carrierNote = {
                Row: rowNum,
                ...row,
                OriginalCarrier: row['Current Carrier']
              };
            }

            try {
              // Validate dates with better error messages
              const effectiveDateResult = validateISODate(row['Effective Date']);
              if (!effectiveDateResult.isValid) {
                errorRows.push({
                  Row: rowNum,
                  ...row,
                  Error: `Invalid effective date format: ${row['Effective Date']}. Please use YYYY-MM-DD or MM-DD-YYYY format.`
                });
                continue;
              }

              const birthDateResult = validateISODate(row['Birth Date']);
              if (!birthDateResult.isValid) {
                errorRows.push({
                  Row: rowNum,
                  ...row,
                  Error: `Invalid birth date format: ${row['Birth Date']}. Please use YYYY-MM-DD or MM-DD-YYYY format.`
                });
                continue;
              }

              const tobaccoUser = ['yes', 'true', '1', 'y'].includes(row['Tobacco User'].trim().toLowerCase())

              paramsList.push([
                row['First Name'].trim(),
                row['Last Name'].trim(),
                email,
                carrierResult.standardizedName,
                row['Plan Type'].trim(),
                effectiveDateResult.isoDate,
                birthDateResult.isoDate,
                tobaccoUser,
                gender,
                zipInfo.state,
                zipCode,
                phoneResult.standardized
              ])
              validRows.push(row)
              
              // If this row had a carrier conversion, track it
              if (carrierNote) {
                convertedCarrierRows.push(carrierNote)
              }
            } catch (e) {
              errorRows.push({
                Row: rowNum,
                ...row,
                Error: 'Unexpected error processing dates. Please ensure dates are in YYYY-MM-DD format.'
              })
            }
          }

          // Insert valid rows
          let insertedCount = 0
          if (paramsList.length > 0) {
            logger.info(`Processing ${paramsList.length} valid rows with overwriteDuplicates=${overwriteDuplicates}`)
            logger.info(`Debug - overwriteDuplicates value type: ${typeof overwriteDuplicates}`)
            if (overwriteDuplicates) {
              logger.info('Using update/insert logic for duplicates')
              // First update existing records
              const updateQuery = /* sql */ `
                UPDATE contacts SET 
                  first_name = ?,
                  last_name = ?,
                  current_carrier = ?,
                  plan_type = ?,
                  effective_date = ?,
                  birth_date = ?,
                  tobacco_user = ?,
                  gender = ?,
                  state = ?,
                  zip_code = ?
                WHERE LOWER(email) = ?
              `

              for (const params of paramsList) {
                const email = params[2].toLowerCase()
                // Check if email exists
                const existingContact = await orgDb.fetchAll(
                  'SELECT 1 FROM contacts WHERE LOWER(email) = ?',
                  [email]
                )

                if (existingContact.length > 0) {
                  // Update existing contact
                  const updateParams = [
                    params[0], // first_name
                    params[1], // last_name
                    params[3], // current_carrier
                    params[4], // plan_type
                    params[5], // effective_date
                    params[6], // birth_date
                    params[7], // tobacco_user
                    params[8], // gender
                    params[9], // state
                    params[10], // zip_code
                    email     // for WHERE clause
                  ]
                  await orgDb.execute(updateQuery, updateParams)
                } else {
                  // Insert new contact
                  await orgDb.execute(
                    `INSERT INTO contacts (
                      first_name, last_name, email, current_carrier, plan_type,
                      effective_date, birth_date, tobacco_user, gender,
                      state, zip_code, phone_number
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
                    params
                  )
                }
              }
              insertedCount = paramsList.length
            } else {
              logger.info('Using insert-only logic for non-duplicates')
              // If not overwriting duplicates, only insert non-duplicate rows
              for (const params of paramsList) {
                const email = params[2].toLowerCase()
                // Check if email exists
                const existingContact = await orgDb.fetchAll(
                  'SELECT 1 FROM contacts WHERE LOWER(email) = ?',
                  [email]
                )

                if (existingContact.length === 0) {
                  // Only insert if email doesn't exist
                  await orgDb.execute(
                    `INSERT INTO contacts (
                      first_name, last_name, email, current_carrier, plan_type,
                      effective_date, birth_date, tobacco_user, gender,
                      state, zip_code, phone_number
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
                    params
                  )
                  insertedCount++
                }
              }
            }
          }

          // Generate error CSV if needed
          let errorCsv = null
          if (errorRows.length > 0) {
            errorCsv = [
              ['Row', ...requiredFields, 'Error'].join(','),
              ...errorRows.map(row => {
                return [
                  row.Row,
                  ...requiredFields.map(field => `"${row[field] || ''}"`),
                  `"${row.Error}"`
                ].join(',')
              })
            ].join('\n')
          }

          // Generate converted carriers CSV if needed
          let convertedCarriersCsv = null;
          if (convertedCarrierRows.length > 0) {
            convertedCarriersCsv = [
              ['Row', ...requiredFields, 'Original Carrier'].join(','),
              ...convertedCarrierRows.map((row: { Row: number; [key: string]: any }) => {
                return [
                  row.Row,
                  ...requiredFields.map(field => `"${row[field] || ''}"`),
                  `"${row.OriginalCarrier}"`
                ].join(',')
              })
            ].join('\n');
          }

          // Get list of supported carriers and their aliases
          const centralDb = new Database();
          const carriersResult = await centralDb.execute(
            'SELECT name, aliases FROM carriers ORDER BY name'
          );
          
          const supportedCarriers = carriersResult.rows.map(row => ({
            name: row.name,
            aliases: row.aliases ? JSON.parse(row.aliases) : []
          }));

          // Create carrier info message
          const carrierInfoMessage = `Supported carriers: ${supportedCarriers.map((c: { name: string; aliases: string[] }) => 
            `${c.name}${c.aliases.length > 0 ? ` (also accepts: ${c.aliases.join(', ')})` : ''}`
          ).join(', ')}`;

          // Create messages array for different types of feedback
          const messages = [];
          
          // Add error message if there are errors
          if (errorRows.length > 0) {
            messages.push(`Found ${errorRows.length} rows with errors. Successfully imported ${insertedCount} rows.`);
          } else {
            messages.push(`Successfully imported ${insertedCount} rows.`);
          }

          // Add carrier conversion message if there were conversions
          if (convertedCarrierRows.length > 0) {
            messages.push(
              `${convertedCarrierRows.length} rows had unrecognized carriers and were marked as "Other". ` +
              `This is normal if these are carriers we don't support. However, please review the carrier conversion CSV ` +
              `to ensure there are no typos or misspellings of supported carriers.`
            );
          }

          // Add supported carriers message
          messages.push(carrierInfoMessage);

          return {
            success: true,
            message: messages.join('\n\n'),
            error_csv: errorCsv,
            converted_carriers_csv: convertedCarriersCsv,
            total_rows: validRows.length + errorRows.length,
            error_rows: errorRows.length,
            valid_rows: insertedCount,
            converted_carrier_rows: convertedCarrierRows.length,
            supported_carriers: supportedCarriers
          }

        } catch (e) {
          logger.error(`Error processing CSV upload: ${e}`)
          return {
            success: false,
            message: String(e),
            error_csv: null,
            converted_carriers_csv: null,
            total_rows: 0,
            error_rows: 0,
            valid_rows: 0,
            converted_carrier_rows: 0,
            supported_carriers: []
          }
        }
      })
      // Add error handler
      .use(errorHandler)
      // Add explicit debug log for auth routes
      .use(app => {
        logger.info('Registering auth routes...')
        return app.use(createAuthRoutes())
      })
      // Add settings routes
      .use(settingsRoutes)
      // Add organization routes
      .use(organizationRoutes)
      // Add brand routes
      .use(createBrandRoutes())
      // Add quotes routes
      .use(quotesRoutes)
      // In production, serve the frontend static files
      .use(process.env.NODE_ENV === 'production' 
        ? async (app) => {
            const plugin = staticPlugin({
              assets: '../dist',
              prefix: '/'
            })
            return app.use(plugin)
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

          // Get the libSQL client
          const client = db.getClient()

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

          // Then create agent settings
          await client.execute({
            sql: `INSERT INTO agent_settings (
              agent_id,
              settings
            ) VALUES (?, ?)`,
            args: [
              userId,
              JSON.stringify({
                stateLicenses: newAgent.stateLicenses,
                carrierContracts: newAgent.carriers,
                stateCarrierSettings: [],
                emailSendBirthday: false,
                emailSendPolicyAnniversary: false,
                emailSendAep: false,
                smartSendEnabled: false
              })
            ]
          })

          logger.info(`Initialized settings for agent: ${userId}`)

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

          // Add admin check
          if (!currentUser.is_admin && !request.url.includes('/setup')) {
            set.status = 403
            return {
              success: false,
              error: 'Only administrators can view agents'
            }
          }

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

          // Map the database results to the expected format
          const agents = result.rows.map((row: DbRow) => {
            const settings = row.settings ? JSON.parse(row.settings) : {
              stateLicenses: [],
              carrierContracts: [],
              stateCarrierSettings: []
            }

            return {
              id: row.id,
              firstName: row.first_name,
              lastName: row.last_name,
              email: row.email,
              phone: row.phone || '',
              is_admin: Boolean(row.is_admin),
              is_agent: Boolean(row.is_agent),
              carriers: settings.carrierContracts || [],
              stateLicenses: settings.stateLicenses || []
            }
          })

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
      // Update PUT endpoint for updating agent details - moved here to be with other agent endpoints
      .put('/api/agents/:id', async ({ params, body, request, set }: { 
        params: { id: string }, 
        body: AgentUpdate,
        request: Request,
        set: { status: number }
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

          // Check if user is an admin
          if (!currentUser.is_admin) {
            logger.error(`Authorization failed: User ${currentUser.id} is not an admin`)
            set.status = 403
            return {
              success: false,
              error: 'Only administrators can update agents'
            }
          }

          const agent = body
          logger.info(`Updating agent ${params.id} - Name: ${agent.firstName} ${agent.lastName}, Phone: ${agent.phone}`)

          // Get the libSQL client
          const client = db.getClient()

          // Update user details
          const userUpdateResult = await client.execute({
            sql: `UPDATE users 
                  SET first_name = ?, 
                      last_name = ?, 
                      email = ?, 
                      phone = ?,
                      is_admin = ?,
                      is_agent = ?
                  WHERE id = ? AND organization_id = ?
                  RETURNING *`,
            args: [
              agent.firstName,
              agent.lastName,
              agent.email,
              agent.phone,
              agent.is_admin ? 1 : 0,
              agent.is_agent ? 1 : 0,
              params.id,
              currentUser.organization_id
            ]
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
                a.settings as agentSettings
              FROM users u
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
      .post('/api/contact-request', async ({ body, request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          const { name, email, type, quoteId } = body
          logger.info(`Processing contact request for ${email} (type: ${type})`)
          
          // Get org-specific database
          const orgDb = await Database.getOrgDb(user.organization_id.toString())

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

        } catch (e) {
          logger.error(`Error processing contact request: ${e}`)
          throw new Error(String(e))
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
          const orgDb = await Database.getOrgDb(user.organization_id.toString())
          
          // Get most recent eligibility answers for this contact
          const result = await orgDb.fetchOne(
            `SELECT answers 
             FROM eligibility_answers 
             WHERE contact_id = ? 
             ORDER BY created_at DESC 
             LIMIT 1`,
            [id]
          )

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
            answers: answers
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
          const orgDb = await Database.getOrgDb(user.organization_id.toString())
          
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
          const followUps = result.rows.map(row => {
            const metadata = JSON.parse(row.metadata)
            return {
              type: metadata.requestType,
              quoteId: metadata.quoteId,
              createdAt: row.created_at
            }
          })

          return followUps

        } catch (e) {
          logger.error(`Error fetching follow-up requests: ${e}`)
          throw new Error(String(e))
        }
      })
      .post('/api/eligibility-answers', async ({ body, request }) => {
        try {
          const user = await getUserFromSession(request)
          if (!user?.organization_id) {
            throw new Error('No organization ID found in session')
          }

          type EligibilityAnswers = {
            quote_id: string;
            answers: Record<string, boolean>;
          }
          const { quote_id, answers } = body as EligibilityAnswers
          logger.info(`POST /api/eligibility-answers - Recording answers for quote ${quote_id}`)

          // Get org-specific database
          const orgDb = await Database.getOrgDb(user.organization_id.toString())

          // Decode the quote ID - format is {orgId-base36}-{contactId-base36}-{hash}
          const [encodedOrgId, encodedContactId] = quote_id.split('-')
          if (!encodedOrgId || !encodedContactId) {
            throw new Error('Invalid quote ID format')
          }

          // Convert from base36 back to numbers
          const orgId = parseInt(encodedOrgId, 36)
          const contactId = parseInt(encodedContactId, 36)
          
          if (isNaN(orgId) || isNaN(contactId)) {
            throw new Error('Invalid quote ID format - could not decode IDs')
          }

          // Verify the contact exists
          const contactResult = await orgDb.fetchOne(
            'SELECT id FROM contacts WHERE id = ?',
            [contactId]
          )
            
          if (!contactResult) {
            throw new Error('Contact not found')
          }

          // Create eligibility_answers table if it doesn't exist
          await orgDb.execute(`
            CREATE TABLE IF NOT EXISTS eligibility_answers (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              contact_id INTEGER NOT NULL,
              answers TEXT NOT NULL,
              created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
              FOREIGN KEY (contact_id) REFERENCES contacts(id)
            )
          `)

          // Insert the answers
          await orgDb.execute(
            'INSERT INTO eligibility_answers (contact_id, answers) VALUES (?, ?)',
            [contactId, JSON.stringify(answers)]
          )

          return {
            success: true,
            message: 'Eligibility answers recorded successfully'
          }

        } catch (e) {
          logger.error(`Error recording eligibility answers: ${e}`)
          throw new Error(String(e))
        }
      })
      .listen(8000)

    logger.info('Server started on port 8000')
  } catch (error) {
    logger.error(`Error starting server: ${error}`)
    process.exit(1)
  }
}

startServer()