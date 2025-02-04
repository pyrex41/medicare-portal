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

// Load ZIP code data
let ZIP_DATA = {}
try {
  ZIP_DATA = JSON.parse(readFileSync('../zipData.json', 'utf-8'))
} catch (e) {
  logger.error(`Error loading ZIP data: ${e}`)
}

const startServer = async () => {
  try {
    const db = new Database()
    await db.init()
    logger.info('Database initialized successfully')

    const app = new Elysia()
      .use(cors({
        // In development, allow the Vite dev server origin
        origin: process.env.NODE_ENV === 'development' 
          ? 'http://localhost:5173'
          : false, // Disable CORS in production
        methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
        allowedHeaders: ['Content-Type'],
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
        console.log(`⮕ ${method} ${path}`) 
        logger.info(`⮕ ${method} ${path}`)
      })
      // Log all responses
      .onResponse((context) => {
        const { request: { method }, path, set } = context
        console.log(`⬅ ${method} ${path} ${set.status}`)
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
        console.error(errorMessage)
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
      .get('/api/contacts', async () => {
        try {
          logger.info('GET /api/contacts - Attempting to fetch contacts')
          const contacts = await db.fetchAll(
            'SELECT * FROM contacts ORDER BY created_at DESC'
          )

          if (!contacts || !Array.isArray(contacts)) {
            logger.warn('GET /api/contacts - No contacts found or invalid response')
            return []
          }

          logger.info(`GET /api/contacts - Successfully fetched ${contacts.length} contacts`)

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
            last_emailed: contact[12],
            created_at: contact[13],
          }))

          logger.info(`GET /api/contacts - Returning ${mappedContacts.length} contacts`)
          return mappedContacts
        } catch (e) {
          logger.error(`Error in GET /api/contacts: ${e}`)
          throw new Error(String(e))
        }
      })
      .post('/api/contacts', async ({ body }: { body: ContactCreate }) => {
        try {
          const contact = body
          logger.info(`Attempting to create contact: ${contact.first_name} ${contact.last_name}`)
          
          const query = `
            INSERT INTO contacts (
              first_name, last_name, email, current_carrier, plan_type,
              effective_date, birth_date, tobacco_user, gender,
              state, zip_code, agent_id
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            RETURNING *
          `
          
          const params = [
            contact.first_name,
            contact.last_name,
            contact.email,
            contact.current_carrier,
            contact.plan_type,
            contact.effective_date,
            contact.birth_date,
            contact.tobacco_user ? 1 : 0,
            contact.gender,
            contact.state,
            contact.zip_code,
            contact.agent_id
          ]

          const result = await db.execute(query, params)
          const row = result[0]

          // Match Python response format
          return {
            id: row[0],
            first_name: row[1],
            last_name: row[2],
            email: row[3],
            current_carrier: row[4],
            plan_type: row[5],
            effective_date: row[6],
            birth_date: row[7],
            tobacco_user: Boolean(row[8]),
            gender: row[9],
            state: row[10],
            zip_code: row[11],
            agent_id: row[12],
            last_emailed_date: row[13],
            created_at: row[14],
          }
        } catch (e) {
          logger.error(`Error creating contact: ${e}`)
          throw new Error(String(e))
        }
      })
      .put('/api/contacts/:id', async ({ params: { id }, body }) => {
        try {
          const contact = body as ContactCreate
          logger.info(`PUT /api/contacts/${id} - Updating contact`)

          // Match Python's conditional query based on agent_id
          const query = contact.agent_id === null ? `
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
            WHERE id = ?
            RETURNING *
          ` : `
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
            WHERE id = ?
            RETURNING *
          `

          const queryParams = contact.agent_id === null ? [
            contact.first_name,
            contact.last_name,
            contact.email,
            contact.current_carrier,
            contact.plan_type,
            contact.effective_date,
            contact.birth_date,
            contact.tobacco_user ? 1 : 0,
            contact.gender,
            contact.state,
            contact.zip_code,
            id
          ] : [
            contact.first_name,
            contact.last_name,
            contact.email,
            contact.current_carrier,
            contact.plan_type,
            contact.effective_date,
            contact.birth_date,
            contact.tobacco_user ? 1 : 0,
            contact.gender,
            contact.state,
            contact.zip_code,
            contact.agent_id,
            id
          ]

          const result = await db.execute(query, queryParams)
          const row = result[0]

          return {
            id: row[0],
            first_name: row[1],
            last_name: row[2],
            email: row[3],
            current_carrier: row[4],
            plan_type: row[5],
            effective_date: row[6],
            birth_date: row[7],
            tobacco_user: Boolean(row[8]),
            gender: row[9],
            state: row[10],
            zip_code: row[11],
            agent_id: row[12],
            last_emailed_date: row[13],
            created_at: row[14],
          }
        } catch (e) {
          logger.error(`Error updating contact: ${e}`)
          throw new Error(String(e))
        }
      })
      // Add DELETE endpoint for contacts
      .delete('/api/contacts', async ({ body }) => {
        try {
          const contactIds = body as number[]
          logger.info(`DELETE /api/contacts - Attempting to delete contacts with IDs: ${contactIds}`)

          // Create placeholders for SQL IN clause
          const placeholders = contactIds.map(() => '?').join(',')
          
          const query = `
            DELETE FROM contacts 
            WHERE id IN (${placeholders})
            RETURNING id
          `

          const result = await db.execute(query, contactIds)
          const deletedIds = result.map(row => row[0])

          logger.info(`DELETE /api/contacts - Successfully deleted ${deletedIds.length} contacts`)

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
      .post('/api/contacts/upload', async ({ body }) => {
        try {
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
            'ZIP Code'
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

          // Get existing emails for duplicate checking
          let existingEmails = new Set<string>()
          const emailResults = await db.fetchAll("SELECT email FROM contacts")
          existingEmails = new Set(emailResults.map(row => row[0].toLowerCase()))

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
            const email = row['Email'].trim().toLowerCase()
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

            try {
              // Validate dates
              const effectiveDate = new Date(row['Effective Date'].trim())
              const birthDate = new Date(row['Birth Date'].trim())
              const tobaccoUser = ['yes', 'true', '1', 'y'].includes(row['Tobacco User'].trim().toLowerCase())

              paramsList.push([
                row['First Name'].trim(),
                row['Last Name'].trim(),
                email,
                row['Current Carrier'].trim(),
                row['Plan Type'].trim(),
                effectiveDate.toISOString().split('T')[0],
                birthDate.toISOString().split('T')[0],
                tobaccoUser ? 1 : 0,
                gender,
                zipInfo.state,
                zipCode
              ])
              validRows.push(row)
            } catch (e) {
              errorRows.push({
                Row: rowNum,
                ...row,
                Error: 'Invalid date format. Dates should be YYYY-MM-DD'
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
              const updateQuery = `
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
                const existingContact = await db.fetchAll(
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
                  await db.execute(updateQuery, updateParams)
                } else {
                  // Insert new contact
                  await db.execute(
                    `INSERT INTO contacts (
                      first_name, last_name, email, current_carrier, plan_type,
                      effective_date, birth_date, tobacco_user, gender,
                      state, zip_code
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
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
                const existingContact = await db.fetchAll(
                  'SELECT 1 FROM contacts WHERE LOWER(email) = ?',
                  [email]
                )

                if (existingContact.length === 0) {
                  // Only insert if email doesn't exist
                  await db.execute(
                    `INSERT INTO contacts (
                      first_name, last_name, email, current_carrier, plan_type,
                      effective_date, birth_date, tobacco_user, gender,
                      state, zip_code
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
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

          return {
            success: true,
            message: errorRows.length > 0
              ? `Found ${errorRows.length} rows with errors. Successfully imported ${insertedCount} rows.`
              : `Successfully imported ${insertedCount} rows`,
            error_csv: errorCsv,
            total_rows: validRows.length + errorRows.length,
            error_rows: errorRows.length,
            valid_rows: insertedCount
          }

        } catch (e) {
          logger.error(`Error processing CSV upload: ${e}`)
          return {
            success: false,
            message: String(e),
            error_csv: null,
            total_rows: 0,
            error_rows: 0,
            valid_rows: 0
          }
        }
      })
      // In production, serve the frontend static files
      .use(process.env.NODE_ENV === 'production' 
        ? staticPlugin({
            assets: '../dist', // Where your built frontend files are
            prefix: '/' // Serve at root path
          })
        : (app) => app
      )
      .listen(8000)

    logger.info('Server started on port 8000')
  } catch (error) {
    logger.error(`Error starting server: ${error}`)
    process.exit(1)
  }
}

startServer()