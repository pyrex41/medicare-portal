import { Elysia, t } from 'elysia'
import { cors } from '@elysiajs/cors'
import { Database } from './database'
import { logger } from './logger'
import { ContactCreate, AgentCreate } from './types'
import { readFileSync } from 'fs'

// Load ZIP code data
let ZIP_DATA = {}
try {
  ZIP_DATA = JSON.parse(readFileSync('../zipData.json', 'utf-8'))
} catch (e) {
  logger.error(`Error loading ZIP data: ${e}`)
}

const db = new Database()

const app = new Elysia()
  .use(cors({
    origin: 'http://localhost:5173',
    credentials: true
  }))
  .get('/api/contacts', async () => {
    try {
      logger.info('Attempting to fetch contacts')
      const contacts = await db.fetchAll(
        'SELECT * FROM contacts ORDER BY created_at DESC LIMIT 100'
      )
      logger.info(`Successfully fetched ${contacts.length} contacts`)

      return contacts.map(contact => ({
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
        updated_at: contact[14]
      }))
    } catch (e) {
      logger.error(`Error in get_contacts: ${e}`)
      throw new Error(String(e))
    }
  })
  .post('/api/contacts', async ({ body }) => {
    try {
      const contact = body as ContactCreate
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
        updated_at: row[15]
      }
    } catch (e) {
      logger.error(`Error creating contact: ${e}`)
      throw new Error(String(e))
    }
  })
  // Add more endpoints here...
  .listen(3000)

console.log(`🦊 Elysia is running at ${app.server?.hostname}:${app.server?.port}`) 