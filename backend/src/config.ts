import { config as dotenvConfig } from 'dotenv'
import { resolve } from 'path'
import { logger } from './logger'
import { existsSync } from 'fs'

// Get absolute path to .env file
const envPath = resolve(__dirname, '../.env')

// Check if .env file exists
if (!existsSync(envPath)) {
  console.error(`❌ .env file not found at: ${envPath}`)
  process.exit(1)
}

// Load .env file with override option
const result = dotenvConfig({ 
  path: envPath,
  override: true // This tells dotenv to override existing env vars
})

if (result.error) {
  console.error('❌ Error loading .env file:', result.error)
  process.exit(1)
}

// Debug: Print raw env file path and contents
console.log('📁 Loading .env from:', envPath)
console.log('📝 Environment variables loaded:', {
  TURSO_DATABASE_URL: process.env.TURSO_DATABASE_URL,
  TURSO_AUTH_TOKEN: process.env.TURSO_AUTH_TOKEN ? '[PRESENT]' : '[MISSING]',
  TURSO_DATABASE_PATH: process.env.TURSO_DATABASE_PATH
})

export const config = {
  TURSO_DATABASE_URL: process.env.TURSO_DATABASE_URL,
  TURSO_AUTH_TOKEN: process.env.TURSO_AUTH_TOKEN,
  TURSO_DATABASE_PATH: process.env.TURSO_DATABASE_PATH,
  quoteApiKey: process.env.QUOTE_API_KEY,
  // Add other config values here
}

// Log loaded config (safely)
logger.info(`Config loaded from ${envPath}`)