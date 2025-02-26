
import { config as dotenvConfig } from 'dotenv'
import { resolve } from 'path'
import { logger } from './logger'
import { existsSync } from 'fs'

// Get absolute path to .env file
const envPath = resolve(__dirname, '../.env')

// Check if .env file exists - but don't exit if using Replit Secrets
const envFileExists = existsSync(envPath)
if (!envFileExists) {
  console.log(`⚠️ .env file not found at: ${envPath}, will attempt to use Replit Secrets instead`)
} else {
  // Load .env file with override option only if it exists
  const result = dotenvConfig({ 
    path: envPath,
    override: true // This tells dotenv to override existing env vars
  })

  if (result.error) {
    console.warn('⚠️ Error loading .env file:', result.error)
  } else {
    console.log('📁 Loading .env from:', envPath)
  }
}

// Log environment variables (safely)
console.log('📝 Environment variables available:', {
  TURSO_DATABASE_URL: process.env.TURSO_DATABASE_URL ? '[PRESENT]' : '[MISSING]',
  TURSO_AUTH_TOKEN: process.env.TURSO_AUTH_TOKEN ? '[PRESENT]' : '[MISSING]',
  TURSO_DATABASE_PATH: process.env.TURSO_DATABASE_PATH ? '[PRESENT]' : '[MISSING]'
})

export const config = {
  TURSO_DATABASE_URL: process.env.TURSO_DATABASE_URL,
  TURSO_AUTH_TOKEN: process.env.TURSO_AUTH_TOKEN,
  TURSO_DATABASE_PATH: process.env.TURSO_DATABASE_PATH,
  quoteApiKey: process.env.QUOTE_API_KEY,
  PUBLIC_URL: process.env.PUBLIC_URL || (process.env.NODE_ENV === 'development' 
    ? 'http://localhost:5173'
    : 'http://localhost:3000'),
  // Add other config values here
}

// Log loaded config (safely)
logger.info(`Config loaded ${envFileExists ? `from ${envPath}` : 'from environment'}`)
