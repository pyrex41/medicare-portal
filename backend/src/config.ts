import { config as dotenvConfig } from 'dotenv'
import { resolve } from 'path'
import { logger } from './logger'
import { existsSync } from 'fs'
import dotenv from 'dotenv'
import fs from 'fs'
import path from 'path'

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

// Load environment variables from .env file
if (fs.existsSync(path.join(process.cwd(), '.env'))) {
  dotenv.config()
}

export const config = {
  TURSO_DATABASE_URL: process.env.TURSO_DATABASE_URL,
  TURSO_AUTH_TOKEN: process.env.TURSO_AUTH_TOKEN,
  TURSO_DATABASE_PATH: process.env.TURSO_DATABASE_PATH,
  TURSO_API_TOKEN: process.env.TURSO_API_TOKEN,
  TURSO_ORG_GROUP: process.env.TURSO_ORG_GROUP || 'medicare-portal',
  TURSO_ORG_SLUG: process.env.TURSO_ORG_SLUG || 'pyrex41',
  quoteApiKey: process.env.QUOTE_API_KEY,
  quoteSecret: process.env.QUOTE_SECRET,
  magicLinkSecret: process.env.MAGIC_LINK_SECRET,
  sendgridApiKey: process.env.SENDGRID_API_KEY,
  sendgridFromEmail: process.env.SENDGRID_FROM_EMAIL,
  PUBLIC_URL: process.env.PUBLIC_URL || (process.env.NODE_ENV === 'development' 
    ? 'http://localhost:5173'
    : 'http://localhost:3000'),
  stripe: {
    secretKey: process.env.STRIPE_SECRET_KEY || '',
    publishableKey: process.env.STRIPE_PUBLISHABLE_KEY || '',
    webhookSecret: process.env.STRIPE_WEBHOOK_SECRET || '',
    prices: {
      basic: process.env.STRIPE_PRICE_BASIC || '',
      pro: process.env.STRIPE_PRICE_PRO || '',
      enterprise: process.env.STRIPE_PRICE_ENTERPRISE || '',
      extraAgent: process.env.STRIPE_PRICE_EXTRA_AGENT || '',
      extraContact: process.env.STRIPE_PRICE_EXTRA_CONTACT || '',
    },
    publicKey: process.env.STRIPE_PUBLIC_KEY,
    connectAccount: process.env.STRIPE_CONNECT_ACCOUNT,
  },
  stripeApiKey: process.env.STRIPE_SECRET_KEY,
  stripeWebhookSecret: process.env.STRIPE_WEBHOOK_SECRET,
  clientUrl: process.env.PUBLIC_URL || 'http://localhost:5173',
}

// Log loaded config (safely)
logger.info(`Config loaded ${envFileExists ? `from ${envPath}` : 'from environment'}`)