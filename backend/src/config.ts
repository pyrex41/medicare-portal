import { config as loadEnv } from 'dotenv'

loadEnv()

export const config = {
  TURSO_DATABASE_URL: process.env.TURSO_DATABASE_URL,
  TURSO_AUTH_TOKEN: process.env.TURSO_AUTH_TOKEN,
  TURSO_DATABASE_PATH: process.env.TURSO_DATABASE_PATH
} 