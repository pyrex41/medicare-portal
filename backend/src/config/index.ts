interface Config {
  TURSO_DATABASE_URL?: string;
  TURSO_AUTH_TOKEN?: string;
  TURSO_DATABASE_PATH?: string;
  TURSO_API_TOKEN?: string;
  TURSO_ORG_GROUP: string;
  TURSO_ORG_SLUG: string;
  STRIPE_SECRET_KEY?: string;
  STRIPE_WEBHOOK_SECRET?: string;
  SENDGRID_API_KEY?: string;
  SENDGRID_FROM_EMAIL?: string;
  SENDGRID_TEMPLATES: Record<string, string>;
  USE_LOCAL_SQLITE: boolean;
  LOCAL_DB_PATH: string;
  clientUrl: string;
}

export const config: Config = {
  TURSO_DATABASE_URL: process.env.TURSO_DATABASE_URL,
  TURSO_AUTH_TOKEN: process.env.TURSO_AUTH_TOKEN,
  TURSO_DATABASE_PATH: process.env.TURSO_DATABASE_PATH,
  TURSO_API_TOKEN: process.env.TURSO_API_TOKEN,
  TURSO_ORG_GROUP: process.env.TURSO_ORG_GROUP || 'default',
  TURSO_ORG_SLUG: process.env.TURSO_ORG_SLUG || 'default',
  STRIPE_SECRET_KEY: process.env.STRIPE_SECRET_KEY,
  STRIPE_WEBHOOK_SECRET: process.env.STRIPE_WEBHOOK_SECRET,
  SENDGRID_API_KEY: process.env.SENDGRID_API_KEY,
  SENDGRID_FROM_EMAIL: process.env.SENDGRID_FROM_EMAIL,
  SENDGRID_TEMPLATES: {
    QUOTE_EMAIL: 'd-f43dasd8f9a8sd7f98asd7f',
    WELCOME_EMAIL: 'd-a9s8d7f98as7df89as7df9'
  },
  USE_LOCAL_SQLITE: process.env.USE_LOCAL_SQLITE !== 'false',
  LOCAL_DB_PATH: process.env.LOCAL_DB_PATH || 'data/organizations',
  clientUrl: process.env.NODE_ENV === 'production' 
    ? 'https://app.example.com'
    : 'http://localhost:5173'
} 