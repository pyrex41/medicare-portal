import crypto from 'crypto';
import { logger } from '../logger';
import { db } from '../database';
import { Database } from '../database';
import type { User } from '../types';
import { config } from '../config';

const algorithm = "aes-256-gcm";
const IV_LENGTH = 12;
const UTF8 = "utf8";
const HEX = "hex";

// Initialize encryption key
let secret = process.env.MAGIC_LINK_SECRET;
if (!secret) {
  if (process.env.NODE_ENV === "production") {
    throw new Error("Must set MAGIC_LINK_SECRET in production");
  }
  secret = "dev-secret-key";
}

const ENCRYPTION_KEY = crypto.scryptSync(secret, "salt", 32);

interface MagicLinkPayload {
  email: string;
  organizationSlug: string;
  expiresAt: number;
  redirectUrl: string;
  orgId?: number;
  name?: string;
}

interface SignupLinkPayload {
  email: string;
  firstName: string;
  lastName: string;
  redirectUrl: string;
}

export class AuthService {
  constructor(private baseUrl?: string) {
    // If no baseUrl is provided, get it from config
    if (!baseUrl) {
      this.baseUrl = config.PUBLIC_URL;
    } else {
      // Ensure baseUrl doesn't end with a slash
      this.baseUrl = baseUrl.replace(/\/$/, '');
    }
    
    logger.info(`AuthService initialized with baseUrl: ${this.baseUrl}`);
  }

  async createSignupLink(
    email: string,
    options?: {
      redirectUrl?: string;
      firstName?: string;
      lastName?: string;
    }
  ): Promise<string> {
    const payload: SignupLinkPayload = {
      email,
      firstName: options?.firstName || '',
      lastName: options?.lastName || '',
      redirectUrl: options?.redirectUrl || '/onboarding'
    };

    logger.info(`Creating signup link with payload: ${JSON.stringify(payload)}`);
    logger.info(`redirectUrl: ${payload.redirectUrl}`);
    const token = this.encrypt(JSON.stringify(payload));
    // URL encode the entire token
    const encodedToken = encodeURIComponent(token);
    logger.info(`Generated magic link token: ${token}`);
    
    return `${this.baseUrl}/signup/verify/${encodedToken}`;
  }

  async verifySignupLink(token: string): Promise<{
    valid: boolean;
    email?: string;
    redirectUrl?: string;
  }> {
    try { 
      const decodedToken = decodeURIComponent(token);
      const decrypted = this.decrypt(decodedToken);
      const payload: SignupLinkPayload = JSON.parse(decrypted);

      if (!payload.email) {
        logger.warn('Missing email in signup link payload');
        return { valid: false };
      }

      if (!payload.redirectUrl) {
        logger.warn('Missing redirectUrl in signup link payload');
        return { valid: false };  
      }

      logger.info('Signup link verification successful, returning payload');
      return {
        valid: true,
        email: payload.email,
        redirectUrl: payload.redirectUrl
      };

    } catch (error) {
      logger.error(`Signup link verification failed: ${error}`);
      return { valid: false };  
    }
  }

  async createMagicLink(
    email: string, 
    organizationSlug: string, 
    options?: { 
      redirectUrl?: string;
      orgId?: number;
      name?: string;
    }
  ): Promise<string> {
    const payload: MagicLinkPayload = {
      email,
      organizationSlug,
      expiresAt: Date.now() + (30 * 60 * 1000), // 30 minutes
      redirectUrl: options?.redirectUrl || '/dashboard',
      ...(options?.orgId && { orgId: options.orgId }),
      ...(options?.name && { name: options.name })
    };

    logger.info(`Creating magic link with payload: ${JSON.stringify(payload)}`);
    logger.info(`redirectUrl: ${payload.redirectUrl}`);
    const token = this.encrypt(JSON.stringify(payload));
    // URL encode the entire token
    const encodedToken = encodeURIComponent(token);
    logger.info(`Generated magic link token: ${token}`);
    return `${this.baseUrl}/auth/verify/${organizationSlug}/${encodedToken}`;
  }

  async verifyMagicLink(token: string, organizationSlug: string): Promise<{
    valid: boolean;
    email?: string;
    redirectUrl?: string;
  }> {
    try {
      logger.info('Starting magic link verification');
      logger.info(`Organization slug: ${organizationSlug}`);

      const decodedToken = decodeURIComponent(token);
      const decrypted = this.decrypt(decodedToken);
      const payload: MagicLinkPayload = JSON.parse(decrypted);

      // Verify organization and expiration
      if (payload.organizationSlug !== organizationSlug) {
        logger.error(`Organization slug mismatch: ${payload.organizationSlug} !== ${organizationSlug}`);
        return { valid: false };
      }

      if (payload.expiresAt < Date.now()) {
        logger.error(`Token expired: ${new Date(payload.expiresAt)} < ${new Date()}`);
        return { valid: false };
      }

      // No need to check agent status here since we only send links to valid agents

      logger.info('Verification successful, returning payload');
      return {
        valid: true,
        email: payload.email,
        redirectUrl: payload.redirectUrl
      };

    } catch (error) {
      logger.error(`Magic link verification failed: ${error}`);
      return { valid: false };
    }
  }

  private encrypt(text: string): string {
    try {
      const iv = crypto.randomBytes(IV_LENGTH);
      const cipher = crypto.createCipheriv(algorithm, ENCRYPTION_KEY, iv);
      let encrypted = cipher.update(text, UTF8, HEX);
      encrypted += cipher.final(HEX);
      const authTag = cipher.getAuthTag();
      
      const token = `${iv.toString(HEX)}:${authTag.toString(HEX)}:${encrypted}`;
      return token;
    } catch (error) {
      logger.error(`Encryption failed: ${error}`);
      throw error;
    }
  }

  private decrypt(text: string): string {
    try {
      const [ivPart, authTagPart, encryptedText] = text.split(":");
      if (!ivPart || !authTagPart || !encryptedText) {
        throw new Error("Invalid token format - missing parts");
      }

      logger.info('Decrypting token parts:');
      logger.info(`IV length: ${ivPart.length}`);
      logger.info(`Auth tag length: ${authTagPart.length}`);
      logger.info(`Encrypted text length: ${encryptedText.length}`);

      const iv = Buffer.from(ivPart, HEX);
      const authTag = Buffer.from(authTagPart, HEX);
      const decipher = crypto.createDecipheriv(algorithm, ENCRYPTION_KEY, iv);
      decipher.setAuthTag(authTag);
      let decrypted = decipher.update(encryptedText, HEX, UTF8);
      decrypted += decipher.final(UTF8);
      return decrypted;
    } catch (error) {
      logger.error(`Decryption failed: ${error}`);
      throw error;
    }
  }

  async getUserFromSession(request: Request): Promise<Partial<User> | null> {
    // Check for bypass header for testing
    const bypassHeader = request.headers.get('X-Bypass-Auth');
    if (bypassHeader) {
      const bypassUserId = bypassHeader === 'true' ? 'test-user-id' : bypassHeader;
      logger.info(`Bypassing authentication with header for user ID: ${bypassUserId}`);
      return {
        id: bypassUserId,
        email: 'test@example.com',
        organization_id: 'test-org-id',
        is_admin: true,
        is_agent: true,
        first_name: 'Test',
        last_name: 'User',
        is_active: true,
        organization_name: 'Test Organization'
      };
    }
    
    // Check if this is a public endpoint that should bypass auth
    const url = new URL(request.url);
    const pathname = url.pathname;
    
    // Log all requested pathnames for debugging
    logger.info(`Auth check for pathname: ${pathname}`);
    
    // Skip auth for subscription/checkout endpoint
    if (pathname === '/api/subscription/checkout') {
      logger.info('Skipping auth check for subscription/checkout endpoint');
      return { skip_auth: true }; // Return a dummy user that won't trigger auth failures
    }
    
    // Skip auth for all self-service endpoints
    if (pathname.startsWith('/api/self-service/')) {
      logger.info(`Skipping auth check for self-service endpoint: ${pathname}`);
      
      return { skip_auth: true }; // Return a dummy user that won't trigger auth failures
    }

    // Test both regex patterns for debugging
    const comparePathTest1 = /^\/compare\/[^\/]+$/.test(pathname);
    const comparePathTest2 = pathname.startsWith('/compare/');
    
    logger.info(`Compare path tests: regex=${comparePathTest1}, startsWith=${comparePathTest2} for ${pathname}`);

    // Skip auth for compare pages with path parameters
    if (pathname.startsWith('/compare/')) {
      logger.info(`Skipping auth check for compare path parameter endpoint: ${pathname}`);
      return { skip_auth: true }; // Return a dummy user that won't trigger auth failures
    }
    
    // Skip auth check for static files
    if (
      pathname.endsWith('.js') || 
      pathname.endsWith('.css') || 
      pathname.endsWith('.png') || 
      pathname.endsWith('.jpg') || 
      pathname.endsWith('.svg') || 
      pathname.endsWith('.ico') ||
      pathname.endsWith('.ttf') ||
      pathname.endsWith('.woff') ||
      pathname.endsWith('.woff2')
    ) {
      logger.info(`Skipping auth check for static file: ${pathname}`);
      return { skip_auth: true };
    }

    const cookieHeader = request.headers.get('Cookie') || '';

    // Get session cookie
    const sessionId = cookieHeader.split('session=')[1]?.split(';')[0];
    
    if (!sessionId) {
      logger.warn('No session cookie found');
      return null;
    }

    // Initialize database
    const db = new Database();

    // Get session data
    const sessionResult = await db.fetchAll(
      'SELECT user_id FROM sessions WHERE id = ?',
      [sessionId]
    );

    if (!sessionResult || sessionResult.length === 0) {
      logger.warn(`No session found for ID: ${sessionId}`);
      return null;
    }

    const userId = sessionResult[0][0];

    // Updated query to use is_admin and is_agent
    const userResult = await db.fetchAll(
      `SELECT 
        u.id,
        u.email,
        u.organization_id,
        u.is_admin,
        u.is_agent,
        u.first_name,
        u.last_name,
        u.is_active,
        u.phone,
        u.signature,
        u.use_org_sender_details,
        u.booking_link,
        o.name as organization_name
       FROM users u
       JOIN organizations o ON u.organization_id = o.id 
       WHERE u.id = ?`,
      [userId]
    );

    if (!userResult || userResult.length === 0) {
      logger.warn('No user found for session');
      return null;
    }

    const user = {
      id: userResult[0][0],
      email: userResult[0][1],
      organization_id: userResult[0][2],
      is_admin: Boolean(userResult[0][3]),
      is_agent: Boolean(userResult[0][4]),
      first_name: userResult[0][5],
      last_name: userResult[0][6],
      is_active: userResult[0][7],
      phone: userResult[0][8],
      signature: userResult[0][9],
      use_org_sender_details: Boolean(userResult[0][10]),
      booking_link: userResult[0][11],
      organization_name: userResult[0][12]
    };

    return user;
  }
}

export async function validateSession(sessionId: string): Promise<User | null> {
  logger.info(`Validating session: ${sessionId}`);
  
  const db = new Database();

  // Get the session
  const session = await db.fetchOne<{
    id: string;
    user_id: number;
    expires_at: string;
  }>('SELECT * FROM sessions WHERE id = ?', [sessionId]);

  logger.info(`Session lookup result: ${session ? JSON.stringify(session) : 'not found'}`);

  if (!session) {
    logger.warn('No session found in database');
    return null;
  }

  // Check if session is expired
  const expiresAt = new Date(session.expires_at);
  const now = new Date();
  logger.info(`Session expires: ${expiresAt}, current time: ${now}`);

  if (expiresAt < now) {
    logger.warn('Session is expired');
    await db.execute('DELETE FROM sessions WHERE id = ?', [sessionId]);
    return null;
  }

  // Get the user associated with this session with updated columns
  const user = await db.fetchOne<User>(
    `SELECT 
      u.id,
      u.email,
      u.organization_id,
      u.is_admin,
      u.is_agent,
      u.first_name,
      u.last_name,
      u.is_active,
      u.phone,
      o.name as organization_name 
     FROM users u
     JOIN organizations o ON u.organization_id = o.id 
     WHERE u.id = ?`,
    [session.user_id]
  );

  logger.info(`User lookup result: ${user ? JSON.stringify(user) : 'not found'}`);

  return user;
}

export function generateToken(): string {
  return crypto.randomBytes(32).toString('hex');
}

export async function getUserFromSession(request: Request) {
  try {
    // Check for X-Bypass-Auth header
    const bypassAuth = request.headers.get('X-Bypass-Auth');
    if (bypassAuth) {
      logger.info(`Bypassing auth check due to X-Bypass-Auth header`);
      return { 
        skip_auth: true,
        id: 1,
        email: 'bypass@example.com',
        organization_id: 1,
        is_admin: true,
        is_agent: true,
        first_name: 'Auth',
        last_name: 'Bypass',
        is_active: true,
        organization_name: 'Bypass Org'
      };
    }
    
    // Check if this is a public endpoint that should bypass auth
    const url = new URL(request.url);
    const pathname = url.pathname;
    
    // Log all requested pathnames for debugging
    logger.info(`Auth check for pathname: ${pathname}`);
    
    // COMPREHENSIVE SPA ROUTES CHECK - any frontend route should bypass auth
    // Check for non-API paths with no file extension (SPA routes)
    if (!pathname.startsWith('/api/') && 
        !pathname.includes('.')) {
      logger.info(`[AUTH BYPASS] SPA route detected (non-API, no extension): ${pathname}`);
      return { 
        skip_auth: true,
        id: 1,
        email: 'bypass@example.com',
        organization_id: 1,
        is_admin: true,
        is_agent: true,
        first_name: 'Auth',
        last_name: 'Bypass',
        is_active: true,
        organization_name: 'Bypass Org'
      };
    }
    
    // Explicit checks for known SPA routes
    if (pathname.startsWith('/compare/') || 
        pathname.startsWith('/quote/') || 
        pathname.startsWith('/eligibility') ||
        pathname.startsWith('/schedule')) {
      logger.info(`[AUTH BYPASS] Known SPA route detected: ${pathname}`);
      return { 
        skip_auth: true,
        id: 1,
        email: 'bypass@example.com',
        organization_id: 1,
        is_admin: true,
        is_agent: true,
        first_name: 'Auth',
        last_name: 'Bypass',
        is_active: true,
        organization_name: 'Bypass Org'
      };
    }
    
    // Skip auth for subscription/checkout endpoint
    if (pathname === '/api/subscription/checkout') {
      logger.info('Skipping auth check for subscription/checkout endpoint');
      return { 
        skip_auth: true,
        id: 1,
        email: 'bypass@example.com',
        organization_id: 1,
        is_admin: true,
        is_agent: true,
        first_name: 'Auth',
        last_name: 'Bypass',
        is_active: true,
        organization_name: 'Bypass Org'
      };
    }
    
    // Skip auth for all self-service endpoints
    if (pathname.startsWith('/api/self-service/')) {
      logger.info(`Skipping auth check for self-service endpoint: ${pathname}`);
      return { 
        skip_auth: true,
        id: 1,
        email: 'bypass@example.com',
        organization_id: 1,
        is_admin: true,
        is_agent: true,
        first_name: 'Auth',
        last_name: 'Bypass',
        is_active: true,
        organization_name: 'Bypass Org'
      };
    }

    // Skip auth for subscription/pricing endpoint
    if (pathname === '/api/subscription/pricing') {
      logger.info('Skipping auth check for subscription/pricing endpoint');
      return { 
        skip_auth: true,
      };  
    }
    
    // Skip auth check for static files
    if (
      pathname.endsWith('.js') || 
      pathname.endsWith('.css') || 
      pathname.endsWith('.png') || 
      pathname.endsWith('.jpg') || 
      pathname.endsWith('.svg') || 
      pathname.endsWith('.ico') ||
      pathname.endsWith('.ttf') ||
      pathname.endsWith('.woff') ||
      pathname.endsWith('.woff2')
    ) {
      logger.info(`Skipping auth check for static file: ${pathname}`);
      return { 
        skip_auth: true,
        id: 1,
        email: 'bypass@example.com',
        organization_id: 1,
        is_admin: true,
        is_agent: true,
        first_name: 'Auth',
        last_name: 'Bypass',
        is_active: true,
        organization_name: 'Bypass Org'
      };
    }

    const cookieHeader = request.headers.get('Cookie') || '';

    // Get session cookie
    const sessionId = cookieHeader.split('session=')[1]?.split(';')[0];
    
    if (!sessionId) {
      logger.warn('No session cookie found');
      return null;
    }

    // Initialize database
    const db = new Database();

    // Get session data
    const sessionResult = await db.fetchAll(
      'SELECT user_id FROM sessions WHERE id = ?',
      [sessionId]
    );

    if (!sessionResult || sessionResult.length === 0) {
      logger.warn(`No session found for ID: ${sessionId}`);
      return null;
    }

    const userId = sessionResult[0][0];

    // Updated query to use is_admin and is_agent
    const userResult = await db.fetchAll(
      `SELECT 
        u.id,
        u.email,
        u.organization_id,
        u.is_admin,
        u.is_agent,
        u.first_name,
        u.last_name,
        u.is_active,
        u.phone,
        u.signature,
        u.use_org_sender_details,
        u.booking_link,
        o.name as organization_name
       FROM users u
       JOIN organizations o ON u.organization_id = o.id 
       WHERE u.id = ?`,
      [userId]
    );

    if (!userResult || userResult.length === 0) {
      logger.warn('No user found for session');
      return null;
    }

    const user = {
      id: userResult[0][0],
      email: userResult[0][1],
      organization_id: userResult[0][2],
      is_admin: Boolean(userResult[0][3]),
      is_agent: Boolean(userResult[0][4]),
      first_name: userResult[0][5],
      last_name: userResult[0][6],
      is_active: userResult[0][7],
      phone: userResult[0][8],
      signature: userResult[0][9],
      use_org_sender_details: Boolean(userResult[0][10]),
      booking_link: userResult[0][11],
      organization_name: userResult[0][12]
    };

    return user;

  } catch (error) {
    logger.error(`Error getting user from session: ${error}`);
    return null;
  }
} 