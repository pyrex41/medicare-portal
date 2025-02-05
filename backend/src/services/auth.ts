import crypto from 'crypto';
import { Database } from '../database';
import { logger } from '../logger';

const MAGIC_LINK_EXPIRY = 30 * 60 * 1000; // 30 minutes
const algorithm = 'aes-256-gcm';
const IV_LENGTH = 12;

interface MagicLinkPayload {
  email: string;
  organizationSlug: string;
  expiresAt: number;
}

export class AuthService {
  private encryptionKey: Buffer;
  
  constructor(
    private db: Database,
    private baseUrl: string
  ) {
    // Generate encryption key from environment secret
    this.encryptionKey = crypto.scryptSync(
      process.env.MAGIC_LINK_SECRET || 'dev-secret-key',
      'salt',
      32
    );
  }

  async createMagicLink(email: string, organizationSlug: string): Promise<string> {
    // Verify organization exists
    const org = await this.db.fetchOne(
      'SELECT id FROM organizations WHERE slug = ?',
      [organizationSlug]
    );
    
    if (!org) {
      throw new Error('Organization not found');
    }

    const payload: MagicLinkPayload = {
      email,
      organizationSlug,
      expiresAt: Date.now() + MAGIC_LINK_EXPIRY
    };

    // Generate encrypted token
    const iv = crypto.randomBytes(IV_LENGTH);
    const cipher = crypto.createCipheriv(algorithm, this.encryptionKey, iv);
    let encrypted = cipher.update(JSON.stringify(payload), 'utf8', 'hex');
    encrypted += cipher.final('hex');
    const authTag = cipher.getAuthTag();

    const token = `${iv.toString('hex')}:${authTag.toString('hex')}:${encrypted}`;

    // Store magic link in database
    await this.db.execute(
      `INSERT INTO magic_links (token, email, organization_id, expires_at)
       VALUES (?, ?, ?, datetime(?))`,
      [token, email, org.id, new Date(payload.expiresAt).toISOString()]
    );

    // Update URL to include redirect
    return `${this.baseUrl}/auth/verify/${organizationSlug}/${encodeURIComponent(token)}?redirect=/templanding`;
  }

  async verifyMagicLink(token: string, organizationSlug: string): Promise<{
    valid: boolean;
    email?: string;
    organizationId?: number;
    redirectUrl?: string;
  }> {
    try {
      // First check database record
      const link = await this.db.fetchOne(
        `SELECT ml.*, o.id as org_id 
         FROM magic_links ml
         JOIN organizations o ON o.id = ml.organization_id
         WHERE ml.token = ? 
         AND o.slug = ?
         AND ml.used_at IS NULL
         AND ml.expires_at > datetime('now')`,
        [token, organizationSlug]
      );

      if (!link) {
        return { valid: false };
      }

      // Verify token encryption
      const [ivHex, authTagHex, encryptedText] = token.split(':');
      const iv = Buffer.from(ivHex, 'hex');
      const authTag = Buffer.from(authTagHex, 'hex');
      
      const decipher = crypto.createDecipheriv(algorithm, this.encryptionKey, iv);
      decipher.setAuthTag(authTag);
      
      let decrypted = decipher.update(encryptedText, 'hex', 'utf8');
      decrypted += decipher.final('utf8');
      
      const payload: MagicLinkPayload = JSON.parse(decrypted);

      // Mark link as used
      await this.db.execute(
        'UPDATE magic_links SET used_at = datetime("now") WHERE token = ?',
        [token]
      );

      return {
        valid: true,
        email: payload.email,
        organizationId: link.org_id,
        redirectUrl: '/templanding' // Add default redirect URL
      };
    } catch (error) {
      logger.error('Magic link verification failed:', error);
      return { valid: false };
    }
  }
} 