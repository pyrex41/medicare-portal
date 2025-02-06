import crypto from 'crypto';
import { logger } from '../logger';
import { db } from '../database';

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
}

export class AuthService {
  constructor(private baseUrl: string) {
    // Ensure baseUrl doesn't end with a slash
    this.baseUrl = baseUrl.replace(/\/$/, '');
  }

  async createMagicLink(email: string, organizationSlug: string): Promise<string> {
    const payload: MagicLinkPayload = {
      email,
      organizationSlug,
      expiresAt: Date.now() + (30 * 60 * 1000), // 30 minutes
      redirectUrl: '/templanding'  // Make sure this matches exactly
    };

    logger.info(`Creating magic link with payload: ${JSON.stringify(payload)}`);
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
} 