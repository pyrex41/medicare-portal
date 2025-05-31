import { readFileSync, writeFileSync, existsSync, mkdirSync } from 'fs';
import { join } from 'path';
import { logger } from '../logger';

// Type definitions for the result types
type OkResult<T> = { ok: true; value: T };
type ErrResult = { ok: false; error: any };
type Result<T> = OkResult<T> | ErrResult;

/**
 * Database client that uses @replit/database on Replit or falls back to JSON mock locally
 * API matches @replit/database exactly
 */
export default class Database {
  private client: any;
  private isMock: boolean = false;

  constructor(dbUrl?: string) {
    if (process.env.REPLIT_DB_URL || dbUrl) {
      try {
        // Try to use the real Replit database
        const ReplitDatabase = require('@replit/database').default;
        this.client = new ReplitDatabase(dbUrl);
        logger.info('Using Replit database');
      } catch (error) {
        logger.warn('Failed to load @replit/database, falling back to JSON mock');
        this.client = new MockReplitDatabase();
        this.isMock = true;
      }
    } else {
      this.client = new MockReplitDatabase();
      this.isMock = true;
      logger.info('Using local JSON mock database');
    }
  }

  async set(key: string, value: any): Promise<void> {
    if (this.isMock) {
      return this.client.set(key, value);
    }
    return this.client.set(key, value);
  }

  async get(key: string, options?: { raw?: boolean }): Promise<Result<any>> {
    if (this.isMock) {
      return this.client.get(key, options);
    }
    return this.client.get(key, options);
  }

  async delete(key: string): Promise<Result<null>> {
    if (this.isMock) {
      return this.client.delete(key);
    }
    return this.client.delete(key);
  }

  async list(prefix?: string): Promise<Result<string[]>> {
    if (this.isMock) {
      return this.client.list(prefix);
    }
    return this.client.list(prefix);
  }

  async empty(): Promise<void> {
    if (this.isMock) {
      return this.client.empty();
    }
    return this.client.empty();
  }

  async getAll(options?: { raw?: boolean }): Promise<Result<Record<string, any>>> {
    if (this.isMock) {
      return this.client.getAll(options);
    }
    return this.client.getAll(options);
  }

  async setMultiple(obj: Record<string, any>): Promise<void> {
    if (this.isMock) {
      return this.client.setMultiple(obj);
    }
    return this.client.setMultiple(obj);
  }

  async deleteMultiple(keys: string[]): Promise<void> {
    if (this.isMock) {
      return this.client.deleteMultiple(keys);
    }
    return this.client.deleteMultiple(keys);
  }
}

/**
 * Mock implementation that exactly matches @replit/database API
 */
class MockReplitDatabase {
  private data: Record<string, any> = {};
  private filePath: string;

  constructor(filename: string = 'replit_mock_db.json') {
    this.filePath = join(process.cwd(), 'data', filename);
    this.loadDb();
  }

  private loadDb() {
    if (existsSync(this.filePath)) {
      try {
        this.data = JSON.parse(readFileSync(this.filePath, 'utf-8'));
      } catch (error) {
        logger.error('Error loading mock database:', error);
        this.data = {};
      }
    }
  }

  private saveDb() {
    // Ensure data directory exists
    const dataDir = join(process.cwd(), 'data');
    if (!existsSync(dataDir)) {
      mkdirSync(dataDir, { recursive: true });
    }
    
    writeFileSync(this.filePath, JSON.stringify(this.data, null, 2));
  }

  async set(key: string, value: any): Promise<void> {
    this.data[key] = value;
    this.saveDb();
  }

  async get(key: string, options?: { raw?: boolean }): Promise<Result<any>> {
    const value = this.data[key];
    if (value === undefined) {
      return { ok: true, value: null };
    }
    
    if (options?.raw && typeof value !== 'string') {
      return { ok: true, value: JSON.stringify(value) };
    }
    
    return { ok: true, value };
  }

  async delete(key: string): Promise<Result<null>> {
    delete this.data[key];
    this.saveDb();
    return { ok: true, value: null };
  }

  async list(prefix?: string): Promise<Result<string[]>> {
    let keys = Object.keys(this.data);
    
    if (prefix) {
      keys = keys.filter(key => key.startsWith(prefix));
    }
    
    return { ok: true, value: keys };
  }

  async empty(): Promise<void> {
    this.data = {};
    this.saveDb();
  }

  async getAll(options?: { raw?: boolean }): Promise<Result<Record<string, any>>> {
    if (options?.raw) {
      const rawData: Record<string, string> = {};
      for (const [key, value] of Object.entries(this.data)) {
        rawData[key] = typeof value === 'string' ? value : JSON.stringify(value);
      }
      return { ok: true, value: rawData };
    }
    
    return { ok: true, value: { ...this.data } };
  }

  async setMultiple(obj: Record<string, any>): Promise<void> {
    Object.assign(this.data, obj);
    this.saveDb();
  }

  async deleteMultiple(keys: string[]): Promise<void> {
    for (const key of keys) {
      delete this.data[key];
    }
    this.saveDb();
  }
}