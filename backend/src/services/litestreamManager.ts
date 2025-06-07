import { spawn, ChildProcess } from 'child_process';
import { logger } from '../logger';
import fs from 'fs/promises';

interface ActiveDatabase {
  orgId: string;
  localPath: string;
  process: ChildProcess;
  lastAccessed: number;
}

class LitestreamManager {
  private static instance: LitestreamManager;
  private activeDatabases: Map<string, ActiveDatabase> = new Map();
  private readonly GCS_BUCKET_NAME = process.env.GCS_BUCKET_NAME || 'replit-object-storage';
  private readonly IDLE_TIMEOUT = 5 * 60 * 1000; // 5 minutes
  private readonly CLEANUP_INTERVAL = 60 * 1000; // 1 minute
  private cleanupInterval: ReturnType<typeof setInterval>;

  private constructor() {
    // Periodically clean up idle database processes
    this.cleanupInterval = setInterval(() => this.cleanupIdleProcesses(), this.CLEANUP_INTERVAL);
    
    // Handle graceful shutdown
    process.on('SIGTERM', () => this.shutdown());
    process.on('SIGINT', () => this.shutdown());
    
    logger.info('[LitestreamManager] Initialized with GCS bucket:', this.GCS_BUCKET_NAME);
  }

  public static getInstance(): LitestreamManager {
    if (!LitestreamManager.instance) {
      LitestreamManager.instance = new LitestreamManager();
    }
    return LitestreamManager.instance;
  }

  public async getDbPath(orgId: string): Promise<string> {
    const localPath = `/tmp/org-${orgId}.db`;

    if (this.activeDatabases.has(orgId)) {
      // If it's already active, just update the timestamp and return the path
      this.activeDatabases.get(orgId)!.lastAccessed = Date.now();
      logger.info(`[Manager|Org ${orgId}] DB already hot. Returning path: ${localPath}`);
      return localPath;
    }

    // --- This is the "hot-load" logic ---
    logger.info(`[Manager|Org ${orgId}] DB is cold. Activating now...`);

    // 1. Restore the database from GCS
    try {
      await this.runLitestreamCommand(`restore -o ${localPath} gcs://${this.GCS_BUCKET_NAME}/litestream-replicas/${orgId}`);
      logger.info(`[Manager|Org ${orgId}] Restore complete.`);
    } catch (error: any) {
      if (error.stderr && error.stderr.includes("no snapshots found")) {
        logger.warn(`[Manager|Org ${orgId}] No replica found in GCS. A new empty DB will be created at ${localPath}`);
        // Litestream restore fails if no replica exists. The file won't be created.
        // We just need to let the Database constructor handle creating the empty file.
      } else {
        logger.error(`[Manager|Org ${orgId}] Restore failed: ${error.message}`);
        throw error; // Rethrow other errors
      }
    }

    // 2. Start a dedicated 'litestream replicate' process for this DB
    const litestreamProcess = spawn('litestream', [
      'replicate',
      '-config', '/workspace/litestream-single-db.yml' // Template config
    ], {
      env: {
        ...process.env,
        // Pass the orgId to the config file via environment variable
        ORG_ID: orgId,
        GCS_BUCKET_NAME: this.GCS_BUCKET_NAME
      }
    });

    litestreamProcess.stdout?.on('data', (data: Buffer) => {
      logger.info(`[Litestream|Org ${orgId}] ${data.toString().trim()}`);
    });
    
    litestreamProcess.stderr?.on('data', (data: Buffer) => {
      logger.error(`[Litestream|Org ${orgId}] ${data.toString().trim()}`);
    });

    litestreamProcess.on('exit', (code: number | null, signal: NodeJS.Signals | null) => {
      logger.info(`[Litestream|Org ${orgId}] Process exited with code ${code} and signal ${signal}`);
      // Remove from active databases if the process exits
      this.activeDatabases.delete(orgId);
    });

    litestreamProcess.on('error', (error: Error) => {
      logger.error(`[Litestream|Org ${orgId}] Process error: ${error.message}`);
      this.activeDatabases.delete(orgId);
    });

    // 3. Store the process and metadata
    this.activeDatabases.set(orgId, {
      orgId,
      localPath,
      process: litestreamProcess,
      lastAccessed: Date.now()
    });

    logger.info(`[Manager|Org ${orgId}] Replication process started with PID: ${litestreamProcess.pid}. Total active DBs: ${this.activeDatabases.size}`);

    return localPath;
  }

  private async runLitestreamCommand(command: string): Promise<{ stdout: string; stderr: string }> {
    return new Promise((resolve, reject) => {
      const [cmd, ...args] = command.split(' ');
      const childProcess = spawn('litestream', args, {
        env: {
          ...process.env,
          GCS_BUCKET_NAME: this.GCS_BUCKET_NAME
        }
      });
      
      let stdout = '';
      let stderr = '';
      
      childProcess.stdout?.on('data', (data: Buffer) => stdout += data);
      childProcess.stderr?.on('data', (data: Buffer) => stderr += data);
      
      childProcess.on('close', (code: number | null) => {
        if (code === 0) {
          resolve({ stdout, stderr });
        } else {
          const error = new Error(`Litestream command "${command}" failed with code ${code}: ${stderr}`);
          (error as any).stderr = stderr;
          reject(error);
        }
      });

      childProcess.on('error', (error: Error) => {
        reject(new Error(`Failed to spawn litestream: ${error.message}`));
      });
    });
  }

  private cleanupIdleProcesses() {
    const now = Date.now();

    logger.info(`[Manager] Running cleanup. Active DBs: ${this.activeDatabases.size}`);

    for (const [orgId, dbInfo] of this.activeDatabases.entries()) {
      if (now - dbInfo.lastAccessed > this.IDLE_TIMEOUT) {
        logger.info(`[Manager|Org ${orgId}] DB is idle. Shutting down replication and cleaning up cache.`);

        // Stop the Litestream process
        dbInfo.process.kill('SIGINT');

        // Remove from the active map
        this.activeDatabases.delete(orgId);

        // Delete the local file to save space
        fs.unlink(dbInfo.localPath).catch(err => {
          logger.error(`[Manager|Org ${orgId}] Failed to delete cached file ${dbInfo.localPath}: ${err}`);
        });
      }
    }
  }

  public shutdown() {
    logger.info('[Manager] Shutting down LitestreamManager');
    
    // Clear the cleanup interval
    clearInterval(this.cleanupInterval);
    
    // Stop all active processes
    for (const [orgId, dbInfo] of this.activeDatabases.entries()) {
      logger.info(`[Manager|Org ${orgId}] Stopping replication process`);
      dbInfo.process.kill('SIGINT');
    }
    
    // Clear the active databases map
    this.activeDatabases.clear();
  }

  // For debugging/monitoring
  public getActiveDatabase(orgId: string): ActiveDatabase | undefined {
    return this.activeDatabases.get(orgId);
  }

  public getActiveDatabaseCount(): number {
    return this.activeDatabases.size;
  }
}

export const litestreamManager = LitestreamManager.getInstance();