import { spawn, ChildProcess } from 'child_process';
import { logger } from '../logger';
import { unlink } from 'fs/promises';

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
    
    logger.info(`[LitestreamManager] Initialized with GCS bucket: ${this.GCS_BUCKET_NAME}`);
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
    let isNewOrganization = false;
    try {
      await this.runLitestreamCommand(`restore -o ${localPath} gcs://${this.GCS_BUCKET_NAME}/litestream-replicas/${orgId}`);
      logger.info(`[Manager|Org ${orgId}] Restore complete from existing replica.`);
    } catch (error: any) {
      if (error.stderr && error.stderr.includes("no snapshots found")) {
        logger.info(`[Manager|Org ${orgId}] No replica found in GCS - this is a new organization. Empty DB will be created.`);
        isNewOrganization = true;
        // This is expected for new organizations - continue with empty database
      } else if (error.message.includes("Litestream binary not found")) {
        logger.error(`[Manager|Org ${orgId}] Litestream binary not available: ${error.message}`);
        throw new Error(`Database replication unavailable: Litestream not installed. Please contact support.`);
      } else if (error.stderr && (error.stderr.includes("AccessDenied") || error.stderr.includes("Forbidden"))) {
        logger.error(`[Manager|Org ${orgId}] GCS access denied: ${error.stderr}`);
        throw new Error(`Database access error: Unable to connect to storage. Please contact support.`);
      } else if (error.stderr && error.stderr.includes("timeout")) {
        logger.error(`[Manager|Org ${orgId}] Network timeout during restore: ${error.stderr}`);
        throw new Error(`Database restore timeout: Please try again in a moment.`);
      } else {
        logger.error(`[Manager|Org ${orgId}] Unexpected restore failure: ${error.message}`);
        logger.error(`[Manager|Org ${orgId}] Error details: ${error.stderr || 'No stderr available'}`);
        throw new Error(`Database restore failed: ${error.message}. Please contact support if this persists.`);
      }
    }

    // 2. Start a dedicated 'litestream replicate' process for this DB
    let litestreamProcess: ChildProcess;
    try {
      litestreamProcess = spawn('litestream', [
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

      // Handle process spawn errors
      litestreamProcess.on('error', (error: Error) => {
        logger.error(`[Manager|Org ${orgId}] Failed to start replication process: ${error.message}`);
        this.activeDatabases.delete(orgId);
        if ((error as any).code === 'ENOENT') {
          throw new Error(`Litestream binary not found during replication start`);
        }
      });

    } catch (error: any) {
      logger.error(`[Manager|Org ${orgId}] Error starting litestream process: ${error.message}`);
      throw new Error(`Failed to start database replication: ${error.message}`);
    }

    litestreamProcess.stdout?.on('data', (data: Buffer) => {
      logger.info(`[Litestream|Org ${orgId}] ${data.toString().trim()}`);
    });
    
    litestreamProcess.stderr?.on('data', (data: Buffer) => {
      const message = data.toString().trim();
      // Don't log as error for initial replica creation - this is expected for new orgs
      if (isNewOrganization && message.includes('creating replica')) {
        logger.info(`[Litestream|Org ${orgId}] ${message}`);
      } else {
        logger.error(`[Litestream|Org ${orgId}] ${message}`);
      }
    });

    litestreamProcess.on('exit', (code: number | null, signal: NodeJS.Signals | null) => {
      logger.info(`[Litestream|Org ${orgId}] Process exited with code ${code} and signal ${signal}`);
      // Remove from active databases if the process exits
      this.activeDatabases.delete(orgId);
    });

    // 3. Store the process and metadata
    this.activeDatabases.set(orgId, {
      orgId,
      localPath,
      process: litestreamProcess,
      lastAccessed: Date.now()
    });

    if (isNewOrganization) {
      logger.info(`[Manager|Org ${orgId}] New organization setup complete. Replication process started with PID: ${litestreamProcess.pid}. Will create initial replica after first database writes.`);
    } else {
      logger.info(`[Manager|Org ${orgId}] Existing organization replication restored. Process PID: ${litestreamProcess.pid}. Total active DBs: ${this.activeDatabases.size}`);
    }

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
          (error as any).exitCode = code;
          reject(error);
        }
      });

      // Handle process spawn errors (e.g., binary not found)
      childProcess.on('error', (error: Error) => {
        if ((error as any).code === 'ENOENT') {
          reject(new Error(`Litestream binary not found. Please ensure litestream is installed and in your PATH. Original error: ${error.message}`));
        } else {
          reject(new Error(`Failed to spawn litestream process: ${error.message}`));
        }
      });
    });
  }

  private cleanupIdleProcesses() {
    const now = Date.now();

    logger.info(`[Manager] Running cleanup. Active DBs: ${this.activeDatabases.size}`);

    // Convert Map entries to array for iteration compatibility
    const entries = Array.from(this.activeDatabases.entries());
    for (const [orgId, dbInfo] of entries) {
      if (now - dbInfo.lastAccessed > this.IDLE_TIMEOUT) {
        logger.info(`[Manager|Org ${orgId}] DB is idle. Shutting down replication and cleaning up cache.`);

        // Stop the Litestream process
        dbInfo.process.kill('SIGINT');

        // Remove from the active map
        this.activeDatabases.delete(orgId);

        // Delete the local file to save space
        unlink(dbInfo.localPath).catch(err => {
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
    const entries = Array.from(this.activeDatabases.entries());
    for (const [orgId, dbInfo] of entries) {
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