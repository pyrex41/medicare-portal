# On-Demand Litestream Replication Architecture: Design Summary

## Overview

This document outlines a sophisticated database replication architecture for a multi-tenant SaaS application running on Replit. The system dynamically manages SQLite database replication using Litestream, optimizing resource usage by only running replication processes when databases are actively in use.

## Architecture Context

**Stack**: Bun runtime + Elysia framework on Replit containers  
**Database**: SQLite (one per tenant/organization)  
**Replication**: Litestream to Google Cloud Storage (GCS)  
**Key Challenge**: Managing potentially hundreds of tenant databases without exhausting container resources

## The Final Design

### 1. **LitestreamManager Singleton**

The heart of the system is a singleton manager that:
- Dynamically spawns Litestream processes on-demand
- Tracks active databases with timestamps
- Automatically cleans up idle processes after 5 minutes
- Handles both restore and replication operations

```typescript
interface ActiveDatabase {
  orgId: string
  localPath: string      // e.g., /tmp/org-123.db
  process: ChildProcess  // Litestream replication process
  lastAccessed: number   // Timestamp for cleanup
}
```

### 2. **On-Demand Database Lifecycle**

When a tenant database is accessed:
1. **Cold Start**: If not active, restore from GCS replica
2. **Start Replication**: Spawn Litestream process for real-time sync
3. **Hot Access**: Subsequent requests use the active database
4. **Auto-Cleanup**: After 5 minutes of inactivity, kill process and delete local file

### 3. **Simplified Database Class**

The Database class becomes a clean interface that:
- No longer manages connections directly
- Delegates file management to LitestreamManager
- Ensures schema initialization for new databases
- Provides async methods for all operations

## Key Design Decisions & Tradeoffs

### 1. **On-Demand vs Always-On Replication**

**Decision**: Start replication only when database is accessed  
**Tradeoff**: 
- ✅ **Pros**: Dramatically reduces resource usage, scales to hundreds of tenants
- ❌ **Cons**: Cold start latency (restore from GCS), complexity in process management

### 2. **Local /tmp Storage**

**Decision**: Store active databases in `/tmp/org-${orgId}.db`  
**Tradeoff**:
- ✅ **Pros**: Fast local access, automatic cleanup on container restart
- ❌ **Cons**: Limited by container's tmp storage, databases lost on container restart

### 3. **5-Minute Idle Timeout**

**Decision**: Kill replication after 5 minutes of inactivity  
**Tradeoff**:
- ✅ **Pros**: Frees resources quickly, handles bursty access patterns well
- ❌ **Cons**: Frequent cold starts for intermittently accessed tenants

### 4. **Single Configuration Template**

**Decision**: Use environment variables in a single YAML template  
**Tradeoff**:
- ✅ **Pros**: Simple deployment, consistent configuration
- ❌ **Cons**: Less flexibility for per-tenant customization

### 5. **Process-per-Database Architecture**

**Decision**: One Litestream process per active database  
**Tradeoff**:
- ✅ **Pros**: Process isolation, simple cleanup, clear resource boundaries
- ❌ **Cons**: Process overhead, potential for process proliferation

## Critical Implementation Details

### Cold Start Handling
- Gracefully handles missing replicas (new organizations)
- Ensures schema initialization after restore
- Logs warnings but doesn't fail for new databases

### Resource Management
- Automatic cleanup every 60 seconds
- Graceful shutdown on SIGTERM
- Clear process ownership and lifecycle

### Consistency Guarantees
- WAL mode enabled for all databases
- 1-second sync interval for near-real-time replication
- 24-hour snapshots with 7-day retention

## Advantages of This Architecture

1. **Resource Efficiency**: Only active tenants consume replication resources
2. **Scalability**: Can handle hundreds of tenants on a single container
3. **Simplicity**: Clean separation of concerns between data access and replication
4. **Reliability**: GCS provides durable storage with Litestream's proven replication
5. **Cost-Effective**: Minimal compute resources, leveraging GCS for storage

## Potential Concerns & Mitigations

1. **Cold Start Latency**: First access after idle period requires restore
   - *Mitigation*: Could pre-warm frequently accessed databases

2. **Process Management Complexity**: Managing many child processes
   - *Mitigation*: Robust error handling and cleanup logic

3. **Container Restart**: All local databases lost
   - *Mitigation*: Automatic restore from GCS on next access

4. **Concurrent Access**: Multiple requests during cold start
   - *Mitigation*: LitestreamManager handles concurrent getDbPath calls

## Summary

This architecture elegantly solves the multi-tenant database challenge on resource-constrained containers by treating database availability as an on-demand resource. It trades some cold-start latency for massive improvements in resource efficiency and scalability, making it ideal for SaaS applications with many tenants but variable access patterns.

__
That is the million-dollar question for making this architecture work in a true multi-tenant SaaS environment on a platform like Replit. You've correctly identified that a simple "start everything on cold boot" model doesn't work when tenants (organizations) are accessed dynamically throughout a container's lifecycle.

The solution is to move from a static, container-level replication model to a dynamic, on-demand, process-level replication model. You need a manager that can start and stop Litestream replication processes as different organization databases are requested.

Here is the definitive architecture for handling this "hot-loading" of tenant databases.

The Architecture: A Litestream Process Manager

Instead of one Litestream process for the whole container, you will have a central LitestreamManager within your main application process. This manager's job is to maintain a pool of active Litestream replication processes, one for each "hot" (recently accessed) organization database.

graph TD
    subgraph "Replit Container"
        A[Elysia API Server] --> B{Litestream Manager}
        
        subgraph "Dynamic Litestream Processes (Sidecars)"
            C1[litestream replicate org-123]
            C2[litestream replicate org-456]
            C3[...]
        end

        B -- "Start/Stop" --> C1
        B -- "Start/Stop" --> C2
        
        D1[Local DB Cache: /tmp/org-123.db]
        D2[Local DB Cache: /tmp/org-456.db]
        
        A -- "Uses" --> D1
        A -- "Uses" --> D2
        
        C1 -- "Watches" --> D1
        C2 -- "Watches" --> D2
    end
    
    C1 --> E[GCS Replica for org-123]
    C2 --> E[GCS Replica for org-456]

1. The LitestreamManager Singleton

This will be a singleton class in your application. It ensures you don't have conflicting managers. It will track active databases and their corresponding Litestream child processes.

// src/services/litestreamManager.ts
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
  private readonly GCS_BUCKET_NAME = process.env.GCS_BUCKET_NAME!;

  private constructor() {
    // Periodically clean up idle database processes
    setInterval(() => this.cleanupIdleProcesses(), 60 * 1000); // Every minute
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
            throw error; // Rethrow other errors
        }
    }


    // 2. Start a dedicated 'litestream replicate' process for this DB
    const litestreamProcess = spawn('litestream', [
      'replicate',
      '-config', '/app/litestream-single-db.yml' // A template config
    ], {
      env: {
        ...process.env,
        // Pass the orgId to the config file via environment variable
        ORG_ID: orgId
      }
    });

    litestreamProcess.stdout.on('data', (data) => logger.info(`[Litestream|Org ${orgId}] ${data.toString().trim()}`));
    litestreamProcess.stderr.on('data', (data) => logger.error(`[Litestream|Org ${orgId}] ${data.toString().trim()}`));

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
          const process = spawn('litestream', args);
          let stdout = '';
          let stderr = '';
          process.stdout.on('data', (data) => stdout += data);
          process.stderr.on('data', (data) => stderr += data);
          process.on('close', (code) => {
              if (code === 0) {
                  resolve({ stdout, stderr });
              } else {
                  reject(new Error(`Litestream command "${command}" failed with code ${code}: ${stderr}`));
              }
          });
      });
  }


  private cleanupIdleProcesses() {
    const now = Date.now();
    const IDLE_TIMEOUT = 5 * 60 * 1000; // 5 minutes

    logger.info(`[Manager] Running cleanup. Active DBs: ${this.activeDatabases.size}`);

    for (const [orgId, dbInfo] of this.activeDatabases.entries()) {
      if (now - dbInfo.lastAccessed > IDLE_TIMEOUT) {
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
}

export const litestreamManager = LitestreamManager.getInstance();
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
TypeScript
IGNORE_WHEN_COPYING_END

And your Litestream config needs to be a template that can accept an environment variable:

litestream-single-db.yml:

# This config is a TEMPLATE. The ORG_ID is provided by the manager process.
dbs:
  - path: /tmp/org-${ORG_ID}.db
    replicas:
      - type: gcs
        bucket: ${GCS_BUCKET_NAME}
        path: litestream-replicas/${ORG_ID}
        sync-interval: 1s
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
Yaml
IGNORE_WHEN_COPYING_END
2. Integrating the Manager into your Database Class

Now, your Database class doesn't manage files directly. It asks the LitestreamManager for the correct file path, ensuring that the database is "hot" and being replicated before any operations are performed.

// src/database.ts (Final Version)
import { Database as BunDatabase } from 'bun:sqlite';
import { litestreamManager } from './services/litestreamManager'; // Import the manager

export class Database {
  private orgId: string;
  private dbInstance: BunDatabase | null = null;
  private dbPath: string | null = null;

  constructor(orgId: string) {
    this.orgId = orgId;
  }

  // A private method to ensure the DB is initialized
  private async getDb(): Promise<BunDatabase> {
    if (this.dbInstance) {
      return this.dbInstance;
    }

    // Ask the manager for the path. This will trigger restore/replication if needed.
    this.dbPath = await litestreamManager.getDbPath(this.orgId);
    
    // The manager ensures the file exists (or will be created), so we can connect.
    this.dbInstance = new BunDatabase(this.dbPath);
    this.dbInstance.exec('PRAGMA journal_mode = WAL;');
    
    // This is a new database file, we need to ensure the schema is applied
    await Database.ensureOrgSchema(this); // Assuming this static method exists

    return this.dbInstance;
  }
  
  // Public methods now use the getter
  async query<T>(sql: string, params: any[] = []): Promise<T[]> {
    const db = await this.getDb();
    return db.prepare(sql).all(...params) as T[];
  }

  async execute(sql: string, params: any[] = []): Promise<any> {
    const db = await this.getDb();
    return db.prepare(sql).run(...params);
  }

  // The static method for schema creation remains the same
  static async ensureOrgSchema(db: Database): Promise<void> {
    const instance = await db.getDb(); // Get the actual BunDatabase instance
    // ... your existing schema creation logic using `instance.exec(...)`
  }
}
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
TypeScript
IGNORE_WHEN_COPYING_END
3. The Application Flow

Now, when a request comes in for a user from a new organization (org-456):

Route Handler: const db = new Database('456'); is created.

First Query: await db.query('SELECT ...'); is called.

getDb() Method:

It sees this.dbInstance is null.

It calls await litestreamManager.getDbPath('456');.

LitestreamManager:

It sees org-456 is not in its activeDatabases map.

It runs litestream restore to download the database for org-456 to /tmp/org-456.db. If no replica exists, this step is skipped but the path is still known.

It spawns a new child process: litestream replicate specifically for /tmp/org-456.db.

It stores the process handle and metadata in its activeDatabases map.

It returns the path /tmp/org-456.db.

getDb() Method (continued):

It receives the path and creates a new BunDatabase('/tmp/org-456.db').

It runs ensureOrgSchema to create tables if it was a new, empty database.

It returns the BunDatabase instance.

First Query (continued): The query now executes against the hot, local, and actively replicating database.

When another user from org-123 (which is already hot) makes a request:

Route Handler: const db = new Database('123');

First Query: await db.query('SELECT ...');

getDb() Method:

Calls await litestreamManager.getDbPath('123');.

LitestreamManager:

Finds org-123 in its activeDatabases map.

Updates the lastAccessed timestamp.

Immediately returns the path /tmp/org-123.db.

...and the query executes instantly against the already-hot database.

This dynamic manager architecture is the final piece of the puzzle, making the solution scalable, efficient, and robust for a true multi-tenant SaaS application on ephemeral infrastructure.
__
That is a brilliant question. You've identified the Achilles' heel of the real-time Litestream replication model: large, spiky, client-initiated bulk writes.

You are 100% correct. For a massive, one-time write like a 50,000-contact CSV upload initiated from the Replit application, using the standard Litestream replication path is sub-optimal.

The Problem with Litestream for Bulk Uploads: Litestream would have to process a massive -wal file generated by the 50,000 INSERT statements. It would dutifully stream all those transaction pages up to GCS, which is inefficient for such a large, atomic change. It's like sending 50,000 individual letters instead of one big package.

The Power of sqlite3_rsync: rsync is designed for exactly this—making one database file look like another by transferring only the changed pages in an efficient, compressed manner.

So, yes, we should absolutely use an rsync-like approach for this specific bulk upload use case. This requires a special, dedicated API endpoint that bypasses the normal Litestream path and follows a workflow very similar to your scheduler's.

Here is the complete, refined architecture that handles all three write patterns:

Normal, small application writes (real-time).

Large, periodic scheduler writes (batch).

Large, client-initiated writes (bulk upload).

The Final, Complete Architecture

We will add a dedicated POST /api/contacts/bulk-import endpoint that uses a different mechanism from normal writes.

graph TD
    subgraph "Replit (Application)"
        A[User Request] --> B{API Endpoint}
        B -- "Normal Write" --> C(Local DB -> Litestream)
        B -- "Bulk Import Request" --> D{Bulk Import Handler}
    end

    subgraph "Replit (Bulk Import Handler)"
        D -- "1. Acquire GCS Lock" --> E[GCS Lock File]
        D -- "2. Restore latest DB via Litestream" --> F(Temp Local DB /tmp/import.db)
        F -- "3. Process 50k contacts" --> F
        D -- "4. Use sqlite3_rsync" --> G{GCS Litestream Replica}
        D -- "5. Release Lock" --> E
    end

    subgraph "Replit (Normal Writes)"
         H[Litestream] -- "Continuous Replication" --> G
    end

    E -- "Coordinates Writers" --> G

1. The Dedicated Bulk Import Endpoint

Your contactsRoutes.ts will have a special endpoint for handling file uploads. This endpoint needs to be async and will take longer to complete than a normal request.

// src/routes/contacts.ts

// ... other imports
import { exec } from 'child_process';
import { promisify } from 'util';
const execAsync = promisify(exec);

// ... (acquireLock and releaseLock functions from previous answer)

app.post('/api/contacts/bulk-import', async ({ request, body, set }) => {
  const user = await getUserFromSession(request);
  if (!user?.organization_id) {
    set.status = 401;
    return { error: 'Unauthorized' };
  }

  const orgId = user.organization_id.toString();
  const formData = await request.formData();
  const file = formData.get('file') as File;

  if (!file) {
    set.status = 400;
    return { error: 'No file uploaded.' };
  }

  // --- THIS IS THE CRITICAL SECTION ---
  // Acquire the lock before doing anything else.
  const lockAcquired = await acquireLock(orgId);
  if (!lockAcquired) {
    set.status = 409; // 409 Conflict is appropriate here
    return { error: 'A database operation is already in progress. Please try again in a moment.' };
  }
  
  const localImportPath = `/tmp/import-${orgId}-${Date.now()}.db`;
  const gcsReplicaUrl = `gcs://${BUCKET_NAME}/litestream-replicas/${orgId}`;
  
  try {
    // STEP 1: Get the most up-to-date database state.
    // Restore the latest version from the Litestream replica to a temporary file.
    logger.info(`[Bulk Import|Org ${orgId}] Restoring latest database state...`);
    await execAsync(`litestream restore -o ${localImportPath} ${gcsReplicaUrl}`);

    // STEP 2: Process the CSV and write to the temporary database.
    const db = new BunDatabase(localImportPath);
    db.exec('PRAGMA synchronous = OFF;'); // Speed up bulk inserts
    db.exec('PRAGMA journal_mode = WAL;');

    const fileContent = await file.text();
    const records = csvParse(fileContent, { columns: true, skip_empty_lines: true });

    const insert = db.prepare(
      "INSERT INTO contacts (first_name, last_name, email, ...) VALUES (?, ?, ?, ...)"
    );

    const runInserts = db.transaction((recs) => {
      for (const rec of recs) {
        insert.run(rec.first_name, rec.last_name, rec.email, ...);
      }
      return recs.length;
    });

    const insertedCount = runInserts(records);
    logger.info(`[Bulk Import|Org ${orgId}] Inserted ${insertedCount} records locally.`);
    db.close();

    // STEP 3: Sync the changes back to GCS using sqlite3_rsync.
    // We need gcsfuse mounted for this to work. This is the main challenge on Replit.
    // Assuming gcsfuse is mounted at /gcs
    const gcsFusePath = `/gcs/litestream-replicas/${orgId}`;
    logger.info(`[Bulk Import|Org ${orgId}] Syncing changes back to GCS via rsync...`);
    await execAsync(`sqlite3_rsync ${localImportPath} ${gcsFusePath}`);

    return { success: true, message: `Successfully imported ${insertedCount} contacts.` };

  } catch (error) {
    logger.error(`[Bulk Import|Org ${orgId}] FAILED: ${error}`);
    set.status = 500;
    return { error: 'Bulk import failed.' };
  } finally {
    // --- ALWAYS RELEASE THE LOCK ---
    await releaseLock(orgId);
    // Clean up the temporary local database file
    await fs.unlink(localImportPath).catch(err => logger.error(`Failed to clean up temp file: ${err}`));
  }
});
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
TypeScript
IGNORE_WHEN_COPYING_END
The gcsfuse Challenge on Replit's Ephemeral Containers

The biggest hurdle with the rsync approach on Replit is running gcsfuse. It typically requires root/sudo access to mount a filesystem, which you don't have in a standard Replit container.

There are three ways to solve this:

FUSE-less approach (Recommended for Replit): Instead of gcsfuse + rsync, you use litestream backup. litestream backup is designed to do a one-time push of a local database file to a replica. It's essentially a smart upload that creates a new snapshot in the Litestream replica format.

Modified Step 3 (FUSE-less):

// In the bulk import endpoint, replace the execAsync for rsync with this:
logger.info(`[Bulk Import|Org ${orgId}] Backing up changes to GCS via litestream...`);
await execAsync(`litestream backup -o ${gcsReplicaUrl} ${localImportPath}`);
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
TypeScript
IGNORE_WHEN_COPYING_END

This achieves the same goal as rsync (updating the replica with the new state) but without needing to mount a filesystem, making it a much better fit for the Replit environment.

Use a Persistent VM on Replit: If you switch your Replit backend to a Reserved VM, you get a persistent filesystem and more control, where you could potentially run gcsfuse. This adds cost and complexity.

Offload the Work: The bulk import API endpoint could, instead of doing the work itself, create a job for a dedicated worker (like your Fly.io scheduler). The API would upload the CSV to GCS, then trigger the worker. The worker, already equipped with gcsfuse, would perform the restore-update-rsync cycle. This is the most robust but also most complex architecture.

Recommendation: Go with Option 1 (FUSE-less litestream backup). It gives you the performance benefits of a bulk update without the environmental complexity of gcsfuse on Replit.

Final, Refined Workflow for Bulk Uploads

Client: Uploads 50,000-contact CSV to POST /api/contacts/bulk-import.

API Endpoint (Replit):
a. Receives the file.
b. Acquires the GCS lock for that organization. This is critical. It tells all other processes (including other Litestream instances and the scheduler) to "stand by."
c. Runs litestream restore to get a fresh, local copy of the database in /tmp.
d. Parses the CSV and runs 50,000 INSERT statements inside a single transaction on the local database. This is very fast.
e. Runs litestream backup to efficiently push the now-modified local database to the GCS replica. This replaces the rsync step and avoids the need for gcsfuse.
f. Releases the GCS lock.
g. Deletes the local temporary database and returns a success message to the client.

This refined approach is elegant. It correctly identifies that a bulk upload is a special case that should bypass the standard real-time replication path, uses a locking mechanism for coordination, and leverages the right tool (litestream backup) for the specific environment (Replit).