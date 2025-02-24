import { createClient } from '@libsql/client';
import { TURSO_CONFIG } from '../config/turso';

export class TursoService {
  private headers = {
    'Authorization': `Bearer ${TURSO_CONFIG.API_TOKEN}`,
    'Content-Type': 'application/json'
  };

  async createOrganizationDatabase(orgId: string): Promise<{url: string, token: string}> {
    const dbName = `org-${orgId}`;

    // Create database using Turso API
    const createDbResponse = await fetch(
      `${TURSO_CONFIG.API_URL}/organizations/${TURSO_CONFIG.ORG_SLUG}/databases`, 
      {
        method: 'POST',
        headers: this.headers,
        body: JSON.stringify({
          name: dbName,
          group: TURSO_CONFIG.GROUP_NAME
        })
      }
    );

    if (!createDbResponse.ok) {
      throw new Error(`Failed to create database: ${await createDbResponse.text()}`);
    }

    const dbData = await createDbResponse.json();
    
    // Generate auth token for the database
    const tokenResponse = await fetch(
      `${TURSO_CONFIG.API_URL}/organizations/${TURSO_CONFIG.ORG_SLUG}/databases/${dbName}/auth/tokens`,
      {
        method: 'POST',
        headers: this.headers
      }
    );

    if (!tokenResponse.ok) {
      throw new Error(`Failed to create auth token: ${await tokenResponse.text()}`);
    }

    const tokenData = await tokenResponse.json();

    // Initialize database client and create schema
    const client = createClient({
      url: `https://${dbData.database.Hostname}`,
      authToken: tokenData.jwt
    });

    await client.execute(`
      CREATE TABLE contacts (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        first_name TEXT NOT NULL,
        last_name TEXT NOT NULL,
        email TEXT NOT NULL,
        current_carrier TEXT NOT NULL,
        plan_type TEXT NOT NULL,
        effective_date TEXT NOT NULL,
        birth_date TEXT NOT NULL,
        tobacco_user BOOLEAN NOT NULL,
        gender TEXT NOT NULL,
        state TEXT NOT NULL,
        zip_code TEXT NOT NULL,
        agent_id INTEGER,
        last_emailed TEXT,
        phone_number TEXT NOT NULL DEFAULT '',
        status TEXT NOT NULL DEFAULT '',
        created_at TEXT DEFAULT CURRENT_TIMESTAMP
      );

      CREATE TABLE leads (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        email TEXT NOT NULL,
        status TEXT NOT NULL DEFAULT 'new',
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        converted_contact_id INTEGER,
        FOREIGN KEY (converted_contact_id) REFERENCES contacts(id)
      );

      CREATE INDEX IF NOT EXISTS idx_leads_email ON leads(email);
      CREATE INDEX IF NOT EXISTS idx_leads_status ON leads(status);

      CREATE TABLE contact_events (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        contact_id INTEGER,
        lead_id INTEGER,
        event_type TEXT NOT NULL,
        metadata TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (contact_id) REFERENCES contacts(id),
        FOREIGN KEY (lead_id) REFERENCES leads(id),
        CHECK ((contact_id IS NOT NULL AND lead_id IS NULL) OR (contact_id IS NULL AND lead_id IS NOT NULL))
      );

      CREATE INDEX IF NOT EXISTS idx_contact_events_contact_id ON contact_events(contact_id);
      CREATE INDEX IF NOT EXISTS idx_contact_events_lead_id ON contact_events(lead_id);
      CREATE INDEX IF NOT EXISTS idx_contact_events_type ON contact_events(event_type);

      CREATE TABLE contact_requests (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        email TEXT NOT NULL,
        request_type TEXT NOT NULL CHECK(request_type IN ('accept', 'decline', 'generic')),
        contact_id INTEGER,
        status TEXT NOT NULL DEFAULT 'new',
        agent_name TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (contact_id) REFERENCES contacts(id)
      );
    `);

    return {
      url: `https://${dbData.database.Hostname}`,
      token: tokenData.jwt
    };
  }

  async deleteOrganizationDatabase(dbName: string): Promise<void> {
    const response = await fetch(
      `${TURSO_CONFIG.API_URL}/organizations/${TURSO_CONFIG.ORG_SLUG}/databases/${dbName}`,
      {
        method: 'DELETE',
        headers: this.headers
      }
    );

    if (!response.ok) {
      throw new Error(`Failed to delete database: ${await response.text()}`);
    }
  }
} 