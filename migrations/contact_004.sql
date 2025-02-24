-- Create leads table to track potential contacts
CREATE TABLE IF NOT EXISTS leads (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    email TEXT NOT NULL,
    status TEXT NOT NULL DEFAULT 'new',
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    converted_contact_id INTEGER,
    FOREIGN KEY (converted_contact_id) REFERENCES contacts(id)
);

-- Add indexes for leads table
CREATE INDEX IF NOT EXISTS idx_leads_email ON leads(email);
CREATE INDEX IF NOT EXISTS idx_leads_status ON leads(status);

-- Drop and recreate contact_events table to support both contacts and leads
DROP TABLE IF EXISTS contact_events;

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

-- Add indexes for contact_events table
CREATE INDEX IF NOT EXISTS idx_contact_events_contact_id ON contact_events(contact_id);
CREATE INDEX IF NOT EXISTS idx_contact_events_lead_id ON contact_events(lead_id);
CREATE INDEX IF NOT EXISTS idx_contact_events_type ON contact_events(event_type); 