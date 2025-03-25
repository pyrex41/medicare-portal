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

-- Create eligibility_answers table if it doesn't exist
CREATE TABLE IF NOT EXISTS eligibility_answers (
    id INTEGER PRIMARY KEY,
    contact_id INTEGER NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    quote_id TEXT,
    answers TEXT NOT NULL,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
);

-- Create index for faster lookups by contact if it doesn't exist
CREATE INDEX IF NOT EXISTS eligibility_answers_contact_id_idx ON eligibility_answers(contact_id);

-- Create a new temporary table without the quote_id column
CREATE TABLE IF NOT EXISTS eligibility_answers_new (
    id INTEGER PRIMARY KEY,
    contact_id INTEGER NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    answers TEXT NOT NULL,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
);

-- Copy data from the old table to the new one, excluding quote_id
INSERT INTO eligibility_answers_new (id, contact_id, answers, created_at)
SELECT id, contact_id, answers, created_at 
FROM eligibility_answers;

-- Drop the old table
DROP TABLE eligibility_answers;

-- Rename the new table to the original name
ALTER TABLE eligibility_answers_new RENAME TO eligibility_answers;

-- Add the quote_id column as nullable
ALTER TABLE eligibility_answers ADD COLUMN quote_id TEXT;

-- Recreate the index
CREATE INDEX IF NOT EXISTS eligibility_answers_contact_id_idx ON eligibility_answers(contact_id); 