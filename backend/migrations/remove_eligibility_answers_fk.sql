-- Migration script to update foreign key constraints to reference contacts instead of contacts_backup

-- Part 1: Fix eligibility_answers table
-- Step 1: Create a new table with the correct foreign key constraint
CREATE TABLE eligibility_answers_new (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    contact_id INTEGER NOT NULL,
    quote_id TEXT NOT NULL,
    answers TEXT NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
);

-- Step 2: Copy data from the old table to the new table
INSERT INTO eligibility_answers_new (id, contact_id, quote_id, answers, created_at)
SELECT id, contact_id, quote_id, answers, created_at
FROM eligibility_answers;

-- Step 3: Drop the old table
DROP TABLE eligibility_answers;

-- Step 4: Rename the new table to the original name
ALTER TABLE eligibility_answers_new RENAME TO eligibility_answers;

-- Part 2: Fix contact_events table
-- Step 1: Create a new table with the correct foreign key constraints
CREATE TABLE contact_events_new (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    contact_id INTEGER,
    lead_id INTEGER,
    event_type TEXT NOT NULL,
    metadata TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE,
    FOREIGN KEY (lead_id) REFERENCES leads(id) ON DELETE CASCADE
);

-- Step 2: Copy data from the old table to the new table
INSERT INTO contact_events_new (id, contact_id, lead_id, event_type, metadata, created_at)
SELECT id, contact_id, lead_id, event_type, metadata, created_at
FROM contact_events;

-- Step 3: Drop the old table
DROP TABLE contact_events;

-- Step 4: Rename the new table to the original name
ALTER TABLE contact_events_new RENAME TO contact_events;

-- Create required indexes
CREATE INDEX IF NOT EXISTS idx_contact_events_contact_id ON contact_events(contact_id);
CREATE INDEX IF NOT EXISTS idx_contact_events_lead_id ON contact_events(lead_id);
CREATE INDEX IF NOT EXISTS idx_contact_events_type ON contact_events(event_type);

-- Optional: Add a comment or log to confirm the migration
-- Note: Foreign key constraints on contact_id now reference contacts(id) instead of contacts_backup 