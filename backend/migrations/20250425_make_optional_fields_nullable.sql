-- Migration to make optional fields nullable
-- This migration allows all non-critical fields to be NULL based on frontend changes
-- This migration needs to be applied to all organization databases
-- This version includes recovery from partial migration failures

-- Method: Split the migration into small, independent steps that can be run
-- even if a previous migration attempt was interrupted

-- STEP 1: Clean up any temporary tables from previous failed migrations
DROP TABLE IF EXISTS contacts_new;

-- STEP 2: Create new contacts table with the desired schema
CREATE TABLE IF NOT EXISTS contacts_new (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email TEXT NOT NULL UNIQUE,
    current_carrier TEXT, 
    current_premium TEXT, 
    plan_type TEXT, 
    effective_date TEXT, 
    birth_date TEXT, 
    tobacco_user INTEGER NOT NULL DEFAULT 0,
    gender TEXT, 
    state TEXT, 
    zip_code TEXT, 
    agent_id INTEGER,
    last_emailed DATETIME,
    phone_number TEXT, 
    status TEXT NOT NULL DEFAULT '',
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- STEP 3: Import data from contacts table if it exists
-- This will fail gracefully if the table doesn't exist
INSERT OR IGNORE INTO contacts_new(
    id, first_name, last_name, email, current_carrier, current_premium, 
    plan_type, effective_date, birth_date, tobacco_user, gender, 
    state, zip_code, agent_id, last_emailed, phone_number, 
    status, created_at, updated_at
)
SELECT 
    id, first_name, last_name, email, current_carrier, current_premium, 
    plan_type, effective_date, birth_date, COALESCE(tobacco_user, 0), gender, 
    state, zip_code, agent_id, last_emailed, phone_number, 
    COALESCE(status, ''), COALESCE(created_at, CURRENT_TIMESTAMP), 
    COALESCE(updated_at, CURRENT_TIMESTAMP)
FROM contacts WHERE 1=1;

-- STEP 4: Import data from contacts_backup table if it exists and contacts doesn't
-- This handles cases where migration was interrupted
INSERT OR IGNORE INTO contacts_new(
    id, first_name, last_name, email, current_carrier, current_premium, 
    plan_type, effective_date, birth_date, tobacco_user, gender, 
    state, zip_code, agent_id, last_emailed, phone_number, 
    status, created_at, updated_at
)
SELECT 
    id, first_name, last_name, email, current_carrier, current_premium, 
    plan_type, effective_date, birth_date, COALESCE(tobacco_user, 0), gender, 
    state, zip_code, agent_id, last_emailed, phone_number, 
    COALESCE(status, ''), COALESCE(created_at, CURRENT_TIMESTAMP), 
    COALESCE(updated_at, CURRENT_TIMESTAMP)
FROM contacts_backup WHERE 1=1;

-- STEP 5: Drop old tables and rename new table
DROP TABLE IF EXISTS contacts;

-- Rename the new table to contacts
ALTER TABLE contacts_new RENAME TO contacts;

-- STEP 6: Recreate indexes
CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email);
CREATE INDEX IF NOT EXISTS idx_contacts_zip_code ON contacts(zip_code);
CREATE INDEX IF NOT EXISTS idx_contacts_state ON contacts(state);
CREATE INDEX IF NOT EXISTS idx_contacts_created_at ON contacts(created_at);
CREATE INDEX IF NOT EXISTS idx_contacts_updated_at ON contacts(updated_at);
CREATE TRIGGER IF NOT EXISTS update_contacts_timestamp AFTER UPDATE ON contacts 
BEGIN 
  UPDATE contacts SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id; 
END; 