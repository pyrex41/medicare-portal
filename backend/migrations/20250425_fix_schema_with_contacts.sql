-- Migration for databases WITH existing contacts table
-- This script makes optional fields nullable

-- Clean up any temporary tables from previous failed migrations
DROP TABLE IF EXISTS contacts_new;

-- Create new contacts table with the desired schema
CREATE TABLE contacts_new (
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

-- Get schema information to determine available columns
CREATE TEMP TABLE IF NOT EXISTS pragma_table_info AS 
SELECT * FROM pragma_table_info('contacts');

-- Set variables for optional column existence
CREATE TEMP TABLE IF NOT EXISTS column_check AS
SELECT 
    EXISTS(SELECT 1 FROM pragma_table_info WHERE name='current_premium') as has_current_premium,
    EXISTS(SELECT 1 FROM pragma_table_info WHERE name='plan_type') as has_plan_type;

-- Copy data from the existing contacts table with safe column access
INSERT INTO contacts_new(
    id, first_name, last_name, email, current_carrier,
    plan_type, effective_date, birth_date, tobacco_user, gender, 
    state, zip_code, agent_id, last_emailed, phone_number, 
    status, created_at, updated_at
)
SELECT 
    id, first_name, last_name, email, current_carrier,
    plan_type, effective_date, birth_date, COALESCE(tobacco_user, 0), gender, 
    state, zip_code, agent_id, last_emailed, phone_number, 
    COALESCE(status, ''), COALESCE(created_at, CURRENT_TIMESTAMP), 
    COALESCE(updated_at, CURRENT_TIMESTAMP)
FROM contacts;

-- Handle current_premium separately based on column check
UPDATE contacts_new 
SET current_premium = (
    SELECT NULL
    FROM contacts 
    WHERE contacts.id = contacts_new.id
);

-- Rename tables
ALTER TABLE contacts RENAME TO contacts_backup;
ALTER TABLE contacts_new RENAME TO contacts;

-- Clean up temp tables
DROP TABLE IF EXISTS pragma_table_info;
DROP TABLE IF EXISTS column_check;

-- Recreate indexes
CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email);
CREATE INDEX IF NOT EXISTS idx_contacts_zip_code ON contacts(zip_code);
CREATE INDEX IF NOT EXISTS idx_contacts_state ON contacts(state);
CREATE INDEX IF NOT EXISTS idx_contacts_created_at ON contacts(created_at);
CREATE INDEX IF NOT EXISTS idx_contacts_updated_at ON contacts(updated_at);
CREATE TRIGGER IF NOT EXISTS update_contacts_timestamp AFTER UPDATE ON contacts 
BEGIN 
  UPDATE contacts SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id; 
END; 