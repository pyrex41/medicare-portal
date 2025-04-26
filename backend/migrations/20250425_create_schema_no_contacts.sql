-- Migration for databases WITHOUT an existing contacts table
-- This script creates the contacts table with nullable fields

-- Drop any temporary tables that might exist
DROP TABLE IF EXISTS contacts_new;

-- Create contacts table with the correct schema
CREATE TABLE IF NOT EXISTS contacts (
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

-- Check if contacts_backup exists before trying to access it
CREATE TEMP TABLE IF NOT EXISTS table_check AS
SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE type='table' AND name='contacts_backup') as has_contacts_backup;

-- If contacts_backup exists, check its columns
CREATE TEMP TABLE IF NOT EXISTS column_check AS
SELECT 
    CASE WHEN (SELECT has_contacts_backup FROM table_check) = 1 THEN
        EXISTS(SELECT 1 FROM pragma_table_info('contacts_backup') WHERE name='current_premium')
    ELSE 0 END as has_current_premium,
    CASE WHEN (SELECT has_contacts_backup FROM table_check) = 1 THEN
        EXISTS(SELECT 1 FROM pragma_table_info('contacts_backup') WHERE name='plan_type')
    ELSE 0 END as has_plan_type;

-- Copy data from contacts_backup if it exists
INSERT OR IGNORE INTO contacts(
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
FROM contacts_backup WHERE EXISTS(SELECT 1 FROM sqlite_master WHERE type='table' AND name='contacts_backup');

-- Clean up temp tables
DROP TABLE IF EXISTS table_check;
DROP TABLE IF EXISTS column_check;

-- Create indexes
CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email);
CREATE INDEX IF NOT EXISTS idx_contacts_zip_code ON contacts(zip_code);
CREATE INDEX IF NOT EXISTS idx_contacts_state ON contacts(state);
CREATE INDEX IF NOT EXISTS idx_contacts_created_at ON contacts(created_at);
CREATE INDEX IF NOT EXISTS idx_contacts_updated_at ON contacts(updated_at);
CREATE TRIGGER IF NOT EXISTS update_contacts_timestamp AFTER UPDATE ON contacts 
BEGIN 
  UPDATE contacts SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id; 
END; 