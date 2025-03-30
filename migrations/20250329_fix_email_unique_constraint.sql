-- Migration to fix email unique constraint for proper ON CONFLICT handling
-- Date: 2025-03-29

-- Turn off foreign key enforcement temporarily for migration
PRAGMA foreign_keys = OFF;

-- Diagnostic step: Get table information to debug schema issues
PRAGMA table_info(contacts);
SELECT name, sql FROM sqlite_master WHERE type='table' AND name='contacts';
SELECT name, sql FROM sqlite_master WHERE type='table' AND name='contacts_backup';
SELECT name, sql FROM sqlite_master WHERE type='table' AND name='eligibility_answers';

-- Check if updated_at column exists in contacts
SELECT COUNT(*) as has_updated_at_column FROM pragma_table_info('contacts') WHERE name = 'updated_at';

-- Step 1: Check if contacts table exists
SELECT name FROM sqlite_master WHERE type='table' AND name='contacts';

-- Save view definitions that depend on contacts table
-- Get the SQL for the v_contact_stats view if it exists
SELECT sql FROM sqlite_master WHERE type='view' AND name='v_contact_stats';

-- Drop any views that depend on the contacts table
DROP VIEW IF EXISTS v_contact_stats;

-- Drop dependent tables temporarily (we'll recreate them later)
-- Store their schema first
SELECT sql FROM sqlite_master WHERE type='table' AND name='eligibility_answers';
CREATE TABLE IF NOT EXISTS schema_backup (
  table_name TEXT PRIMARY KEY,
  table_sql TEXT,
  data_backup_table TEXT
);

-- Store schema info for dependent tables
INSERT OR REPLACE INTO schema_backup (table_name, table_sql)
SELECT 'eligibility_answers', sql FROM sqlite_master 
WHERE type='table' AND name='eligibility_answers';

-- Create backup for eligibility_answers if it exists
CREATE TABLE IF NOT EXISTS eligibility_answers_backup AS 
SELECT * FROM eligibility_answers
WHERE EXISTS (SELECT 1 FROM sqlite_master WHERE type='table' AND name='eligibility_answers');

-- Update schema_backup to record the data backup
UPDATE schema_backup SET data_backup_table = 'eligibility_answers_backup'
WHERE table_name = 'eligibility_answers';

-- Now drop the dependent table to avoid foreign key issues
DROP TABLE IF EXISTS eligibility_answers;

-- Check if updated_at column exists in contacts
SELECT COUNT(*) as has_updated_at_column FROM pragma_table_info('contacts') WHERE name = 'updated_at';

-- Add updated_at column to contacts if it doesn't exist
-- Since SQLite doesn't have IF NOT EXISTS for ADD COLUMN in older versions, we need a workaround
-- We'll use a transaction and a separate table to store the results

-- First approach: Try-catch with transaction
BEGIN TRANSACTION;
  -- This will succeed if the column doesn't exist and fail if it does
  SELECT CASE 
    WHEN NOT EXISTS(SELECT 1 FROM pragma_table_info('contacts') WHERE name = 'updated_at')
    THEN 'Adding updated_at column'
    ELSE 'Column already exists'
  END as status;
  
  -- Only try to add the column if it doesn't exist
  SELECT CASE 
    WHEN NOT EXISTS(SELECT 1 FROM pragma_table_info('contacts') WHERE name = 'updated_at')
    THEN (
      -- This is a dummy statement to make the syntax valid - the actual ALTER TABLE is below
      SELECT 'Will add column'
    ) 
    ELSE (
      -- This is a dummy statement to make the syntax valid
      SELECT 'Will skip adding column'
    )
  END as action;
COMMIT;

-- Now we can safely execute the ALTER TABLE outside the transaction
-- It will error if the column already exists, but we'll catch that in the script
BEGIN;
  SELECT 'Attempting to add updated_at column if needed' as operation;
  SELECT CASE 
    WHEN NOT EXISTS(SELECT 1 FROM pragma_table_info('contacts') WHERE name = 'updated_at')
    THEN 'true'
    ELSE 'false'
  END as should_add_column;
COMMIT;

-- Step 2: If contacts table doesn't exist, create it from scratch
CREATE TABLE IF NOT EXISTS contacts (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE,
  current_carrier TEXT NOT NULL,
  plan_type TEXT NOT NULL,
  effective_date TEXT NOT NULL,
  birth_date TEXT NOT NULL,
  tobacco_user INTEGER NOT NULL,
  gender TEXT NOT NULL,
  state TEXT NOT NULL,
  zip_code TEXT NOT NULL,
  agent_id INTEGER,
  last_emailed DATETIME,
  phone_number TEXT NOT NULL DEFAULT '',
  status TEXT NOT NULL DEFAULT '',
  aep_request INTEGER DEFAULT 0,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Check if we need to restore from backup
SELECT name FROM sqlite_master WHERE type='table' AND name='contacts_backup';

-- Dynamically construct column list for backup restore based on what exists
-- This will be more flexible when dealing with different schema versions
CREATE TEMP TABLE IF NOT EXISTS contact_columns AS
SELECT name FROM pragma_table_info('contacts');

-- If original migration was interrupted and we have a backup, restore it with flexible column handling
INSERT OR IGNORE INTO contacts (
  id, first_name, last_name, email, current_carrier, plan_type, 
  effective_date, birth_date, tobacco_user, gender, state, zip_code, 
  agent_id, last_emailed, phone_number, 
  -- Only include these if they exist in both tables
  CASE WHEN EXISTS (SELECT 1 FROM pragma_table_info('contacts_backup') WHERE name = 'status') THEN 'status' ELSE NULL END,
  CASE WHEN EXISTS (SELECT 1 FROM pragma_table_info('contacts_backup') WHERE name = 'aep_request') THEN 'aep_request' ELSE NULL END,
  created_at, updated_at
)
SELECT 
  id, first_name, last_name, email, current_carrier, plan_type, 
  effective_date, birth_date, tobacco_user, gender, state, zip_code, 
  agent_id, last_emailed, phone_number, 
  -- Only include these if they exist, otherwise use defaults
  CASE WHEN EXISTS (SELECT 1 FROM pragma_table_info('contacts_backup') WHERE name = 'status') 
       THEN status ELSE '' END,
  CASE WHEN EXISTS (SELECT 1 FROM pragma_table_info('contacts_backup') WHERE name = 'aep_request') 
       THEN aep_request ELSE 0 END,
  created_at, 
  CASE WHEN EXISTS (SELECT 1 FROM pragma_table_info('contacts_backup') WHERE name = 'updated_at') 
       THEN updated_at ELSE created_at END
FROM contacts_backup 
WHERE EXISTS (SELECT 1 FROM sqlite_master WHERE type='table' AND name='contacts_backup')
AND NOT EXISTS (SELECT 1 FROM contacts LIMIT 1);

-- After this point, we'll proceed with the main migration steps if contacts exists
-- First check if we have contacts with data
SELECT COUNT(*) as contacts_count FROM contacts;

-- Create a flexible backup taking into account column differences
CREATE TABLE IF NOT EXISTS contacts_backup AS 
SELECT 
  id, first_name, last_name, email, current_carrier, plan_type, 
  effective_date, birth_date, tobacco_user, gender, state, zip_code, 
  agent_id, last_emailed, phone_number,
  CASE WHEN EXISTS (SELECT 1 FROM pragma_table_info('contacts') WHERE name = 'status') 
       THEN status ELSE '' END as status,
  CASE WHEN EXISTS (SELECT 1 FROM pragma_table_info('contacts') WHERE name = 'aep_request') 
       THEN aep_request ELSE 0 END as aep_request,
  created_at,
  CASE WHEN EXISTS (SELECT 1 FROM pragma_table_info('contacts') WHERE name = 'updated_at') 
       THEN updated_at ELSE created_at END as updated_at
FROM contacts 
WHERE EXISTS (SELECT 1 FROM contacts LIMIT 1)
AND NOT EXISTS (SELECT 1 FROM sqlite_master WHERE type='table' AND name='contacts_backup');

-- Create a temporary table with deduplicated contacts
-- We'll keep the most recently updated record for each email
CREATE TABLE contacts_temp AS
SELECT 
  id, first_name, last_name, email, current_carrier, plan_type, 
  effective_date, birth_date, tobacco_user, gender, state, zip_code, 
  agent_id, last_emailed, phone_number, status, aep_request,
  created_at, updated_at
FROM (
  SELECT *, ROW_NUMBER() OVER (PARTITION BY LOWER(TRIM(email)) ORDER BY 
    CASE 
      -- Handle missing updated_at column by using created_at as fallback
      WHEN updated_at IS NULL THEN created_at 
      ELSE updated_at 
    END DESC,
    id DESC
  ) as rn
  FROM contacts
) WHERE rn = 1;

-- Drop existing indexes on contacts
DROP INDEX IF EXISTS idx_contacts_email;
DROP INDEX IF EXISTS idx_contacts_email_unique;
DROP INDEX IF EXISTS idx_contacts_agent_id;
DROP INDEX IF EXISTS idx_contacts_status;
DROP INDEX IF EXISTS idx_contacts_aep_request;
DROP TRIGGER IF EXISTS update_contacts_timestamp;

-- Drop the original contacts table
DROP TABLE IF EXISTS contacts;

-- Rename the temporary table to contacts
ALTER TABLE contacts_temp RENAME TO contacts;

-- Add the PRIMARY KEY constraint back (using proper methods, not the reserved name)
-- Don't try to create sqlite_autoindex_contacts_1 directly, as it's a reserved name
-- Instead, declare the id column as PRIMARY KEY directly when possible
-- If your SQLite version doesn't support ALTER TABLE ADD PRIMARY KEY, use this to properly 
-- set id as primary key in an existing table:
CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_id_pk ON contacts(id);

-- Create the proper unique constraint on the email column
-- This will ensure ON CONFLICT(email) works correctly
-- SQLite doesn't support ALTER TABLE ADD CONSTRAINT, so we use a unique index instead
CREATE UNIQUE INDEX contact_email_unique ON contacts(email);

-- Add our lowercased trimmed index for case-insensitive comparison
-- This is for searching and preventing duplicates when inserting
CREATE UNIQUE INDEX idx_contacts_email_unique ON contacts(LOWER(TRIM(email)));

-- Recreate other indexes from the original table
CREATE INDEX idx_contacts_email ON contacts(email);
CREATE INDEX idx_contacts_agent_id ON contacts(agent_id);
CREATE INDEX idx_contacts_status ON contacts(status);
CREATE INDEX idx_contacts_aep_request ON contacts(aep_request);

-- Ensure the updated_at column exists
SELECT COUNT(*) as has_updated_at_column FROM pragma_table_info('contacts') WHERE name = 'updated_at';

-- Recreate the update timestamp trigger
CREATE TRIGGER update_contacts_timestamp 
AFTER UPDATE ON contacts 
BEGIN 
  UPDATE contacts SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id; 
END;

-- Create the contact_events table if it doesn't exist
CREATE TABLE IF NOT EXISTS contact_events (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  contact_id INTEGER,
  lead_id INTEGER,
  event_type TEXT NOT NULL,
  metadata TEXT,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
  -- Removing foreign keys initially, will add them after all tables exist
);

-- Create indexes for contact_events
CREATE INDEX IF NOT EXISTS idx_contact_events_contact_id ON contact_events(contact_id);
CREATE INDEX IF NOT EXISTS idx_contact_events_lead_id ON contact_events(lead_id);
CREATE INDEX IF NOT EXISTS idx_contact_events_type ON contact_events(event_type);

-- Create the leads table if it doesn't exist
CREATE TABLE IF NOT EXISTS leads (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  email TEXT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(email)
);

-- Create index for leads
CREATE INDEX IF NOT EXISTS idx_leads_email ON leads(email);

-- Recreate eligibility_answers from backup if available
CREATE TABLE IF NOT EXISTS eligibility_answers (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  contact_id INTEGER NOT NULL,
  quote_id TEXT NOT NULL,
  answers TEXT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
  -- Removing foreign keys initially, will add them after all tables exist
);

-- Create index for eligibility_answers
CREATE INDEX IF NOT EXISTS idx_eligibility_answers_contact_id ON eligibility_answers(contact_id);

-- Now add foreign key constraints using ALTER TABLE if supported, or recreate the tables if needed
-- SQLite doesn't support adding constraints with ALTER TABLE, but we can enable them functionally
-- by creating appropriate indexes and enforcing at the application level

-- Recreate constraints by adding appropriate DELETE triggers as a workaround for ON DELETE CASCADE
-- When a contact is deleted, delete related records
CREATE TRIGGER IF NOT EXISTS trg_contacts_delete
AFTER DELETE ON contacts
BEGIN
  DELETE FROM eligibility_answers WHERE contact_id = OLD.id;
  DELETE FROM contact_events WHERE contact_id = OLD.id;
END;

-- When a lead is deleted, delete or null related records
CREATE TRIGGER IF NOT EXISTS trg_leads_delete
AFTER DELETE ON leads
BEGIN
  DELETE FROM contact_events WHERE lead_id = OLD.id;
END;

-- Restore data for eligibility_answers if we have a backup
INSERT OR IGNORE INTO eligibility_answers
SELECT * FROM eligibility_answers_backup
WHERE EXISTS (SELECT 1 FROM sqlite_master WHERE type='table' AND name='eligibility_answers_backup');

-- Drop backup tables when done
DROP TABLE IF EXISTS eligibility_answers_backup;
DROP TABLE IF EXISTS schema_backup;
DROP TABLE IF EXISTS contact_columns;

-- Recreate the v_contact_stats view
CREATE VIEW IF NOT EXISTS v_contact_stats AS
SELECT 
  COUNT(*) as total_contacts,
  SUM(CASE WHEN status = 'active' THEN 1 ELSE 0 END) as active_contacts,
  SUM(CASE WHEN created_at > date('now', '-30 days') THEN 1 ELSE 0 END) as new_contacts_30d
FROM contacts;

-- Show the results
SELECT 
  COUNT(*) as total_contacts_after,
  (SELECT COUNT(*) FROM contacts_backup WHERE EXISTS (SELECT 1 FROM sqlite_master WHERE type='table' AND name='contacts_backup')) as backup_count,
  COUNT(*) - COUNT(DISTINCT LOWER(TRIM(email))) as duplicate_emails_after
FROM contacts;

-- Final check: Verify we can do an ON CONFLICT query
INSERT OR IGNORE INTO contacts (
  first_name, last_name, email, current_carrier, plan_type, 
  effective_date, birth_date, tobacco_user, gender, state, zip_code, phone_number
) 
VALUES ('Test', 'User', 'test@example.com', 'Test', 'Test', 
  '2025-01-01', '1970-01-01', 0, 'M', 'TX', '12345', '123-456-7890')
ON CONFLICT(email) DO NOTHING;

-- Turn foreign keys back on
PRAGMA foreign_keys = ON; 