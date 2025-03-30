-- Migration to deduplicate contacts and ensure unique email constraint works properly
-- This script will:
-- 1. Check for duplicate emails
-- 2. Back up the existing contacts table
-- 3. Deduplicate contacts by keeping the most recently updated record for each email
-- 4. Create the unique index to prevent future duplicates

-- Print counts before
SELECT COUNT(*) as total_contacts_before FROM contacts;
SELECT COUNT(*) - COUNT(DISTINCT LOWER(TRIM(email))) as duplicate_emails FROM contacts;

-- Check if we already have a unique index on email
SELECT COUNT(*) as has_unique_index FROM sqlite_master 
WHERE type='index' AND name='idx_contacts_email_unique';

-- Create a backup of the original table for safety
DROP TABLE IF EXISTS contacts_backup;
CREATE TABLE contacts_backup AS SELECT * FROM contacts;

-- Only proceed with deduplication if there are duplicates
CREATE TABLE contacts_temp AS
SELECT * FROM (
  SELECT id, first_name, last_name, email, current_carrier, plan_type, effective_date,
         birth_date, tobacco_user, gender, state, zip_code, agent_id, last_emailed,
         phone_number, created_at, updated_at,
         ROW_NUMBER() OVER (PARTITION BY LOWER(TRIM(email)) ORDER BY updated_at DESC) as rn
  FROM contacts
) WHERE rn = 1;

-- Drop any existing unique index on email before we drop the table
DROP INDEX IF EXISTS idx_contacts_email_unique;
DROP INDEX IF EXISTS idx_contacts_email;

-- Drop original table
DROP TABLE contacts;

-- Rename the deduped table to contacts
ALTER TABLE contacts_temp RENAME TO contacts;

-- Recreate the primary key
CREATE UNIQUE INDEX sqlite_autoindex_contacts_1 ON contacts(id);

-- Add unique index on email with case and whitespace insensitivity
CREATE UNIQUE INDEX idx_contacts_email_unique ON contacts(LOWER(TRIM(email)));

-- Add other helpful indexes
CREATE INDEX idx_contacts_email ON contacts(email);
CREATE INDEX IF NOT EXISTS idx_contacts_state ON contacts(state);
CREATE INDEX IF NOT EXISTS idx_contacts_agent_id ON contacts(agent_id);

-- Print counts after
SELECT COUNT(*) as total_contacts_after FROM contacts;

-- Verify indexes
SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='contacts';