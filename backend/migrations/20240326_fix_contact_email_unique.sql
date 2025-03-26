-- Migration to deduplicate contacts and ensure unique email constraint works properly

-- Print counts before
SELECT COUNT(*) as total_contacts_before FROM contacts;
SELECT COUNT(*) - COUNT(DISTINCT LOWER(TRIM(email))) as duplicate_emails FROM contacts;

-- Check if we already have a unique index on email
SELECT COUNT(*) as has_unique_index FROM sqlite_master 
WHERE type='index' AND name='idx_contacts_email_unique';

-- Only proceed with deduplication if there are duplicates
CREATE TABLE IF NOT EXISTS contacts_temp AS
SELECT * FROM (
  SELECT *, ROW_NUMBER() OVER (PARTITION BY LOWER(TRIM(email)) ORDER BY updated_at DESC) as rn
  FROM contacts
) WHERE rn = 1;

-- Drop any existing unique index on email before we drop the table
DROP INDEX IF EXISTS idx_contacts_email_unique;

-- Create a backup of the original table for safety
CREATE TABLE contacts_backup AS SELECT * FROM contacts;

-- Drop original table
DROP TABLE contacts;

-- Rename the deduped table to contacts
ALTER TABLE contacts_temp RENAME TO contacts;

-- Recreate the primary key
CREATE UNIQUE INDEX sqlite_autoindex_contacts_1 ON contacts(id);

-- Add unique index on email with case and whitespace insensitivity
CREATE UNIQUE INDEX idx_contacts_email_unique ON contacts(LOWER(TRIM(email)));

-- Add other helpful indexes
CREATE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email);
CREATE INDEX IF NOT EXISTS idx_contacts_state ON contacts(state);
CREATE INDEX IF NOT EXISTS idx_contacts_agent_id ON contacts(agent_id);

-- Print counts after
SELECT COUNT(*) as total_contacts_after FROM contacts;