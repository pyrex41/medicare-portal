-- Migration to ensure unique index on email in contacts table
-- Run this script on all organization databases to prevent duplicate contacts

-- First check if we have duplicates
SELECT COUNT(*) as total_contacts FROM contacts;
SELECT COUNT(*) - COUNT(DISTINCT LOWER(TRIM(email))) as duplicate_emails FROM contacts;

-- Check if we already have the unique index on email
SELECT COUNT(*) as has_unique_index FROM sqlite_master 
WHERE type='index' AND name='idx_contacts_email_unique';

-- Create unique index if it doesn't exist 
-- (This will fail if there are duplicate emails, so run the deduplication process first)
CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email_unique ON contacts(LOWER(TRIM(email)));

-- Create or update the standard index
CREATE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email);

-- Force indexes to be used by SQLite
ANALYZE;

-- Verify index creation
SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='contacts';