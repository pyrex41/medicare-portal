-- Add updated_at column to organizations table
ALTER TABLE organizations ADD COLUMN updated_at TEXT;

-- Set existing rows to have current timestamp
UPDATE organizations 
SET updated_at = DATETIME('now')
WHERE updated_at IS NULL; 