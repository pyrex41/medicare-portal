-- Add Turso database info to organizations table
ALTER TABLE organizations ADD COLUMN turso_db_url TEXT;
ALTER TABLE organizations ADD COLUMN turso_auth_token TEXT;

-- Remove contacts table since it will move to org-specific Turso DBs
DROP TABLE IF EXISTS contacts; 