-- Create pending_signups table to store user information temporarily during signup
CREATE TABLE IF NOT EXISTS pending_signups (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  organization_id INTEGER NOT NULL,
  first_name TEXT NOT NULL,
  last_name TEXT,
  email TEXT NOT NULL,
  created_at TEXT NOT NULL,
  FOREIGN KEY (organization_id) REFERENCES organizations(id) ON DELETE CASCADE
);

-- Add index for faster lookups
CREATE INDEX IF NOT EXISTS idx_pending_signups_org_id ON pending_signups(organization_id);
CREATE INDEX IF NOT EXISTS idx_pending_signups_email ON pending_signups(email); 