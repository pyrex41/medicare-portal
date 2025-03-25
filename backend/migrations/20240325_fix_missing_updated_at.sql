-- Check if updated_at column exists, if not add it
PRAGMA foreign_keys=off;

BEGIN TRANSACTION;

-- Create a temporary table with the same structure plus the updated_at column
CREATE TABLE IF NOT EXISTS contacts_new (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  email TEXT NOT NULL,
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
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Copy data from the old table to the new table
INSERT INTO contacts_new 
SELECT 
  id, 
  first_name, 
  last_name, 
  email, 
  current_carrier, 
  plan_type, 
  effective_date, 
  birth_date, 
  tobacco_user, 
  gender, 
  state, 
  zip_code, 
  agent_id, 
  last_emailed, 
  phone_number, 
  COALESCE(created_at, CURRENT_TIMESTAMP),
  CURRENT_TIMESTAMP
FROM contacts;

-- Drop the old table
DROP TABLE contacts;

-- Rename the new table to the original name
ALTER TABLE contacts_new RENAME TO contacts;

-- Recreate indexes
CREATE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email);
CREATE INDEX IF NOT EXISTS idx_contacts_agent_id ON contacts(agent_id);
CREATE INDEX IF NOT EXISTS idx_contacts_state ON contacts(state);
CREATE INDEX IF NOT EXISTS idx_contacts_current_carrier ON contacts(current_carrier);
CREATE INDEX IF NOT EXISTS idx_contacts_effective_date ON contacts(effective_date);
CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email_unique ON contacts(LOWER(TRIM(email)));

COMMIT;

PRAGMA foreign_keys=on;
