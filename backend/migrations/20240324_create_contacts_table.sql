-- Create contacts table if it doesn't exist
CREATE TABLE IF NOT EXISTS contacts (
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

-- Add indexes for common queries
CREATE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email);
CREATE INDEX IF NOT EXISTS idx_contacts_agent_id ON contacts(agent_id);
CREATE INDEX IF NOT EXISTS idx_contacts_state ON contacts(state);
CREATE INDEX IF NOT EXISTS idx_contacts_current_carrier ON contacts(current_carrier);
CREATE INDEX IF NOT EXISTS idx_contacts_effective_date ON contacts(effective_date);

-- Add unique constraint on email
CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email_unique ON contacts(LOWER(TRIM(email))); 