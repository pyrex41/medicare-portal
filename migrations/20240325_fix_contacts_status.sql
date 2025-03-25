-- Temporarily disable foreign key constraints
PRAGMA foreign_keys = OFF;

-- Drop any leftover tables/views from previous failed migrations
DROP TABLE IF EXISTS contacts_new;
DROP VIEW IF EXISTS v_contact_stats;

-- Create contacts table if it doesn't exist
CREATE TABLE IF NOT EXISTS contacts (
  id INTEGER PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  email TEXT NOT NULL,
  current_carrier TEXT,
  plan_type TEXT,
  effective_date TEXT NOT NULL,
  birth_date TEXT NOT NULL,
  tobacco_user BOOLEAN NOT NULL,
  gender TEXT NOT NULL,
  state TEXT NOT NULL,
  zip_code TEXT NOT NULL,
  agent_id INTEGER,
  last_emailed TEXT,
  phone_number TEXT NOT NULL DEFAULT '',
  status TEXT NOT NULL DEFAULT '',
  created_at TEXT DEFAULT CURRENT_TIMESTAMP,
  aep_request BOOLEAN DEFAULT FALSE,
  aep_request_date DATETIME
);

-- Create new contacts table with proper constraints
CREATE TABLE contacts_new (
  id INTEGER PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  email TEXT NOT NULL,
  current_carrier TEXT,
  plan_type TEXT,
  effective_date TEXT NOT NULL,
  birth_date TEXT NOT NULL,
  tobacco_user BOOLEAN NOT NULL,
  gender TEXT NOT NULL,
  state TEXT NOT NULL,
  zip_code TEXT NOT NULL,
  agent_id INTEGER,
  last_emailed TEXT,
  phone_number TEXT NOT NULL DEFAULT '',
  status TEXT NOT NULL DEFAULT '',
  created_at TEXT DEFAULT CURRENT_TIMESTAMP,
  aep_request BOOLEAN DEFAULT FALSE,
  aep_request_date DATETIME
);

-- Copy data from old table to new table, handling missing columns
INSERT INTO contacts_new (
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
  status,
  created_at,
  aep_request,
  aep_request_date
)
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
  NULL as last_emailed,
  '' as phone_number,
  '' as status,
  CURRENT_TIMESTAMP as created_at,
  FALSE as aep_request,
  NULL as aep_request_date
FROM contacts;

-- Drop old contacts table and rename new one
DROP TABLE contacts;
ALTER TABLE contacts_new RENAME TO contacts;

-- Create indices for better performance
CREATE INDEX IF NOT EXISTS idx_contacts_agent_id ON contacts(agent_id);
CREATE INDEX IF NOT EXISTS idx_contacts_status ON contacts(status);
CREATE INDEX IF NOT EXISTS idx_contacts_aep_request ON contacts(aep_request);

-- Recreate the view
CREATE VIEW v_contact_stats AS
WITH contact_metrics AS (
    SELECT 
        agent_id,
        current_carrier,
        state,
        COUNT(*) as total_contacts,
        SUM(CASE WHEN last_emailed IS NOT NULL THEN 1 ELSE 0 END) as contacted_count,
        SUM(CASE WHEN tobacco_user = 1 THEN 1 ELSE 0 END) as tobacco_users,
        SUM(CASE WHEN plan_type IS NOT NULL THEN 1 ELSE 0 END) as with_plan_type
    FROM contacts
    GROUP BY agent_id, current_carrier, state
)
SELECT 
    cm.*,
    (SELECT COUNT(DISTINCT current_carrier) FROM contacts WHERE agent_id = cm.agent_id) as unique_carriers,
    (SELECT COUNT(DISTINCT state) FROM contacts WHERE agent_id = cm.agent_id) as unique_states
FROM contact_metrics cm;

-- Re-enable foreign key constraints
PRAGMA foreign_keys = ON; 