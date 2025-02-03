-- Create agents table
CREATE TABLE IF NOT EXISTS agents (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email TEXT NOT NULL UNIQUE,
    phone TEXT NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Create trigger for agents updated_at
CREATE TRIGGER IF NOT EXISTS update_agents_timestamp 
AFTER UPDATE ON agents
BEGIN
    UPDATE agents SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;

-- Create temporary table with new schema
CREATE TABLE contacts_new (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email TEXT NOT NULL,
    current_carrier TEXT NOT NULL,
    plan_type TEXT NOT NULL,
    effective_date DATE NOT NULL,
    birth_date DATE NOT NULL,
    tobacco_user BOOLEAN NOT NULL DEFAULT FALSE,
    gender TEXT CHECK(gender IN ('M', 'F')) NOT NULL,
    state TEXT NOT NULL,
    zip_code TEXT NOT NULL,
    agent_id INTEGER,
    last_emailed_date DATE,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (agent_id) REFERENCES agents(id)
);

-- Copy existing data with generated email addresses
INSERT INTO contacts_new (
    id, first_name, last_name, email, current_carrier,
    plan_type, effective_date, birth_date, tobacco_user,
    gender, state, zip_code, last_emailed_date, created_at,
    updated_at
)
SELECT 
    id, first_name, last_name,
    lower(first_name || '.' || last_name || '@example.com') as email,
    current_carrier, plan_type, effective_date, birth_date,
    tobacco_user, gender, state, zip_code, last_emailed_date,
    created_at, updated_at
FROM contacts;

-- Drop old table and rename new one
DROP TABLE contacts;
ALTER TABLE contacts_new RENAME TO contacts;

-- Recreate the updated_at trigger for contacts
CREATE TRIGGER IF NOT EXISTS update_contacts_timestamp 
AFTER UPDATE ON contacts
BEGIN
    UPDATE contacts SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;

-- Create indexes
CREATE INDEX idx_contacts_email ON contacts(email);
CREATE INDEX idx_contacts_agent_id ON contacts(agent_id);
CREATE INDEX idx_agents_email ON agents(email); 