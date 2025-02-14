-- Drop unused tables
DROP TABLE IF EXISTS agents;
DROP TABLE IF EXISTS migrations;
DROP TABLE IF EXISTS verification_tokens;

-- Drop and recreate contacts table
DROP TABLE IF EXISTS contacts;
CREATE TABLE contacts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email TEXT NOT NULL,
    current_carrier TEXT,
    plan_type TEXT,
    effective_date DATE,
    birth_date DATE,
    tobacco_user BOOLEAN DEFAULT FALSE,
    gender TEXT,
    state TEXT,
    zip_code TEXT,
    agent_id INTEGER,
    last_emailed DATETIME,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (agent_id) REFERENCES users(id)
);

-- Add indexes for performance
CREATE INDEX IF NOT EXISTS idx_contacts_email ON contacts(email);
CREATE INDEX IF NOT EXISTS idx_contacts_agent_id ON contacts(agent_id); 