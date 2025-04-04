-- Migration to allow NULL values in current_carrier, plan_type and add current_premium column
-- This migration needs to be applied to all organization databases

-- Create a temporary table with the desired schema
CREATE TABLE contacts_new (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email TEXT NOT NULL UNIQUE,
    current_carrier TEXT, -- Changed to allow NULL
    current_premium TEXT, -- New column, allows NULL
    plan_type TEXT, -- Changed to allow NULL
    effective_date TEXT NOT NULL,
    birth_date TEXT NOT NULL,
    tobacco_user INTEGER NOT NULL,
    gender TEXT NOT NULL,
    state TEXT NOT NULL,
    zip_code TEXT NOT NULL,
    agent_id INTEGER,
    last_emailed DATETIME,
    phone_number TEXT NOT NULL DEFAULT '',
    status TEXT NOT NULL DEFAULT '',
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
    NULL as current_premium, -- Set existing rows to NULL for new column
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
    updated_at
FROM contacts;

-- Drop the old table
DROP TABLE contacts;

-- Rename the new table to contacts
ALTER TABLE contacts_new RENAME TO contacts;

-- Recreate any indexes that existed on the original table
CREATE UNIQUE INDEX idx_contacts_email ON contacts(email);
CREATE INDEX idx_contacts_zip_code ON contacts(zip_code);
CREATE INDEX idx_contacts_state ON contacts(state);
CREATE INDEX idx_contacts_created_at ON contacts(created_at);
CREATE INDEX idx_contacts_updated_at ON contacts(updated_at); 