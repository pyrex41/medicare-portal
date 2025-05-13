CREATE TABLE IF NOT EXISTS deleted_contacts (
    original_contact_id INTEGER NOT NULL, -- The ID from the original 'contacts' table
    first_name TEXT, -- Made nullable as they might be empty in original
    last_name TEXT,  -- Made nullable
    email TEXT NOT NULL, -- Crucial for billable count
    current_carrier TEXT,
    plan_type TEXT,
    effective_date TEXT,
    birth_date TEXT,
    tobacco_user INTEGER,
    gender TEXT,
    state TEXT,
    zip_code TEXT,
    agent_id INTEGER,
    phone_number TEXT,
    -- Include any other fields from 'contacts' you want to preserve historically
    deleted_at DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL
    -- No primary key on original_contact_id + email to allow multiple deletions if a user is re-added and deleted again
    -- though the billing logic will only count the most recent deletion within 12 months.
    -- A new unique ID for this table might be useful if needed:
    -- id TEXT PRIMARY KEY DEFAULT (hex(randomblob(16)))
);

CREATE INDEX IF NOT EXISTS idx_deleted_contacts_email ON deleted_contacts (LOWER(TRIM(email)));
CREATE INDEX IF NOT EXISTS idx_deleted_contacts_deleted_at ON deleted_contacts (deleted_at);
CREATE INDEX IF NOT EXISTS idx_deleted_contacts_original_id ON deleted_contacts (original_contact_id);
