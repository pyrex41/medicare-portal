CREATE TABLE IF NOT EXISTS contacts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    current_carrier TEXT NOT NULL,
    plan_type TEXT NOT NULL,
    effective_date DATE NOT NULL,
    birth_date DATE NOT NULL,
    tobacco_user BOOLEAN NOT NULL DEFAULT FALSE,
    gender TEXT CHECK(gender IN ('M', 'F')) NOT NULL,
    state TEXT NOT NULL,
    zip_code TEXT NOT NULL,
    last_emailed_date DATE,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Trigger to update the updated_at timestamp
CREATE TRIGGER IF NOT EXISTS update_contacts_timestamp 
AFTER UPDATE ON contacts
BEGIN
    UPDATE contacts SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END; 