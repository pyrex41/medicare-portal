-- First, let's check if the table exists
SELECT sql FROM sqlite_master WHERE type='table' AND name='users';

-- Drop users_new if it exists from a failed migration
DROP TABLE IF EXISTS users_new;

-- Enable foreign key support
PRAGMA foreign_keys=OFF;

-- Create new table with updated role constraint
CREATE TABLE users_new (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT UNIQUE NOT NULL,
    organization_id INTEGER NOT NULL,
    role TEXT CHECK(role IN ('admin', 'agent', 'admin_agent')) NOT NULL,
    last_login DATETIME,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    first_name TEXT NOT NULL DEFAULT '',
    last_name TEXT NOT NULL DEFAULT '',
    is_active INTEGER NOT NULL DEFAULT 1,
    phone TEXT NOT NULL DEFAULT '',
    FOREIGN KEY (organization_id) REFERENCES organizations(id)
);

-- Copy the data
INSERT INTO users_new 
SELECT id, email, organization_id, role, last_login, created_at, 
       first_name, last_name, is_active, phone
FROM users;

-- Drop the old table
DROP TABLE users;

-- Rename the new table
ALTER TABLE users_new RENAME TO users;

-- Re-enable foreign key support
PRAGMA foreign_keys=ON; 