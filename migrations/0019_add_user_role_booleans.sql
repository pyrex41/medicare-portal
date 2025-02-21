-- First, let's check if the table exists
SELECT sql FROM sqlite_master WHERE type='table' AND name='users';

-- Drop users_new if it exists from a failed migration
DROP TABLE IF EXISTS users_new;

-- Enable foreign key support
PRAGMA foreign_keys=OFF;

-- Create new table with is_admin and is_agent columns
CREATE TABLE users_new (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT UNIQUE NOT NULL,
    organization_id INTEGER NOT NULL,
    is_admin BOOLEAN NOT NULL DEFAULT 0,
    is_agent BOOLEAN NOT NULL DEFAULT 0,
    last_login DATETIME,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    first_name TEXT NOT NULL DEFAULT '',
    last_name TEXT NOT NULL DEFAULT '',
    is_active INTEGER NOT NULL DEFAULT 1,
    phone TEXT NOT NULL DEFAULT '',
    FOREIGN KEY (organization_id) REFERENCES organizations(id)
);

-- Copy the data with role conversion
INSERT INTO users_new 
SELECT 
    id, 
    email, 
    organization_id,
    CASE 
        WHEN role = 'admin' THEN 1
        WHEN role = 'admin_agent' THEN 1
        ELSE 0
    END as is_admin,
    CASE 
        WHEN role = 'agent' THEN 1
        WHEN role = 'admin_agent' THEN 1
        ELSE 0
    END as is_agent,
    last_login,
    created_at,
    first_name,
    last_name,
    is_active,
    phone
FROM users;

-- Drop the old table
DROP TABLE users;

-- Rename the new table
ALTER TABLE users_new RENAME TO users;

-- Create indexes for performance
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_users_org_id ON users(organization_id);

-- Re-enable foreign key support
PRAGMA foreign_keys=ON; 