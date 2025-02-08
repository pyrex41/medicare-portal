-- Rename existing settings column to org_settings
ALTER TABLE organizations 
RENAME COLUMN settings TO org_settings;

-- Create agent_settings table
CREATE TABLE agent_settings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    agent_id INTEGER NOT NULL REFERENCES users(id),
    inherit_org_settings BOOLEAN DEFAULT 1,
    settings TEXT NOT NULL DEFAULT '{}',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(agent_id)
);

-- Trigger to update updated_at
CREATE TRIGGER update_agent_settings_timestamp
AFTER UPDATE ON agent_settings
BEGIN
    UPDATE agent_settings SET updated_at = CURRENT_TIMESTAMP
    WHERE id = NEW.id;
END; 