-- Add default_agent_id to organizations table
ALTER TABLE organizations ADD COLUMN default_agent_id INTEGER REFERENCES users(id);

-- Add index for faster lookups
CREATE INDEX IF NOT EXISTS idx_organizations_default_agent ON organizations(default_agent_id); 