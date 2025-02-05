-- Add slug column to organizations table
ALTER TABLE organizations ADD COLUMN slug TEXT DEFAULT NULL;

-- Update existing organizations with a default slug based on name
UPDATE organizations 
SET slug = LOWER(REPLACE(name, ' ', '-'));

-- Make slug NOT NULL after populating data
ALTER TABLE organizations 
ALTER COLUMN slug SET NOT NULL;

CREATE UNIQUE INDEX IF NOT EXISTS idx_organizations_slug 
ON organizations(slug); 