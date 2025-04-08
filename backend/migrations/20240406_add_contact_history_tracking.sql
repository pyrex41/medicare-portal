-- Migration: 20240406_add_contact_history_tracking.sql
-- Description: Adds contact history tracking table for usage-based billing

-- Create table to track unique contacts by email for billing purposes
CREATE TABLE IF NOT EXISTS contact_history (
  id TEXT PRIMARY KEY,
  organization_id TEXT NOT NULL,
  user_id TEXT NOT NULL,
  email TEXT NOT NULL,
  first_name TEXT,
  last_name TEXT,
  first_uploaded TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  last_uploaded TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  status TEXT DEFAULT 'active', -- active, deleted, blocked
  billing_cycle_key TEXT, -- To identify which billing cycle this contact belongs to
  UNIQUE(organization_id, email)
);

-- Add indexes for efficient querying
CREATE INDEX IF NOT EXISTS idx_contact_history_org_email ON contact_history(organization_id, email);
CREATE INDEX IF NOT EXISTS idx_contact_history_org_user ON contact_history(organization_id, user_id);
CREATE INDEX IF NOT EXISTS idx_contact_history_first_uploaded ON contact_history(first_uploaded);
CREATE INDEX IF NOT EXISTS idx_contact_history_billing_cycle ON contact_history(organization_id, billing_cycle_key);

-- Add tracking fields to organizations table if they don't exist
ALTER TABLE organizations ADD COLUMN IF NOT EXISTS contact_count_billing_date TEXT; -- Date when billing cycle starts (e.g., '01' for 1st of month)
ALTER TABLE organizations ADD COLUMN IF NOT EXISTS current_billing_cycle_key TEXT; -- Current billing cycle identifier
ALTER TABLE organizations ADD COLUMN IF NOT EXISTS current_unique_contacts INTEGER DEFAULT 0; -- Count of unique contacts in current cycle

-- Create table to track billing cycle history
CREATE TABLE IF NOT EXISTS billing_cycle_history (
  id TEXT PRIMARY KEY,
  organization_id TEXT NOT NULL,
  cycle_key TEXT NOT NULL, -- Format: YYYY-MM for monthly cycles
  start_date TIMESTAMP NOT NULL,
  end_date TIMESTAMP NOT NULL,
  contact_count INTEGER DEFAULT 0,
  usage_reported BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(organization_id, cycle_key)
);

-- Create a trigger to update last_uploaded timestamp when existing contacts are re-uploaded
CREATE TRIGGER IF NOT EXISTS update_contact_last_uploaded
AFTER UPDATE ON contact_history
BEGIN
  UPDATE contact_history
  SET last_uploaded = CURRENT_TIMESTAMP
  WHERE id = NEW.id AND OLD.email = NEW.email;
END;

-- Create a function to generate a new billing cycle key
CREATE FUNCTION IF NOT EXISTS generate_billing_cycle_key(date_string TEXT)
RETURNS TEXT AS $$
BEGIN
  RETURN to_char(date_string::date, 'YYYY-MM');
END;
$$ LANGUAGE plpgsql; 