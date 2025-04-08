-- Migration: Add usage-based subscription tracking fields to organizations table
-- Date: 2025-04-07

-- Add fields for usage-based subscription tracking
ALTER TABLE organizations ADD COLUMN stripe_usage_item_id TEXT;
ALTER TABLE organizations ADD COLUMN current_contact_count INTEGER DEFAULT 0;
ALTER TABLE organizations ADD COLUMN last_usage_report_time DATETIME;
ALTER TABLE organizations ADD COLUMN usage_meter_id TEXT;
ALTER TABLE organizations ADD COLUMN auto_upgrade_limit INTEGER DEFAULT 0;

-- Add an index for more efficient subscription lookups
CREATE INDEX IF NOT EXISTS idx_organizations_subscription ON organizations (stripe_subscription_id); 