-- Migration: Add GCS/SQLite database support columns to organizations table
-- Date: 2025-01-07
-- Description: Adds support for tracking SQLite databases with GCS storage instead of Turso

-- Add columns for GCS-based SQLite storage
ALTER TABLE organizations ADD COLUMN db_type TEXT DEFAULT 'turso' CHECK(db_type IN ('turso', 'sqlite'));
ALTER TABLE organizations ADD COLUMN gcs_bucket_name TEXT;
ALTER TABLE organizations ADD COLUMN gcs_replica_path TEXT;
ALTER TABLE organizations ADD COLUMN is_db_provisioning INTEGER DEFAULT 0;

-- Add indexes for efficient querying
CREATE INDEX IF NOT EXISTS idx_organizations_db_type ON organizations(db_type);
CREATE INDEX IF NOT EXISTS idx_organizations_gcs_bucket ON organizations(gcs_bucket_name);
CREATE INDEX IF NOT EXISTS idx_organizations_provisioning ON organizations(is_db_provisioning);