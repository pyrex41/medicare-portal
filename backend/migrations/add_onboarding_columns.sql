-- Add columns for storing onboarding state

-- Add onboarding_step column (INT with default 0)
ALTER TABLE organizations ADD COLUMN onboarding_step INTEGER DEFAULT 0;

-- Add temp_session_id column (TEXT, nullable)
ALTER TABLE organizations ADD COLUMN temp_session_id TEXT; 