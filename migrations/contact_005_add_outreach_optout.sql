-- Add outreach opt-out column for individual contacts
-- This allows marking specific contacts to be excluded from all outreach communications
ALTER TABLE contacts ADD COLUMN do_not_communicate BOOLEAN NOT NULL DEFAULT FALSE;

-- Add index for performance when filtering opted-out contacts
CREATE INDEX idx_contacts_do_not_communicate ON contacts(do_not_communicate);

-- Add failed underwriting status column to track contacts who failed underwriting
-- This will be used for the reduced outreach frequency feature
ALTER TABLE contacts ADD COLUMN failed_underwriting BOOLEAN NOT NULL DEFAULT FALSE;

-- Add index for failed underwriting status
CREATE INDEX idx_contacts_failed_underwriting ON contacts(failed_underwriting);