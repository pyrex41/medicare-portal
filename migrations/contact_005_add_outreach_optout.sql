-- Add outreach opt-out column for individual contacts
-- This allows marking specific contacts to be excluded from all outreach communications
ALTER TABLE contacts ADD COLUMN do_not_communicate BOOLEAN NOT NULL DEFAULT FALSE;

-- Add index for performance when filtering opted-out contacts
CREATE INDEX IF NOT EXISTS idx_contacts_do_not_communicate ON contacts(do_not_communicate);

-- Note: Failed underwriting status is derived from contact_events table
-- where event_type = 'eligibility_answered' and metadata JSON contains "has_medical_conditions": true 