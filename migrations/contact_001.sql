-- Add new columns with defaults
ALTER TABLE contacts ADD COLUMN phone_number TEXT NOT NULL DEFAULT '';
ALTER TABLE contacts ADD COLUMN status TEXT NOT NULL DEFAULT '';