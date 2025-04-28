-- Migration script to add AEP request columns to contacts table

-- Add aep_request column (boolean field defaulting to false)
ALTER TABLE contacts ADD COLUMN aep_request BOOLEAN DEFAULT FALSE;

-- Add aep_request_date column (timestamp for when the request was made)
ALTER TABLE contacts ADD COLUMN aep_request_date DATETIME;

-- Create index for faster queries on aep_request status
CREATE INDEX IF NOT EXISTS idx_contacts_aep_request ON contacts(aep_request);

-- Sample query to check if migration worked:
-- SELECT name FROM pragma_table_info('contacts') WHERE name IN ('aep_request', 'aep_request_date'); 