-- Add AEP request columns to contacts table
ALTER TABLE contacts ADD COLUMN aep_request BOOLEAN DEFAULT FALSE;
ALTER TABLE contacts ADD COLUMN aep_request_date DATETIME; 