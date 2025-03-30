-- Begin transaction for atomic operations
BEGIN TRANSACTION;

-- Identify and remove duplicate emails (keeping only the row with highest ID)
DELETE FROM contacts 
WHERE id NOT IN (
    SELECT MAX(id) 
    FROM contacts 
    GROUP BY LOWER(TRIM(email))
);

-- Add missing unique index for case-insensitive email uniqueness
CREATE UNIQUE INDEX IF NOT EXISTS idx_contacts_email_unique ON contacts(LOWER(TRIM(email)));

-- Commit the transaction
COMMIT;

-- Verify the index was created
PRAGMA index_list(contacts); 