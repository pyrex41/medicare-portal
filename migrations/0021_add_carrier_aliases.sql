-- Add aliases column to carriers table
ALTER TABLE carriers ADD COLUMN aliases JSON DEFAULT '[]';

-- Update existing carriers with known aliases
UPDATE carriers 
SET aliases = '["UHC", "UnitedHealthcare", "AARP"]'
WHERE name = 'United Healthcare';

UPDATE carriers 
SET aliases = '["MOO"]'
WHERE name = 'Mutual of Omaha';

UPDATE carriers 
SET aliases = '["Ace", "Chubb"]'
WHERE name = 'Ace Chubb'; 