-- Set default value for is_active to 1 in users table
ALTER TABLE users ADD COLUMN temp_is_active INTEGER NOT NULL DEFAULT 1;
UPDATE users SET temp_is_active = is_active;
ALTER TABLE users DROP COLUMN is_active;
ALTER TABLE users RENAME COLUMN temp_is_active TO is_active;

-- Update existing users to be active
UPDATE users SET is_active = 1 WHERE email = 'reuben.brooks@medicaremax.ai'; 