-- Update any existing users that might have incorrect is_admin/is_agent values
UPDATE users
SET is_admin = 1, is_agent = 1
WHERE is_admin = 0 AND is_agent = 0;

-- Make sure admin_agent users have both flags set
UPDATE users
SET is_admin = 1, is_agent = 1
WHERE is_admin = 1 OR is_agent = 1;

-- Set admin-only users
UPDATE users
SET is_admin = 1, is_agent = 0
WHERE email LIKE '%@medicaremax.ai';

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_users_is_admin ON users(is_admin);
CREATE INDEX IF NOT EXISTS idx_users_is_agent ON users(is_agent); 