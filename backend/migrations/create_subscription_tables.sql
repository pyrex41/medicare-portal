-- Create subscriptions table for tracking Stripe subscriptions
CREATE TABLE IF NOT EXISTS subscriptions (
  id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL,
  stripe_subscription_id TEXT NOT NULL,
  stripe_customer_id TEXT NOT NULL,
  stripe_usage_item_id TEXT,  -- For usage-based billing
  status TEXT NOT NULL,
  current_contact_count INTEGER DEFAULT 0,  -- Current number of contacts for this user
  current_period_start TIMESTAMP,
  current_period_end TIMESTAMP,
  cancel_at_period_end BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY(user_id) REFERENCES users(id)
);

-- Track contact count changes for billing and auditing
CREATE TABLE IF NOT EXISTS contact_count_history (
  id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL,
  count INTEGER NOT NULL,
  previous_count INTEGER,
  change_type TEXT NOT NULL, -- 'add', 'remove', 'initial', etc.
  logged_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY(user_id) REFERENCES users(id)
);

-- Track contact counts for each user
CREATE TABLE IF NOT EXISTS contact_counts (
  user_id TEXT PRIMARY KEY,
  count INTEGER NOT NULL DEFAULT 0,
  last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY(user_id) REFERENCES users(id)
);

-- Add auto_upgrade_limit column to users table
ALTER TABLE users ADD COLUMN IF NOT EXISTS auto_upgrade_limit INTEGER DEFAULT 0;

-- Create index on user_id for subscriptions
CREATE INDEX IF NOT EXISTS idx_subscriptions_user_id ON subscriptions(user_id);

-- Create index on status for quick filtering of active subscriptions
CREATE INDEX IF NOT EXISTS idx_subscriptions_status ON subscriptions(status); 