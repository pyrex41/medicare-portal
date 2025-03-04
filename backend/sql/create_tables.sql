-- Create organizations table
CREATE TABLE IF NOT EXISTS organizations (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  slug TEXT NOT NULL UNIQUE,
  subscription_tier TEXT DEFAULT 'basic',
  agent_limit INTEGER DEFAULT 1,
  contact_limit INTEGER DEFAULT 1000,
  stripe_customer_id TEXT,
  stripe_subscription_id TEXT,
  stripe_checkout_session TEXT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create subscription_tiers table
CREATE TABLE IF NOT EXISTS subscription_tiers (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  agent_limit INTEGER NOT NULL,
  contact_limit INTEGER NOT NULL,
  price_monthly INTEGER NOT NULL,
  price_yearly INTEGER NOT NULL,
  features TEXT NOT NULL
);

-- Insert default subscription tiers
INSERT INTO subscription_tiers 
(id, name, agent_limit, contact_limit, price_monthly, price_yearly, features)
VALUES 
('basic', 'Basic', 1, 1000, 2900, 29900, '["Email Support", "Basic Analytics", "1 Agent"]'),
('pro', 'Professional', 5, 10000, 9900, 99900, '["Email & Phone Support", "Advanced Analytics", "Team Collaboration", "API Access", "5 Agents"]'),
('enterprise', 'Enterprise', 10, 20000, 19900, 199900, '["24/7 Priority Support", "Custom Reporting", "Dedicated Account Manager", "White-labeling", "SSO Integration", "10 Agents"]'); 