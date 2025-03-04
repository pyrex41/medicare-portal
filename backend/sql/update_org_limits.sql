-- Update organization tier limits
-- Basic tier: 1 agent, 1,000 contacts
UPDATE organizations 
SET agent_limit = 1, contact_limit = 1000 
WHERE subscription_tier = 'basic';

-- Pro tier: 5 agents, 10,000 contacts
UPDATE organizations 
SET agent_limit = 5, contact_limit = 10000 
WHERE subscription_tier = 'pro';

-- Enterprise tier: 10 agents, 20,000 contacts  
UPDATE organizations 
SET agent_limit = 10, contact_limit = 20000 
WHERE subscription_tier = 'enterprise';

-- Make sure subscription_tiers table is updated with correct limits
INSERT OR REPLACE INTO subscription_tiers 
(id, name, agent_limit, contact_limit, price_monthly, price_yearly, features)
VALUES 
('basic', 'Basic', 1, 1000, 2900, 29900, '["Email Support", "Basic Analytics", "1 Agent"]'),
('pro', 'Professional', 5, 10000, 9900, 99900, '["Email & Phone Support", "Advanced Analytics", "Team Collaboration", "API Access", "5 Agents"]'),
('enterprise', 'Enterprise', 10, 20000, 19900, 199900, '["24/7 Priority Support", "Custom Reporting", "Dedicated Account Manager", "White-labeling", "SSO Integration", "10 Agents"]'); 