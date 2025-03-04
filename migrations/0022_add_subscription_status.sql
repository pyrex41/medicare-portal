-- Add subscription status fields to organizations table
ALTER TABLE organizations ADD COLUMN subscription_status TEXT NOT NULL DEFAULT 'active';
ALTER TABLE organizations ADD COLUMN billing_cycle_end DATETIME;
ALTER TABLE organizations ADD COLUMN trial_end_date DATETIME;
ALTER TABLE organizations ADD COLUMN payment_failure_count INTEGER NOT NULL DEFAULT 0;
ALTER TABLE organizations ADD COLUMN last_payment_date DATETIME;
ALTER TABLE organizations ADD COLUMN extra_agents INTEGER NOT NULL DEFAULT 0;
ALTER TABLE organizations ADD COLUMN extra_contacts INTEGER NOT NULL DEFAULT 0;

-- Create a view for organization status
CREATE VIEW IF NOT EXISTS organization_status AS
SELECT 
    o.id,
    o.slug,
    o.name,
    o.subscription_tier,
    o.subscription_status,
    o.agent_limit,
    o.contact_limit,
    o.extra_agents,
    o.extra_contacts,
    o.billing_cycle_end,
    o.payment_failure_count,
    (SELECT COUNT(*) FROM agents WHERE organization_id = o.id) as current_agent_count,
    (SELECT COUNT(*) FROM contacts WHERE organization_id = o.id) as current_contact_count,
    CASE 
        WHEN o.subscription_status != 'active' THEN 'inactive'
        WHEN (SELECT COUNT(*) FROM agents WHERE organization_id = o.id) > (o.agent_limit + o.extra_agents) THEN 'agent_limit_exceeded'
        WHEN (SELECT COUNT(*) FROM contacts WHERE organization_id = o.id) > (o.contact_limit + o.extra_contacts) THEN 'contact_limit_exceeded'
        ELSE 'good_standing'
    END as account_status
FROM organizations o;