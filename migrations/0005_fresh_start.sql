-- Drop all existing tables
DROP TABLE IF EXISTS sessions;
DROP TABLE IF EXISTS magic_links;
DROP TABLE IF EXISTS contacts;
DROP TABLE IF EXISTS agents;
DROP TABLE IF EXISTS users;
DROP TABLE IF EXISTS subscription_tiers;
DROP TABLE IF EXISTS organizations;

-- Create fresh tables
CREATE TABLE organizations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    slug TEXT NOT NULL,
    subscription_tier TEXT NOT NULL DEFAULT 'basic',
    agent_limit INTEGER NOT NULL DEFAULT 5,
    contact_limit INTEGER NOT NULL DEFAULT 100,
    stripe_customer_id TEXT,
    stripe_subscription_id TEXT,
    settings JSON,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(slug)
);

CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT UNIQUE NOT NULL,
    organization_id INTEGER NOT NULL,
    role TEXT CHECK(role IN ('admin', 'agent')) NOT NULL,
    is_active BOOLEAN DEFAULT true,
    last_login DATETIME,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (organization_id) REFERENCES organizations(id)
);

CREATE TABLE magic_links (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    token TEXT UNIQUE NOT NULL,
    email TEXT NOT NULL,
    organization_id INTEGER NOT NULL,
    expires_at DATETIME NOT NULL,
    used_at DATETIME,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (organization_id) REFERENCES organizations(id)
);

CREATE TABLE agents (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    organization_id INTEGER NOT NULL,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email TEXT NOT NULL,
    phone TEXT NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (organization_id) REFERENCES organizations(id),
    UNIQUE(email, organization_id)
);

CREATE TABLE contacts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    organization_id INTEGER NOT NULL,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email TEXT NOT NULL,
    current_carrier TEXT,
    plan_type TEXT,
    effective_date DATE,
    birth_date DATE,
    tobacco_user BOOLEAN DEFAULT FALSE,
    gender TEXT,
    state TEXT,
    zip_code TEXT,
    agent_id INTEGER,
    last_emailed DATETIME,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (agent_id) REFERENCES agents(id),
    FOREIGN KEY (organization_id) REFERENCES organizations(id)
);

CREATE TABLE subscription_tiers (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    agent_limit INTEGER NOT NULL,
    contact_limit INTEGER NOT NULL,
    price_monthly INTEGER NOT NULL,
    price_yearly INTEGER NOT NULL,
    features JSON NOT NULL
);

CREATE TABLE sessions (
    id TEXT PRIMARY KEY,
    user_id INTEGER NOT NULL,
    expires_at DATETIME NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Insert default subscription tiers
INSERT INTO subscription_tiers (id, name, agent_limit, contact_limit, price_monthly, price_yearly, features)
VALUES 
    ('basic', 'Basic', 5, 100, 2900, 29000, '{"emailAutomation": false, "customBranding": false}'),
    ('pro', 'Professional', 20, 1000, 7900, 79000, '{"emailAutomation": true, "customBranding": false}'),
    ('enterprise', 'Enterprise', 100, 10000, 19900, 199000, '{"emailAutomation": true, "customBranding": true}');

-- Create demo organization
INSERT INTO organizations (name, slug, subscription_tier)
VALUES ('Demo Organization', 'demo-org', 'basic'); 