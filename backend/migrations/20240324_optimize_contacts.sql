-- Optimize contacts table performance

-- Add missing columns if they don't exist
SELECT CASE 
    WHEN NOT EXISTS(SELECT 1 FROM pragma_table_info('contacts') WHERE name = 'created_at') 
    THEN 'ALTER TABLE contacts ADD COLUMN created_at DATETIME DEFAULT CURRENT_TIMESTAMP;'
    ELSE 'SELECT 1;'
END AS sql_to_run;

SELECT CASE 
    WHEN NOT EXISTS(SELECT 1 FROM pragma_table_info('contacts') WHERE name = 'updated_at') 
    THEN 'ALTER TABLE contacts ADD COLUMN updated_at DATETIME DEFAULT CURRENT_TIMESTAMP;'
    ELSE 'SELECT 1;'
END AS sql_to_run;

SELECT CASE 
    WHEN NOT EXISTS(SELECT 1 FROM pragma_table_info('contacts') WHERE name = 'last_emailed') 
    THEN 'ALTER TABLE contacts ADD COLUMN last_emailed DATETIME;'
    ELSE 'SELECT 1;'
END AS sql_to_run;

-- Add core indexes that should work on all databases
-- Email is guaranteed to exist as it's a core field
CREATE INDEX IF NOT EXISTS idx_contacts_email ON contacts (email COLLATE NOCASE);

-- Agent ID is guaranteed to exist as it's a core field
CREATE INDEX IF NOT EXISTS idx_contacts_agent_id ON contacts (agent_id);

-- Add carrier index which should exist as it's a core field
CREATE INDEX IF NOT EXISTS idx_contacts_agent_carrier ON contacts (agent_id, current_carrier COLLATE NOCASE);

-- Add state index which should exist as it's a core field
CREATE INDEX IF NOT EXISTS idx_contacts_state ON contacts (state COLLATE NOCASE);

-- Add plan type index which should exist as it's a core field
CREATE INDEX IF NOT EXISTS idx_contacts_plan_type ON contacts (plan_type COLLATE NOCASE);

-- Add tobacco user index which should exist as it's a core field
CREATE INDEX IF NOT EXISTS idx_contacts_tobacco_user ON contacts (tobacco_user);

-- Add timestamp indexes only if columns exist
SELECT CASE 
    WHEN EXISTS(SELECT 1 FROM pragma_table_info('contacts') WHERE name = 'created_at') 
    THEN 'CREATE INDEX IF NOT EXISTS idx_contacts_created_at ON contacts (created_at);'
    ELSE 'SELECT 1;'
END AS sql_to_run;

SELECT CASE 
    WHEN EXISTS(SELECT 1 FROM pragma_table_info('contacts') WHERE name = 'updated_at') 
    THEN 'CREATE INDEX IF NOT EXISTS idx_contacts_updated_at ON contacts (updated_at);'
    ELSE 'SELECT 1;'
END AS sql_to_run;

SELECT CASE 
    WHEN EXISTS(SELECT 1 FROM pragma_table_info('contacts') WHERE name = 'last_emailed') 
    THEN 'CREATE INDEX IF NOT EXISTS idx_contacts_last_emailed ON contacts (last_emailed);'
    ELSE 'SELECT 1;'
END AS sql_to_run;

-- Add date field indexes which should exist as they're core fields
CREATE INDEX IF NOT EXISTS idx_contacts_birth_date ON contacts (birth_date);
CREATE INDEX IF NOT EXISTS idx_contacts_effective_date ON contacts (effective_date);

-- Create a view for contact statistics that works with core fields only
CREATE VIEW IF NOT EXISTS v_contact_stats AS
WITH contact_metrics AS (
    SELECT 
        agent_id,
        current_carrier,
        state,
        COUNT(*) as total_contacts,
        SUM(CASE WHEN tobacco_user = 1 THEN 1 ELSE 0 END) as tobacco_users,
        SUM(CASE WHEN plan_type IS NOT NULL THEN 1 ELSE 0 END) as with_plan_type
    FROM contacts
    GROUP BY agent_id, current_carrier, state
)
SELECT 
    cm.*,
    (SELECT COUNT(DISTINCT current_carrier) FROM contacts WHERE agent_id = cm.agent_id) as unique_carriers,
    (SELECT COUNT(DISTINCT state) FROM contacts WHERE agent_id = cm.agent_id) as unique_states
FROM contact_metrics cm; 