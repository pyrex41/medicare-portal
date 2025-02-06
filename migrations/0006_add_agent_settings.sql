CREATE TABLE agent_settings (
    id SERIAL PRIMARY KEY,
    agent_id INTEGER NOT NULL REFERENCES users(id),
    state_licenses TEXT[], -- Array of state codes
    carrier_contracts TEXT[], -- Array of carrier ids
    state_carrier_settings JSONB, -- Stores state->carrier->settings mapping
    email_send_birthday BOOLEAN DEFAULT false,
    email_send_policy_anniversary BOOLEAN DEFAULT false,
    email_send_aep BOOLEAN DEFAULT false,
    smart_send_enabled BOOLEAN DEFAULT false,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(agent_id)
);

-- Trigger to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER update_agent_settings_updated_at
    BEFORE UPDATE ON agent_settings
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column(); 