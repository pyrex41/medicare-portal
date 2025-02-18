CREATE TABLE brand_settings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    organization_id INTEGER NOT NULL,
    brand_name TEXT NOT NULL DEFAULT '',
    primary_color TEXT NOT NULL DEFAULT '#6B46C1',
    secondary_color TEXT NOT NULL DEFAULT '#9F7AEA',
    logo_data TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (organization_id) REFERENCES organizations(id),
    UNIQUE(organization_id)
);

CREATE TRIGGER update_brand_settings_timestamp 
AFTER UPDATE ON brand_settings
BEGIN
    UPDATE brand_settings 
    SET updated_at = CURRENT_TIMESTAMP
    WHERE id = NEW.id;
END; 