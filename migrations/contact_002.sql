-- Add contact_events table for generic event logging
CREATE TABLE IF NOT EXISTS contact_events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    contact_id INTEGER NOT NULL,
    event_type TEXT NOT NULL,
    metadata TEXT, -- JSON field for additional event-specific data
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (contact_id) REFERENCES contacts(id)
);

CREATE INDEX IF NOT EXISTS idx_contact_events_contact_id ON contact_events(contact_id);
CREATE INDEX IF NOT EXISTS idx_contact_events_type ON contact_events(event_type);


DROP TABLE IF EXISTS contact_requests;

