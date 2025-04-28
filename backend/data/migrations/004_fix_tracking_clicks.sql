-- Add quote_id column if it doesn't exist
ALTER TABLE tracking_clicks ADD COLUMN quote_id TEXT;

-- Create a new table with the desired structure
CREATE TABLE tracking_clicks_new (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  tracking_id TEXT NOT NULL,
  contact_id INTEGER,
  quote_id TEXT,
  clicked_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
);

-- Copy data from the old table to the new table
INSERT INTO tracking_clicks_new (id, tracking_id, contact_id, quote_id, clicked_at)
SELECT id, tracking_id, contact_id, quote_id, clicked_at FROM tracking_clicks;

-- Drop the old table
DROP TABLE tracking_clicks;

-- Rename the new table to the original name
ALTER TABLE tracking_clicks_new RENAME TO tracking_clicks;

-- Recreate the indexes
CREATE INDEX idx_tracking_clicks_tracking_id ON tracking_clicks(tracking_id);
CREATE INDEX idx_tracking_clicks_contact_id ON tracking_clicks(contact_id);
CREATE INDEX idx_tracking_clicks_clicked_at ON tracking_clicks(clicked_at);