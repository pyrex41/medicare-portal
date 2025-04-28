-- Migration: 003_add_tracking_clicks.sql
-- Description: Adds tracking_clicks table for link click tracking
-- Date: 2023-08-30

-- Create the tracking_clicks table if it doesn't exist
CREATE TABLE IF NOT EXISTS tracking_clicks (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  tracking_id TEXT NOT NULL,
  path TEXT NOT NULL,
  query TEXT,
  contact_id INTEGER,
  ip_address TEXT,
  user_agent TEXT, 
  referrer TEXT,
  clicked_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
);

-- Create indexes for efficient querying
CREATE INDEX IF NOT EXISTS idx_tracking_clicks_tracking_id ON tracking_clicks(tracking_id);
CREATE INDEX IF NOT EXISTS idx_tracking_clicks_contact_id ON tracking_clicks(contact_id);
CREATE INDEX IF NOT EXISTS idx_tracking_clicks_clicked_at ON tracking_clicks(clicked_at);

-- Add versioning information to track that this migration has been applied
