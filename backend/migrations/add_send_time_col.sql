-- Migration to add scheduled_send_time column to email_schedules table
-- This allows for separating the scheduling time from the actual send time

-- Check if email_schedules table exists
CREATE TABLE IF NOT EXISTS email_schedules (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    contact_id INTEGER NOT NULL,
    email_type TEXT NOT NULL,
    scheduled_send_date TEXT NOT NULL,
    status TEXT NOT NULL DEFAULT 'pre-scheduled',
    skip_reason TEXT,
    created_at TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at TEXT NOT NULL DEFAULT (datetime('now')),
    tracking_id TEXT,
    FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
);

-- Add scheduled_send_time column if it doesn't exist
PRAGMA foreign_keys=off;
BEGIN TRANSACTION;

-- Create a temporary table with the new schema
CREATE TABLE email_schedules_new (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    contact_id INTEGER NOT NULL,
    email_type TEXT NOT NULL,
    scheduled_send_date TEXT NOT NULL,
    scheduled_send_time TEXT NOT NULL DEFAULT '08:30:00',
    status TEXT NOT NULL DEFAULT 'scheduled',
    skip_reason TEXT,
    created_at TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at TEXT NOT NULL DEFAULT (datetime('now')),
    batch_id TEXT,
    FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
);

-- Copy data from the old table to the new table
INSERT INTO email_schedules_new 
    (id, contact_id, email_type, scheduled_send_date, status, skip_reason, created_at, updated_at, batch_id)
SELECT 
    id, contact_id, email_type, scheduled_send_date, status, skip_reason, created_at, updated_at, batch_id 
FROM email_schedules;

-- Drop the old table
DROP TABLE email_schedules;

-- Rename the new table to the original name
ALTER TABLE email_schedules_new RENAME TO email_schedules;

-- Add an index to improve query performance for scheduled email lookups
CREATE INDEX IF NOT EXISTS idx_email_schedules_date_time_status ON email_schedules 
    (scheduled_send_date, scheduled_send_time, status);

-- Create a unique index on contact_id, email_type, and scheduled_send_date
-- This prevents duplicate scheduling for the same contact, email type, and date
CREATE UNIQUE INDEX IF NOT EXISTS idx_email_schedules_unique ON email_schedules 
    (contact_id, email_type, scheduled_send_date);

COMMIT;
PRAGMA foreign_keys=on;