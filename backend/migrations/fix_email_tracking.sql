PRAGMA foreign_keys=off;

-- Rename the old table
ALTER TABLE email_send_tracking RENAME TO email_send_tracking_old;

-- Recreate the table with the correct foreign key
CREATE TABLE email_send_tracking (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  org_id INTEGER NOT NULL,
  contact_id INTEGER NOT NULL,
  email_type TEXT NOT NULL,
  scheduled_date TEXT NOT NULL,
  send_status TEXT NOT NULL CHECK(send_status IN ('pending', 'processing', 'accepted', 'delivered', 'sent', 'deferred', 'bounced', 'dropped', 'failed', 'skipped')) DEFAULT 'pending',
  send_mode TEXT NOT NULL CHECK(send_mode IN ('test', 'production')) DEFAULT 'test',
  test_email TEXT,
  send_attempt_count INTEGER NOT NULL DEFAULT 0,
  last_attempt_date TEXT,
  last_error TEXT,
  batch_id TEXT NOT NULL,
  message_id TEXT,
  delivery_status TEXT,
  status_checked_at TEXT,
  status_details TEXT,
  created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (contact_id) REFERENCES contacts(id) ON DELETE CASCADE
);

-- Copy data from old table (if needed)
INSERT INTO email_send_tracking (
  id, org_id, contact_id, email_type, scheduled_date, send_status, send_mode, test_email, send_attempt_count, last_attempt_date, last_error, batch_id, message_id, delivery_status, status_checked_at, status_details, created_at, updated_at
)
SELECT
  id, org_id, contact_id, email_type, scheduled_date, send_status, send_mode, test_email, send_attempt_count, last_attempt_date, last_error, batch_id, message_id, delivery_status, status_checked_at, status_details, created_at, updated_at
FROM email_send_tracking_old;

DROP TABLE email_send_tracking_old;

PRAGMA foreign_keys=on;
