PRAGMA foreign_keys=off;
BEGIN TRANSACTION;
CREATE TABLE contacts_new (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  email TEXT NOT NULL,
  current_carrier TEXT,
  plan_type TEXT,
  effective_date TEXT NOT NULL,
  birth_date TEXT NOT NULL,
  tobacco_user BOOLEAN NOT NULL,
  gender TEXT NOT NULL,
  state TEXT NOT NULL,
  zip_code TEXT NOT NULL,
  agent_id INTEGER,
  last_emailed TEXT,
  phone_number TEXT NOT NULL DEFAULT '',
  status TEXT NOT NULL DEFAULT '',
  created_at TEXT DEFAULT CURRENT_TIMESTAMP
);
INSERT INTO contacts_new SELECT * FROM contacts;
DROP TABLE contacts;
ALTER TABLE contacts_new RENAME TO contacts;
COMMIT;
PRAGMA foreign_keys=on;