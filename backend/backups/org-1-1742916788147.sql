CREATE TABLE contacts (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            first_name TEXT NOT NULL,
            last_name TEXT NOT NULL,
            email TEXT NOT NULL,
            current_carrier TEXT NOT NULL,
            plan_type TEXT NOT NULL,
            effective_date TEXT NOT NULL,
            birth_date TEXT NOT NULL,
            tobacco_user INTEGER NOT NULL,
            gender TEXT NOT NULL,
            state TEXT NOT NULL,
            zip_code TEXT NOT NULL,
            agent_id INTEGER,
            last_emailed DATETIME,
            phone_number TEXT NOT NULL DEFAULT '',
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
          );

CREATE UNIQUE INDEX idx_contacts_email_unique ON contacts(LOWER(TRIM(email)));
