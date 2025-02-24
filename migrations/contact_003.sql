-- Create eligibility_answers table to track responses
CREATE TABLE IF NOT EXISTS eligibility_answers (
    id INTEGER PRIMARY KEY,
    contact_id INTEGER NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    answers TEXT NOT NULL, -- Format: {"1": true, "2": false, "3": true}
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
);

-- Create index for faster lookups by contact
CREATE INDEX IF NOT EXISTS eligibility_answers_contact_id_idx ON eligibility_answers(contact_id);