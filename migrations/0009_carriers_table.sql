CREATE TABLE carriers (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE COLLATE NOCASE,
    created_at TEXT DEFAULT (datetime('now')),
    updated_at TEXT DEFAULT (datetime('now'))
);

-- Initial carrier data
INSERT INTO carriers (name) VALUES 
    ('Aetna'),
    ('Humana'),
    ('UnitedHealthcare'),
    ('Cigna'),
    ('Aflac'),
    ('Allstate'),
    ('Mutual of Omaha'),
    ('Ace Chubb');

CREATE TRIGGER update_carriers_timestamp 
AFTER UPDATE ON carriers
BEGIN
    UPDATE carriers 
    SET updated_at = datetime('now')
    WHERE id = NEW.id;
END; 