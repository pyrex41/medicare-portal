CREATE TABLE guaranteed_issue_recommendations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    state TEXT NOT NULL COLLATE NOCASE,
    carrier TEXT NOT NULL COLLATE NOCASE,
    created_at TEXT DEFAULT (datetime('now')),
    updated_at TEXT DEFAULT (datetime('now')),
    UNIQUE(state, carrier)
);

CREATE TRIGGER update_gi_recommendations_timestamp 
AFTER UPDATE ON guaranteed_issue_recommendations
BEGIN
    UPDATE guaranteed_issue_recommendations 
    SET updated_at = datetime('now')
    WHERE id = NEW.id;
END; 