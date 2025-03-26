-- Create waitlist table
CREATE TABLE IF NOT EXISTS waitlist (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    email TEXT NOT NULL UNIQUE,
    phone TEXT NOT NULL,
    num_agents INTEGER NOT NULL,
    book_size INTEGER NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Create index on email for faster lookups
CREATE INDEX IF NOT EXISTS idx_waitlist_email ON waitlist(email);

-- Create trigger to update updated_at timestamp
CREATE TRIGGER IF NOT EXISTS tg_waitlist_updated_at 
    AFTER UPDATE ON waitlist
    FOR EACH ROW
BEGIN
    UPDATE waitlist 
    SET updated_at = CURRENT_TIMESTAMP
    WHERE id = NEW.id;
END; 