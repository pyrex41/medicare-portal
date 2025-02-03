from pathlib import Path
import sys

# Add parent directory to path to import database
sys.path.append(str(Path(__file__).resolve().parent.parent.parent))

from db.database import Database

def migrate():
    db = Database()
    
    try:
        # Create temporary table with new schema
        db.execute("""
            CREATE TABLE contacts_new (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                first_name TEXT NOT NULL,
                last_name TEXT NOT NULL,
                email TEXT NOT NULL,
                current_carrier TEXT,
                plan_type TEXT,
                effective_date DATE,
                birth_date DATE,
                tobacco_user BOOLEAN DEFAULT FALSE,
                gender TEXT,
                state TEXT,
                zip_code TEXT,
                agent_id INTEGER,
                last_emailed DATETIME,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY (agent_id) REFERENCES agents(id)
            )
        """)
        
        # Copy existing data, setting email to a default value
        db.execute("""
            INSERT INTO contacts_new (
                id, first_name, last_name, email, current_carrier,
                plan_type, effective_date, birth_date, tobacco_user,
                gender, state, zip_code, last_emailed, created_at
            )
            SELECT 
                id, first_name, last_name,
                lower(first_name || '.' || last_name || '@example.com') as email,
                current_carrier, plan_type, effective_date, birth_date,
                tobacco_user, gender, state, zip_code, last_emailed, created_at
            FROM contacts
        """)
        
        # Drop old table
        db.execute("DROP TABLE contacts")
        
        # Rename new table to contacts
        db.execute("ALTER TABLE contacts_new RENAME TO contacts")
        
        # Create indexes
        db.execute("CREATE INDEX idx_contacts_email ON contacts(email)")
        db.execute("CREATE INDEX idx_contacts_agent_id ON contacts(agent_id)")
        
        print("✓ Successfully migrated contacts table")
        
        # Create agents table
        db.execute("""
            CREATE TABLE IF NOT EXISTS agents (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                first_name TEXT NOT NULL,
                last_name TEXT NOT NULL,
                email TEXT NOT NULL UNIQUE,
                phone TEXT NOT NULL,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        db.execute("CREATE INDEX idx_agents_email ON agents(email)")
        
        print("✓ Successfully created agents table")
        
    except Exception as e:
        print(f"✗ Migration failed: {e}")
        raise

if __name__ == "__main__":
    migrate() 