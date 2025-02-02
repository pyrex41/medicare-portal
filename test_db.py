import os
from pathlib import Path
from dotenv import load_dotenv
import time
from datetime import date

# Load environment variables from .env file
ENV_FILE = Path(__file__).resolve().parent / '.env'
print(f"Looking for .env file at: {ENV_FILE}")

if not ENV_FILE.exists():
    raise FileNotFoundError(f".env file not found at {ENV_FILE}")

load_dotenv(ENV_FILE, override=True)

# Import after loading env vars
import libsql_experimental as libsql

def test_connection():
    # Get credentials
    db_url = os.getenv("TURSO_DATABASE_URL")
    auth_token = os.getenv("TURSO_AUTH_TOKEN")
    
    print(f"\nAttempting to connect to: {db_url}")
    
    try:
        # Try to connect
        conn = libsql.connect(db_url, auth_token=auth_token)
        print("✓ Connection successful!")
        
        # Check table schema
        print("\nChecking database schema:")
        schema = conn.execute("SELECT sql FROM sqlite_master WHERE type='table' AND name='contacts'").fetchone()
        if schema:
            print(f"✓ Found contacts table schema:\n{schema[0]}")
        else:
            print("✗ contacts table not found!")
            print("\nCreating contacts table...")
            conn.execute("""
                CREATE TABLE IF NOT EXISTS contacts (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    first_name TEXT NOT NULL,
                    last_name TEXT NOT NULL,
                    current_carrier TEXT,
                    plan_type TEXT,
                    effective_date DATE,
                    birth_date DATE,
                    tobacco_user BOOLEAN DEFAULT FALSE,
                    gender TEXT,
                    state TEXT,
                    zip_code TEXT,
                    last_emailed DATETIME,
                    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
                )
            """)
            conn.commit()
            print("✓ Table created!")
        
        # Generate unique test data
        test_first_name = f"Test_{int(time.time())}"
        test_last_name = "User"
        today = date.today().isoformat()
        
        print(f"\nInserting test record: {test_first_name} {test_last_name}")
        
        # Try to insert a test record with all required fields
        conn.execute("""
            INSERT INTO contacts (
                first_name, last_name, current_carrier, plan_type,
                effective_date, birth_date, tobacco_user, gender,
                state, zip_code
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            test_first_name,    # first_name
            test_last_name,     # last_name
            "Test Carrier",     # current_carrier
            "Test Plan",        # plan_type
            today,              # effective_date
            "1970-01-01",      # birth_date
            False,             # tobacco_user
            "M",               # gender
            "CA",              # state
            "12345"            # zip_code
        ))
        conn.commit()
        
        # Verify the insert by reading it back
        result = conn.execute(
            "SELECT first_name, last_name, current_carrier, plan_type FROM contacts WHERE first_name = ?",
            (test_first_name,)
        ).fetchone()
        
        if result:
            print(f"✓ Test insert verified! Found record: {result}")
        else:
            print("✗ Test insert failed - couldn't read back the inserted record")
            
        # Show total count of records
        count = conn.execute("SELECT COUNT(*) FROM contacts").fetchone()[0]
        print(f"\nTotal records in contacts table: {count}")
        
        # Show last 3 records
        print("\nLast 3 records:")
        records = conn.execute("""
            SELECT first_name, last_name, current_carrier, plan_type, created_at 
            FROM contacts 
            ORDER BY created_at DESC 
            LIMIT 3
        """).fetchall()
        for record in records:
            print(f"  {record}")
        
    except Exception as e:
        print(f"✗ Error: {e}")
        print(f"Error type: {type(e)}")

if __name__ == "__main__":
    test_connection() 