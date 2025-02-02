import os
from pathlib import Path
from dotenv import load_dotenv
from db.database import db

# Get the project root directory
BASE_DIR = Path(__file__).resolve().parent.parent

# Load environment variables from .env file in project root
load_dotenv(BASE_DIR / '.env')

def run_migrations():
    print(f"Using database URL: {os.getenv('TURSO_DATABASE_URL')}")  # Debug print
    
    # Read and execute migration files
    migrations_dir = BASE_DIR / "migrations"
    for migration_file in sorted(migrations_dir.glob("*.sql")):
        print(f"Running migration: {migration_file.name}")
        with open(migration_file) as f:
            migration_sql = f.read()
            db.execute(migration_sql)

if __name__ == "__main__":
    run_migrations() 