from pathlib import Path
from db.database import db

def run_migrations():
    # Read and execute migration files
    migrations_dir = Path("migrations")
    for migration_file in sorted(migrations_dir.glob("*.sql")):
        print(f"Running migration: {migration_file.name}")
        with open(migration_file) as f:
            migration_sql = f.read()
            db.execute(migration_sql)

if __name__ == "__main__":
    run_migrations() 