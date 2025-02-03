import os
from dotenv import load_dotenv
import libsql_experimental as libsql
from pathlib import Path

# Load environment variables from .env file
ENV_FILE = Path(__file__).resolve().parent.parent / '.env'
load_dotenv(ENV_FILE, override=True)

class Database:
    def __init__(self):
        self.db_url = os.getenv("TURSO_DATABASE_URL")
        self.auth_token = os.getenv("TURSO_AUTH_TOKEN")
        if not self.db_url or not self.auth_token:
            raise ValueError("Missing database credentials in .env file")
        
        # Ensure tables exist
        self.init_tables()

    def init_tables(self):
        """Initialize database tables if they don't exist"""
        self.execute("""
            CREATE TABLE IF NOT EXISTS agents (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                first_name TEXT NOT NULL,
                last_name TEXT NOT NULL,
                email TEXT NOT NULL UNIQUE,
                phone TEXT NOT NULL,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        # Note: We can't modify existing tables with ALTER in SQLite
        # So we'll handle this in the migration script
        self.execute("""
            CREATE TABLE IF NOT EXISTS contacts (
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

    def get_connection(self):
        return libsql.connect(
            self.db_url,
            auth_token=self.auth_token
        )

    def execute(self, query: str, params: tuple = None):
        conn = self.get_connection()
        result = conn.execute(query, params)
        conn.commit()
        return result

    def fetch_all(self, query: str, params: tuple = None):
        conn = self.get_connection()
        result = conn.execute(query, params)
        return result.fetchall()

    def fetch_one(self, query: str, params: tuple = None):
        conn = self.get_connection()
        result = conn.execute(query, params)
        return result.fetchone()

    def update(self, query: str, params: tuple = None):
        """Execute an update query and return the updated row"""
        conn = self.get_connection()
        result = conn.execute(query, params)
        conn.commit()
        return result.fetchone()

def get_db():
    return Database()