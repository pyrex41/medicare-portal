import os
from dotenv import load_dotenv
import libsql_experimental as libsql
from pathlib import Path
from contextlib import contextmanager

# Load environment variables from .env file
ENV_FILE = Path(__file__).resolve().parent.parent / '.env'
load_dotenv(ENV_FILE, override=True)

class Database:
    def __init__(self):
        self.db_url = os.getenv("TURSO_DATABASE_URL")
        self.auth_token = os.getenv("TURSO_AUTH_TOKEN")
        self.db_path = os.getenv("TURSO_DATABASE_PATH")
        if not self.db_url or not self.auth_token:
            raise ValueError("Missing database credentials in .env file")
        
        # Create a single connection when initializing
        self.connection = self._create_connection()
        
        # Ensure tables exist
        self.init_tables()

    def _create_connection(self):
        """Create and return a database connection"""
        if not self.db_path:
            return libsql.connect(
                self.db_url,
                auth_token=self.auth_token
            )
        else:
            print(f"Connecting to {self.db_path} with sync URL {self.db_url}")
            return libsql.connect(
                self.db_path,
                sync_url=self.db_url,
                auth_token=self.auth_token,
                sync_interval=300,  # Sync every 5 minutes instead of every minute
            )

    def init_tables(self):
        """Initialize database tables if they don't exist"""
        self.connection.executescript("""
            CREATE TABLE IF NOT EXISTS agents (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                first_name TEXT NOT NULL,
                last_name TEXT NOT NULL,
                email TEXT NOT NULL UNIQUE,
                phone TEXT NOT NULL,
                created_at DATETIME DEFAULT CURRENT_TIMESTAMP
            );

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
            );
        """)

    def execute(self, query: str, params: tuple = None):
        """Execute a query within a transaction"""
        result = self.connection.execute(query, params)
        self.connection.commit()
        return result

    def executescript(self, script: str):
        result = self.connection.executescript(script)
        return result

    def executemany(self, query: str, params_list: list):
        """Execute multiple inserts in a single transaction"""
        if not params_list:
            return None
        
        # Add VALUES to the query if not present
        if "VALUES" not in query.upper():
            values = ','.join(['(' + ','.join(['?'] * len(params_list[0])) + ')'] * len(params_list))
            query = f"{query} VALUES {values}"
        
        # Flatten the params list and convert to tuple
        flat_params = tuple(item for sublist in params_list for item in sublist)
        
        result = self.connection.execute(query, flat_params)
        self.connection.commit()
        return result

    def fetch_all(self, query: str, params: tuple = None):
        """Execute a query and fetch all results"""
        return self.connection.execute(query, params).fetchall()

    def fetch_one(self, query: str, params: tuple = None):
        """Execute a query and fetch one result"""
        return self.connection.execute(query, params).fetchone()

    def update(self, query: str, params: tuple = None):
        """Execute an update query and return the updated row"""
        result = self.connection.execute(query, params)
        self.connection.commit()
        return result.fetchone()

db = Database()

def get_db():
    return db