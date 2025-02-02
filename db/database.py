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

def get_db():
    return Database()