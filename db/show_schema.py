from database import get_db
from pathlib import Path
import sys
from typing import List

def show_schema():
    db = get_db()
    conn = db.get_connection()
    output: List[str] = []
    
    # Get all tables first
    output.append("\n=== Tables ===")
    tables = conn.execute("SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;").fetchall()
    output.extend(f"  {table[0]}" for table in tables)
    
    # Get all schema details
    output.append("\n=== Detailed Schema ===")
    for (table_name,) in tables:
        # Table creation SQL
        sql = conn.execute(
            "SELECT sql FROM sqlite_master WHERE type='table' AND name=?;",
            (table_name,)
        ).fetchone()[0]
        
        output.append(f"\n--- {table_name} ---")
        output.append(sql)
        
        # Column info
        output.append("\nColumns:")
        columns = conn.execute(f"PRAGMA table_info({table_name})").fetchall()
        for col in columns:
            cid, name, type_, notnull, dflt_value, pk = col
            nullable = "NOT NULL" if notnull else "NULL"
            default = f"DEFAULT {dflt_value}" if dflt_value is not None else ""
            primary = "PRIMARY KEY" if pk else ""
            output.append(f"  {name}: {type_} {nullable} {default} {primary}")
        
        # Foreign keys
        foreign_keys = conn.execute(f"PRAGMA foreign_key_list({table_name})").fetchall()
        if foreign_keys:
            output.append("\nForeign Keys:")
            output.extend(f"  {fk[3]} -> {fk[2]}({fk[4]})" for fk in foreign_keys)
    
    # Get all indexes
    output.append("\n=== Indexes ===")
    for (table_name,) in tables:
        indexes = conn.execute("""
            SELECT name, sql 
            FROM sqlite_master 
            WHERE type='index' 
            AND tbl_name=? 
            AND name NOT LIKE 'sqlite_autoindex%'
            ORDER BY name;
        """, (table_name,)).fetchall()
        
        if indexes:
            output.append(f"\n--- {table_name} Indexes ---")
            for name, sql in indexes:
                output.append(f"  {name}:")
                output.append(f"    {sql}")
    
    # Get all triggers
    output.append("\n=== Triggers ===")
    for (table_name,) in tables:
        triggers = conn.execute("""
            SELECT name, sql 
            FROM sqlite_master 
            WHERE type='trigger' 
            AND tbl_name=?
            ORDER BY name;
        """, (table_name,)).fetchall()
        
        if triggers:
            output.append(f"\n--- {table_name} Triggers ---")
            for name, sql in triggers:
                output.append(f"  {name}:")
                output.append(f"    {sql}")
    
    # Print all output at once
    print('\n'.join(output))

if __name__ == "__main__":
    show_schema() 