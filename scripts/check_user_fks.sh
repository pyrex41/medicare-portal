#!/bin/bash

DB="medicare-portal"

# Get all table names except sqlite_sequence (internal)
tables=$(turso db shell $DB "SELECT name FROM sqlite_master WHERE type='table' AND name != 'sqlite_sequence';" | tail -n +2)

echo "Checking for foreign keys referencing 'users' in all tables..."

for table in $tables; do
    # Get foreign keys for this table
    fk_info=$(turso db shell $DB "PRAGMA foreign_key_list('$table');" | grep users)
    if [ ! -z "$fk_info" ]; then
        echo "Table '$table' has foreign key(s) referencing 'users':"
        turso db shell $DB "PRAGMA foreign_key_list('$table');" | grep users
        echo "----------------------------------------"
    fi
done

echo "Done."