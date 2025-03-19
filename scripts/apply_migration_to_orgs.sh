#!/bin/bash

# Check if SQL file is provided
if [ $# -eq 0 ]; then
    echo "Error: No SQL file provided"
    echo "Usage: $0 <path_to_sql_file>"
    exit 1
fi

SQL_FILE=$1

# Check if the SQL file exists
if [ ! -f "$SQL_FILE" ]; then
    echo "Error: SQL file '$SQL_FILE' not found"
    exit 1
fi

echo "Applying migration from $SQL_FILE to all org databases..."

# Get all org databases
ORG_DBS=$(turso db list | grep "org-" | awk '{print $1}')

# Check if any org databases were found
if [ -z "$ORG_DBS" ]; then
    echo "No org databases found"
    exit 0
fi

# Apply the migration to each org database
for DB in $ORG_DBS; do
    echo "Applying migration to $DB"
    turso db shell "$DB" < "$SQL_FILE"
    
    # Check the exit status
    if [ $? -eq 0 ]; then
        echo "✅ Successfully applied migration to $DB"
    else
        echo "❌ Failed to apply migration to $DB"
    fi
done

echo "Migration process completed!" 