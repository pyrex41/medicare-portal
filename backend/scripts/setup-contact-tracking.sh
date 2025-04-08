#!/bin/bash

# Setup script for contact tracking system
# This script installs dependencies and sets up the database tables

# Exit on any error
set -e

echo "Setting up contact tracking system..."

# Install UUID package
echo "Installing dependencies..."
npm install uuid
npm install --save-dev @types/uuid

# Create necessary database tables
echo "Setting up database tables..."
DB_PATH="./database.db"

# Run the SQL migration
echo "Applying database migration..."
MIGRATION_PATH="./migrations/20240406_add_contact_history_tracking.sql"

# Use the migrate script if available
if [ -f "./scripts/migrate.sh" ]; then
  ./scripts/migrate.sh $MIGRATION_PATH
else
  # Fallback if migrate script doesn't exist
  echo "Migration script not found, attempting direct application..."
  
  # For SQLite
  if [ -f "$DB_PATH" ]; then
    sqlite3 $DB_PATH < $MIGRATION_PATH
  else
    echo "Database file not found at $DB_PATH"
    echo "Please run this script from the backend directory or update the DB_PATH variable."
    exit 1
  fi
fi

# Set file as executable
chmod +x ./scripts/setup-contact-tracking.sh

echo "Contact tracking system setup complete!"
echo ""
echo "Usage:"
echo "  - New contacts are automatically tracked when added to the system"
echo "  - Billing is based on unique contacts per billing cycle"
echo "  - Contact statistics are available at /api/contact-tracking/usage-stats"
echo "" 