#!/bin/bash
# Script to apply both migrations to handle all organization database states

echo "Starting contact schema migration fix..."

# First run migration for databases WITH contacts table
echo "Step 1: Applying migration for databases WITH an existing contacts table..."
./scripts/apply_migration_to_orgs.sh backend/migrations/20250425_fix_schema_with_contacts.sql

# Then run migration for databases WITHOUT contacts table
echo "Step 2: Applying migration for databases WITHOUT an existing contacts table..."
./scripts/apply_migration_to_orgs.sh backend/migrations/20250425_create_schema_no_contacts.sql

echo "Contact schema migration completed!"
echo "Some errors are expected and can be ignored if they relate to missing tables." 