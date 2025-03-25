# Bulk Contact Import Functionality

This document describes the bulk contact import feature that allows importing large numbers of contacts (35,000+) into organizational databases efficiently.

## Overview

The bulk import process works by:
1. Processing and validating input contacts
2. Getting the schema from the existing organization database 
3. Creating a new database with a timestamp-based name
4. Generating optimized SQL with multi-row INSERT statements
5. Uploading the SQL to the new database
6. Updating the organization to point to the new database
7. Cleaning up the old database

## Usage

Three options are provided for using the bulk import functionality:

### 1. Web Interface

A simple HTML form is provided for testing the bulk import API:

```bash
# Start the backend server
cd backend && bun run dev

# Access the test page in your browser
open http://localhost:8000/test-bulk-import.html

# Log in as an admin user first (required for the import API)
# You can use the development login helper:
open http://localhost:8000/api/dev/session/login
```

### 2. Generate Test Data

Generate test data for performance testing with:

```bash
# Generate 1,000 test contacts
bun run generate-data 1000

# Generate 50,000 test contacts in a specific file
bun run generate-data 50000 path/to/output.csv
```

### 3. Command Line Import

Import contacts directly from the command line with:

```bash
# Import contacts for organization ID 1
bun run test-import 1 ./scripts/sample-contacts.csv

# Import and overwrite existing contacts with the same email
bun run test-import 1 ./scripts/sample-contacts.csv --overwrite
```

### 4. API Endpoint

The API endpoint for bulk imports can be accessed programmatically:

```javascript
// Example fetch request
const formData = new FormData();
formData.append('file', csvFile); // File object
formData.append('overwriteExisting', true); // Optional, defaults to false

const response = await fetch('/api/contacts/bulk-import', {
  method: 'POST',
  body: formData,
  credentials: 'include'
});

const result = await response.json();

// To check import status:
const statusResponse = await fetch(`/api/contacts/import-status/${organizationId}`, {
  method: 'GET',
  credentials: 'include'
});

const statusResult = await statusResponse.json();
```

## CSV Format

The CSV file should have headers that match the contact fields. Both camelCase and snake_case property names are supported:

```csv
first_name,last_name,email,current_carrier,plan_type,effective_date,birth_date,tobacco_user,gender,state,zip_code,phone_number
John,Doe,john.doe@example.com,Aetna,F,2023-01-01,1955-08-21,0,Male,FL,33101,123-456-7890
```

## Monitoring Import Progress

You can monitor the progress of an import using the import status endpoint:

```bash
# Get import status for organization 123
curl http://localhost:8000/api/contacts/import-status/123
```

The status endpoint returns:

```json
{
  "status": "in progress" | "completed" | "error" | "none",
  "message": "Description of the current status",
  "timestamp": "ISO date when the import was started",
  "age": 5 // Age in minutes
}
```

This endpoint is also available through the web interface at `/test-bulk-import.html` for easy testing.

## Performance Considerations

- The implementation is optimized for large datasets with:
  - Transaction batching (500 contacts per INSERT statement)
  - Index manipulation (drop before bulk insert, recreate after)
  - Property name normalization for CSV flexibility
  - In-memory validation and deduplication
  - Asynchronous processing with status monitoring

## Implementation Details

The core implementation is in two main files:

1. `/backend/src/database.ts` - Contains the `bulkImportContacts` method
2. `/backend/src/services/turso.ts` - Contains the `createDatabaseFromDump` method

The process works by:
- Using timestamped database names to avoid conflicts
- Maintaining schema consistency by copying existing table structure
- Processing SQL in phases (schema, data, indexes) for reliability
- Using detailed logging for troubleshooting

## Error Handling

The implementation includes robust error handling:
- Database connection failures
- SQL execution errors
- Validation errors for malformed contacts
- Cleanup operations for partial failures