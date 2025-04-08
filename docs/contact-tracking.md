# Contact Tracking for Usage-Based Billing

This document explains how the contact tracking system works for usage-based billing.

## Overview

The system tracks unique contacts across billing cycles to ensure accurate billing based on the number of unique contacts processed, rather than the current count in the database at any given time.

## Key Concepts

### Unique Contact Tracking

- Each unique email address is counted only once per billing cycle
- Contacts are tracked even if they are later deleted from the main contacts table
- Re-uploading the same contact multiple times in the same billing cycle counts only once

### Billing Cycles

- Billing cycles are monthly periods starting on a configurable day (defaults to 1st of month)
- Usage is reset at the start of each new billing cycle
- The system automatically creates new billing cycles as needed

### Usage-Based Billing

- Base subscription includes 500 contacts ($60/month)
- Additional contacts are billed at $40 per 500 contacts
- Usage is reported to Stripe automatically when contacts are tracked

## Database Schema

The system uses the following tables:

### contact_history

Tracks every unique contact processed by the system:

```sql
CREATE TABLE contact_history (
  id TEXT PRIMARY KEY,
  organization_id TEXT NOT NULL,
  user_id TEXT NOT NULL,
  email TEXT NOT NULL,
  first_name TEXT,
  last_name TEXT,
  first_uploaded TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  last_uploaded TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  status TEXT DEFAULT 'active',
  billing_cycle_key TEXT,
  UNIQUE(organization_id, email)
);
```

### billing_cycle_history

Tracks billing cycles and their contact counts:

```sql
CREATE TABLE billing_cycle_history (
  id TEXT PRIMARY KEY,
  organization_id TEXT NOT NULL,
  cycle_key TEXT NOT NULL,
  start_date TIMESTAMP NOT NULL,
  end_date TIMESTAMP NOT NULL,
  contact_count INTEGER DEFAULT 0,
  usage_reported BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(organization_id, cycle_key)
);
```

### organizations (new columns)

Additional columns on the organizations table:

```sql
ALTER TABLE organizations ADD COLUMN contact_count_billing_date TEXT;
ALTER TABLE organizations ADD COLUMN current_billing_cycle_key TEXT;
ALTER TABLE organizations ADD COLUMN current_unique_contacts INTEGER DEFAULT 0;
```

## API Endpoints

The system provides the following API endpoints:

### Track Individual Contact

```
POST /api/contact-tracking/track
```

**Request Body:**
```json
{
  "email": "contact@example.com",
  "firstName": "John",
  "lastName": "Doe"
}
```

**Response:**
```json
{
  "success": true,
  "isNew": true,
  "contactId": "contact-uuid",
  "stats": {
    "currentCycle": "2024-04",
    "uniqueContacts": 42,
    "billingCycleStart": "2024-04-01T00:00:00.000Z",
    "billingCycleEnd": "2024-05-01T00:00:00.000Z",
    "contactsRemaining": 458,
    "isOverLimit": false
  }
}
```

### Track Contact Batch

```
POST /api/contact-tracking/batch
```

**Request Body:**
```json
{
  "contacts": [
    {
      "email": "contact1@example.com",
      "firstName": "John",
      "lastName": "Doe"
    },
    {
      "email": "contact2@example.com",
      "firstName": "Jane",
      "lastName": "Smith"
    }
  ]
}
```

**Response:**
```json
{
  "success": true,
  "newCount": 2,
  "totalProcessed": 2,
  "stats": {
    "currentCycle": "2024-04",
    "uniqueContacts": 44,
    "billingCycleStart": "2024-04-01T00:00:00.000Z",
    "billingCycleEnd": "2024-05-01T00:00:00.000Z",
    "contactsRemaining": 456,
    "isOverLimit": false
  }
}
```

### Get Usage Stats

```
GET /api/contact-tracking/usage-stats
```

**Response:**
```json
{
  "success": true,
  "stats": {
    "currentCycle": "2024-04",
    "uniqueContacts": 44,
    "billingCycleStart": "2024-04-01T00:00:00.000Z",
    "billingCycleEnd": "2024-05-01T00:00:00.000Z",
    "contactsRemaining": 456,
    "isOverLimit": false
  }
}
```

### Reset Contact Count (Admin Only)

```
POST /api/contact-tracking/reset
```

**Request Body:**
```json
{
  "email": "contact@example.com",
  "reason": "Duplicate upload"
}
```

**Response:**
```json
{
  "success": true,
  "stats": {
    "currentCycle": "2024-04",
    "uniqueContacts": 43,
    "billingCycleStart": "2024-04-01T00:00:00.000Z",
    "billingCycleEnd": "2024-05-01T00:00:00.000Z",
    "contactsRemaining": 457,
    "isOverLimit": false
  }
}
```

## Implementation Details

### Contact Tracking Flow

1. When a contact is added to the system:
   - Check if it already exists in the contact_history table
   - If it's new or from a different billing cycle, count it as a new contact
   - Update the organization's unique contact count

2. When reporting to Stripe:
   - Calculate the number of 500-contact blocks beyond the initial 500
   - Report the updated usage to Stripe via the Stripe API
   - Update the billing cycle record to track that usage has been reported

3. When a billing cycle ends:
   - A new billing cycle is automatically created on the next contact upload
   - The contact count is reset for the new cycle
   - Each unique contact is counted again in the new cycle

### Edge Cases

**Deleted Contacts:**
- Contacts deleted from the main contacts table are still counted for billing
- The contact_history table maintains the record for billing purposes

**Data Errors:**
- If contacts need to be removed from billing due to errors, use the reset endpoint
- Only admins can reset contact counts

**Bulk Changes:**
- The system processes contacts in batches to handle large uploads
- Each unique email is still tracked properly

## Setup and Maintenance

To set up the contact tracking system:

1. Run the migration:
   ```
   cd backend
   ./scripts/setup-contact-tracking.sh
   ```

2. Make sure UUID package is installed:
   ```
   npm install uuid
   npm install --save-dev @types/uuid
   ```

3. Restart the backend server

## Integration with Stripe

The system automatically reports usage to Stripe:

1. Each organization has a Stripe subscription with:
   - Base tier (fixed price for first 500 contacts)
   - Usage-based price for additional contacts

2. Usage reporting:
   - When contact count changes, the system calculates blocks of 500 contacts
   - Reports the updated quantity to Stripe using the usage_record API
   - Stripe will bill accordingly at the end of the billing cycle 