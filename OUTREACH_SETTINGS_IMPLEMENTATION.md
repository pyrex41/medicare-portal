# Outreach Settings Implementation

## Overview
This document describes the implementation of new outreach settings features for the Medicare Max platform.

## Features Implemented

### 1. Contact Outreach Delay Setting ✅
- **Field**: `contactOutreachDelayYears` (integer)
- **Options**: 1 year (default), 2 years, 3 years
- **Purpose**: Determines how long to wait before reaching out to contacts
- **Location**: Settings page and Onboarding flow

### 2. Outreach Types Selection ✅
- **Field**: `outreachTypes` (object)
- **Sub-fields**: 
  - `birthday` (boolean) - Birthday outreach
  - `enrollmentAnniversary` (boolean) - Enrollment anniversary outreach  
  - `scheduleIncrease` (boolean) - Schedule increase outreach
  - `aep` (boolean) - Annual enrollment period outreach
- **Default**: All enabled (true)
- **Validation**: At least one must be selected
- **Location**: Settings page and Onboarding flow

### 3. Smart Send Toggle ✅
- **Field**: `smartSendEnabled` (boolean) 
- **Purpose**: Enable/disable smart send functionality
- **Location**: Settings page (already existed, confirmed working)

### 4. Individual Contact Opt-out ✅
- **Database Field**: `do_not_communicate` (boolean) in contacts table
- **Purpose**: Allows marking specific contacts to be excluded from all outreach
- **Default**: false
- **Index**: Added for performance (`idx_contacts_do_not_communicate`)

### 5. **Failed Underwriting Outreach Reduction** ✅
- **Field**: `failedUnderwritingOutreach` (object)
- **Sub-fields**:
  - `enabled` (boolean) - Whether to reduce frequency
  - `frequency` (string) - Currently "annual" 
  - `timing` (string) - When to send: "birthday", "enrollmentAnniversary", "aep", "scheduleIncrease"
- **Data Source**: Derived from `contact_events` table where `event_type = 'eligibility_answered'` and metadata contains `"has_medical_conditions": true`
- **Purpose**: Reduce outreach to once per year for contacts who failed underwriting

## Files Modified

### Frontend Changes:
1. **`frontend/src/Settings.elm`** - Added complete outreach settings section
2. **`frontend/src/Onboarding.elm`** - Added outreach settings to onboarding flow

### Backend Changes:
1. **`backend/src/types.ts`** - Added BaseSettings interface with new fields
2. **`backend/src/routes/settings.ts`** - Updated default settings and handling
3. **`backend/src/routes/onboarding.ts`** - Added outreach settings to onboarding data

### Database Changes:
1. **`migrations/contact_005_add_outreach_optout.sql`** - Contact-level opt-out functionality

## Database Schema Changes

### Migration: `contact_005_add_outreach_optout.sql`
```sql
-- Add outreach opt-out column for individual contacts
ALTER TABLE contacts ADD COLUMN do_not_communicate BOOLEAN NOT NULL DEFAULT FALSE;
CREATE INDEX IF NOT EXISTS idx_contacts_do_not_communicate ON contacts(do_not_communicate);

-- Note: Failed underwriting status is derived from contact_events table
-- where event_type = 'eligibility_answered' and metadata JSON contains "has_medical_conditions": true
```

## Default Values

### Backend Defaults (TypeScript):
```typescript
contactOutreachDelayYears: 1,
outreachTypes: {
  birthday: true,
  enrollmentAnniversary: true, 
  scheduleIncrease: true,
  aep: true
},
failedUnderwritingOutreach: {
  enabled: false,
  frequency: 'annual',
  timing: 'birthday'
}
```

### Frontend Defaults (Elm):
```elm
contactOutreachDelayYears = 1
outreachTypes = { birthday = True, enrollmentAnniversary = True, scheduleIncrease = True, aep = True }
failedUnderwritingOutreach = { enabled = False, frequency = "annual", timing = "birthday" }
```

## User Interface

### Settings Page
- New "Contact Outreach Settings" section added after email settings
- All settings save automatically when changed
- Backward compatible with existing organizations (optional fields)

### Onboarding Flow
- New "Contact Outreach Settings" section in licensing step
- Settings included in final onboarding data submission
- Default values ensure smooth onboarding experience

### Components Added:
- Contact outreach delay dropdown (1-3 years)
- Outreach types checkboxes with validation note
- Failed underwriting toggle with timing selector

## Data Flow

1. **Onboarding**: Outreach settings collected → stored in `org_settings` JSON column
2. **Settings**: Outreach settings displayed → updated via PUT `/api/settings/org`
3. **Contacts**: Individual opt-out flags stored in contacts table columns

## API Integration

### Onboarding Endpoint: `/api/onboarding/licensing`
**Request Body** (updated):
```json
{
  "email": "user@example.com",
  "selectedCarriers": ["Aetna", "Humana"],
  "useSmartSend": true,
  "contactOutreachDelayYears": 1,
  "outreachTypes": {
    "birthday": true,
    "enrollmentAnniversary": true,
    "scheduleIncrease": true,
    "aep": true
  },
  "failedUnderwritingOutreach": {
    "enabled": false,
    "frequency": "annual",
    "timing": "birthday"
  }
}
```

### Settings Endpoint: `/api/settings/org`
Settings are stored in the `org_settings` JSON column in the organizations table and follow the same structure.

## Validation & Error Handling

- **Frontend**: At least one outreach type must be selected (validation message shown)
- **Backend**: Default values provided for all new fields to maintain backward compatibility
- **Database**: All new columns have default values and NOT NULL constraints

## Testing Status

- ✅ **Elm Compilation**: Successfully compiles without errors
- ✅ **Frontend UI**: Outreach settings visible in both Settings and Onboarding
- ✅ **Backend API**: Endpoints updated to handle new fields
- ✅ **Data Persistence**: Settings stored in organization settings
- ✅ **Backward Compatibility**: Existing organizations get sensible defaults

## Usage Guidelines

### For Implementation Teams:
- Run the database migration before deploying
- Outreach logic should check both organization settings and individual contact flags
- Failed underwriting status should be determined by querying `contact_events` table for eligibility answers with `"has_medical_conditions": true`
- Failed underwriting contacts should respect the reduced frequency setting

### For Users:
- Configure organization-wide outreach settings in Settings or during onboarding
- Mark individual contacts as "do not communicate" on contact profiles (UI pending)
- Enable failed underwriting reduction to automatically reduce frequency for specific contacts

## Future Enhancements

Potential areas for expansion:
- Individual contact profile UI for opt-out toggles
- Additional timing options for failed underwriting outreach
- More granular outreach type controls
- Contact-specific outreach delay overrides
- Outreach frequency analytics and reporting
- Bulk contact opt-out management 