# Outreach Settings Implementation

## Overview
This document describes the implementation of new outreach settings features for the Medicare Max platform.

## Features Implemented

### 1. Contact Outreach Delay Setting
- **Field**: `contactOutreachDelayYears` (integer)
- **Options**: 1 year (default), 2 years, 3 years
- **Purpose**: Determines how long to wait before reaching out to contacts
- **Location**: Settings page and Onboarding flow

### 2. Outreach Types Selection
- **Field**: `outreachTypes` (object)
- **Sub-fields**: 
  - `birthday` (boolean) - Birthday outreach
  - `enrollmentAnniversary` (boolean) - Enrollment anniversary outreach  
  - `scheduleIncrease` (boolean) - Schedule increase outreach
  - `aep` (boolean) - Annual enrollment period outreach
- **Default**: All enabled (true)
- **Validation**: At least one must be selected
- **Location**: Settings page and Onboarding flow

### 3. Smart Send Toggle
- **Field**: `smartSendEnabled` (boolean) 
- **Purpose**: Enable/disable smart send functionality
- **Location**: Settings page (already existed, confirmed working)

### 4. Individual Contact Opt-out
- **Database Field**: `do_not_communicate` (boolean) in contacts table
- **Purpose**: Allows marking specific contacts to be excluded from all outreach
- **Default**: false
- **Index**: Added for performance (`idx_contacts_do_not_communicate`)

### 5. Failed Underwriting Outreach Reduction
- **Field**: `failedUnderwritingOutreach` (object)
- **Sub-fields**:
  - `enabled` (boolean) - Whether to reduce frequency
  - `frequency` (string) - Currently "annual" 
  - `timing` (string) - When to send: "birthday", "enrollmentAnniversary", "aep", "scheduleIncrease"
- **Database Field**: `failed_underwriting` (boolean) in contacts table
- **Purpose**: Reduce outreach to once per year for contacts who failed underwriting

## Database Changes

### Migration: `/workspace/migrations/contact_005_add_outreach_optout.sql`
```sql
-- Add outreach opt-out column for individual contacts
ALTER TABLE contacts ADD COLUMN do_not_communicate BOOLEAN NOT NULL DEFAULT FALSE;
CREATE INDEX idx_contacts_do_not_communicate ON contacts(do_not_communicate);

-- Add failed underwriting status column
ALTER TABLE contacts ADD COLUMN failed_underwriting BOOLEAN NOT NULL DEFAULT FALSE;
CREATE INDEX idx_contacts_failed_underwriting ON contacts(failed_underwriting);
```

## Backend Changes

### Files Modified:
1. `/workspace/backend/src/types.ts` - Added BaseSettings interface with new fields
2. `/workspace/backend/src/routes/settings.ts` - Updated default settings
3. `/workspace/backend/src/routes/onboarding.ts` - Added outreach settings to onboarding flow

### Default Values:
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

## Frontend Changes

### Files Modified:
1. `/workspace/frontend/src/Settings.elm` - Added outreach settings section
2. `/workspace/frontend/src/Onboarding/Steps/LicensingSettings.elm` - Added outreach settings to onboarding

### New UI Components:
- Contact outreach delay dropdown (1-3 years)
- Outreach types checkboxes with validation
- Failed underwriting toggle with timing selector
- Form validation to ensure at least one outreach type is selected

## Integration Points

### Settings Page
- New "Contact Outreach Settings" section added after email settings
- All settings save automatically when changed
- Backward compatible with existing organizations (optional fields)

### Onboarding Flow
- New "Outreach Settings" expandable section in LicensingSettings step
- Settings included in final onboarding data submission
- Default values ensure smooth onboarding experience

### Data Flow
1. **Onboarding**: Outreach settings collected → stored in `org_settings` JSON column
2. **Settings**: Outreach settings displayed → updated via PUT `/api/settings/org`
3. **Contacts**: Individual opt-out flags stored in contacts table columns

## Usage Guidelines

### For Implementation Teams:
- Run the database migration before deploying
- Outreach logic should check both organization settings and individual contact flags
- Failed underwriting contacts should respect the reduced frequency setting

### For Users:
- Configure organization-wide outreach settings in Settings or during onboarding
- Mark individual contacts as "do not communicate" on contact profiles
- Enable failed underwriting reduction to automatically reduce frequency for specific contacts

## Future Enhancements

Potential areas for expansion:
- Additional timing options for failed underwriting outreach
- More granular outreach type controls
- Contact-specific outreach delay overrides
- Outreach frequency analytics and reporting