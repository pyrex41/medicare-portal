<think>Let me analyze the code to identify all the endpoints involved in the onboarding process and summarize what information is sent to each.

Based on the provided code, I can see the following endpoints used in the onboarding flow:

1. `/api/onboarding/initialize`
2. `/api/onboarding/resume-onboarding`
3. `/api/onboarding/{orgSlug}/user`
4. `/api/onboarding/{orgSlug}/company`
5. `/api/onboarding/{orgSlug}/licensing`
6. `/api/onboarding/{orgSlug}/team-members`
7. `/api/onboarding/{orgSlug}/complete`
8. `/api/organizations/complete-onboarding` (this appears to be possibly a different endpoint for the same purpose as the one above)
9. `/api/organizations/check-email/{email}` (for checking email availability)
10. `/api/me` (for fetching user details)
11. `/api/organizations/signup` (appears to be used for user details in the old workflow)

Let me go through each one to summarize the request methods, data sent, and purpose.</think># Onboarding Endpoints Summary

Based on the code you've shared, here's a detailed summary of all the endpoints involved in the progressive onboarding process:

## 1. Initialize Onboarding
- **Endpoint**: `/api/onboarding/initialize` (POST)
- **Purpose**: Start a new onboarding process
- **Data Sent**:
  ```json
  {
    "planType": "string", // basic, pro, or enterprise
    "email": "string"
  }
  ```
- **Response**:
  ```json
  {
    "organizationId": number,
    "slug": "string",
    "sessionToken": "string",
    "onboardingStep": number
  }
  ```

## 2. Resume Onboarding
- **Endpoint**: `/api/onboarding/resume-onboarding` (POST)
- **Purpose**: Resume an in-progress onboarding
- **Data Sent**:
  ```json
  {
    "email": "string"
  }
  ```
- **Response**: Same as initialize endpoint

## 3. Update User Details
- **Endpoint**: `/api/onboarding/{orgSlug}/user` (PUT)
- **Headers**: `Authorization: Bearer {sessionToken}`
- **Purpose**: Save user's personal information
- **Data Sent**:
  ```json
  {
    "firstName": "string",
    "lastName": "string",
    "email": "string",
    "phone": "string"
  }
  ```
- **Response**:
  ```json
  {
    "onboardingStep": number
  }
  ```

## 4. Update Company Details
- **Endpoint**: `/api/onboarding/{orgSlug}/company` (PUT)
- **Headers**: `Authorization: Bearer {sessionToken}`
- **Purpose**: Save company information
- **Data Sent**:
  ```json
  {
    "agencyName": "string",
    "website": "string",
    "phone": "string",
    "primaryColor": "string",
    "secondaryColor": "string",
    "logo": "string" // optional, can be null
  }
  ```
- **Response**:
  ```json
  {
    "onboardingStep": number
  }
  ```

## 5. Update Licensing Details
- **Endpoint**: `/api/onboarding/{orgSlug}/licensing` (PUT)
- **Headers**: `Authorization: Bearer {sessionToken}`
- **Purpose**: Save licensing and carrier information
- **Data Sent**:
  ```json
  {
    "stateLicenses": ["string"],
    "carrierContracts": ["string"],
    "useSmartSendForGI": boolean
  }
  ```
- **Response**:
  ```json
  {
    "onboardingStep": number,
    "nextStep": number,
    "isBasicPlan": boolean
  }
  ```

## 6. Add Team Members
- **Endpoint**: `/api/onboarding/{orgSlug}/team-members` (PUT)
- **Headers**: `Authorization: Bearer {sessionToken}`
- **Purpose**: Add team members/agents to the organization
- **Data Sent**:
  ```json
  {
    "agents": [
      {
        "firstName": "string",
        "lastName": "string",
        "email": "string",
        "phone": "string",
        "isAdmin": boolean,
        "isAgent": boolean
      }
    ]
  }
  ```
- **Response**:
  ```json
  {
    "onboardingStep": number
  }
  ```

## 7. Complete Subscription
- **Endpoint**: `/api/onboarding/{orgSlug}/complete` (POST)
- **Headers**: `Authorization: Bearer {sessionToken}`
- **Purpose**: Complete the onboarding process and set up subscription
- **Data Sent**:
  ```json
  {
    "planType": "string",
    "extraAgents": number,
    "extraContacts": number
  }
  ```
- **Response**:
  ```json
  {
    "clientSecret": "string",
    "publishableKey": "string"
  }
  ```

## 8. Complete Onboarding (Alternative Endpoint)
- **Endpoint**: `/api/organizations/complete-onboarding` (POST)
- **Purpose**: Submit all onboarding data at once (seems to be a legacy endpoint)
- **Data Sent**: A complex object containing all the onboarding data:
  ```json
  {
    "plan": {
      "type": "string",
      "extraAgents": number,
      "extraContacts": number,
      "price": number,
      "billingCycle": "string"
    },
    "user": {
      "firstName": "string",
      "lastName": "string",
      "email": "string",
      "phone": "string"
    },
    "company": {
      "agencyName": "string",
      "website": "string",
      "phone": "string",
      "primaryColor": "string",
      "secondaryColor": "string",
      "logo": "string" // or null
    },
    "licensing": {
      "stateLicenses": ["string"],
      "carrierContracts": ["string"],
      "stateCarrierSettings": [
        {
          "state": "string",
          "carrier": "string",
          "active": boolean,
          "targetGI": boolean
        }
      ]
    },
    "agents": [...] // list of agent objects
  }
  ```

## 9. Check Email Availability
- **Endpoint**: `/api/organizations/check-email/{email}` (GET)
- **Purpose**: Check if an email is available for registration
- **Response**:
  ```json
  {
    "available": boolean,
    "message": "string"
  }
  ```

The main flow follows a step-by-step progression where each endpoint corresponds to a step in the onboarding process. The progressive approach allows users to save their progress and continue later, with state tracked both on the backend and in the browser's local storage.

Let me know if you need more specific details about any particular endpoint or aspect of the implementation!
