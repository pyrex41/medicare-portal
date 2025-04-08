# Stripe Integration Setup Guide

This guide covers how to set up our contact-based pricing model in Stripe.

## Overview

Our contact-based pricing uses a tiered model with the following structure:
- Base tier: $60/month for 500 contacts
- Additional tiers: $40/month for each additional 500 contacts

## Development Mode

During development, the application uses a mock Stripe implementation by default. This allows you to test the subscription flow without needing real Stripe credentials.

To use the mock implementation:
- Ensure `USE_MOCK_STRIPE=true` in your `.env` file (or just omit it)
- Or omit Stripe credentials entirely

To use real Stripe in development:
- Set `USE_REAL_STRIPE=true` in your `.env` file
- Provide valid Stripe API keys

## Production Setup

### 1. Stripe Account Setup

1. Create a Stripe account at [stripe.com](https://stripe.com)
2. Get your API keys from the Stripe Dashboard (Developers → API keys)

### 2. Create Products and Prices

#### Create the Base Product

1. In the Stripe Dashboard, go to Products → Add Product
2. Set details:
   - Name: "Contact-Based Subscription"
   - Description: "Subscription with contact-based pricing"

#### Create the Base Tier Price

1. On the product page, add a price:
   - Pricing model: Standard pricing
   - Price: $60.00
   - Billing period: Monthly
   - Name: "Base Tier (500 Contacts)"
2. Save the price and copy the Price ID (starts with `price_`)

#### Create the Additional Contacts Price

1. On the same product, add another price:
   - Pricing model: Standard pricing  
   - Price: $40.00
   - Billing period: Monthly
   - Name: "Additional 500 Contacts"
2. Save the price and copy the Price ID

### 3. Configure Webhook Endpoint

1. Go to Developers → Webhooks → Add Endpoint
2. Set URL to `https://your-domain.com/api/stripe-webhook`
3. Select these events:
   - `customer.subscription.updated`
   - `customer.subscription.created`
   - `invoice.payment_succeeded`
   - `invoice.payment_failed`
   - `customer.subscription.deleted`
4. Save and copy the Webhook Signing Secret

### 4. Environment Variables

Add these variables to your environment:

```
# Stripe API Keys
STRIPE_SECRET_KEY=sk_live_XXXXXXXXXXXXXXXXXX
STRIPE_PUBLISHABLE_KEY=pk_live_XXXXXXXXXXXXXXXXXX
STRIPE_WEBHOOK_SECRET=whsec_XXXXXXXXXXXXXXXXXX

# Stripe Price IDs
STRIPE_PRICE_CONTACT_BASE_TIER=price_XXXXXXXXXXXXXXXXXX
STRIPE_PRICE_ADDITIONAL_CONTACTS=price_XXXXXXXXXXXXXXXXXX
```

### 5. Testing the Integration

1. Start your application with the proper environment variables
2. Use Stripe's test mode (test API keys) for initial testing
3. Verify webhook delivery in the Stripe Dashboard
4. Test the full subscription flow including upgrades and downgrades

## Monitoring

In the Stripe Dashboard, you can monitor:

1. Customers: View all your customers and their subscription details
2. Subscriptions: Monitor active subscriptions and their status
3. Invoices: View all generated invoices and payment history
4. Events: Track webhook events and their delivery status

## Troubleshooting

- Check the server logs for errors when creating or updating subscriptions
- Verify webhook delivery in the Stripe Dashboard
- Use Stripe CLI to test webhook delivery locally

## Subscription Data Format

When a subscription is created, the following metadata is stored:

- `userId`: User ID in your system
- `contactTier`: Numeric tier value (1 = 500 contacts, 2 = 1000 contacts, etc.)
- `contactLimit`: Total contact limit for this subscription
- `organizationId`: Organization ID if applicable
- `tierId`: Original tier ID if applicable

## Contact-Based Pricing Formula

The pricing is calculated as:
- Base price: $60 for the first 500 contacts
- $40 for each additional 500 contacts

For example:
- 500 contacts = $60/month
- 1,000 contacts = $100/month
- 2,500 contacts = $220/month
- 5,000 contacts = $420/month
- 10,000 contacts = $820/month 