-- Add stripe_checkout_session column to organizations table
ALTER TABLE organizations ADD COLUMN stripe_checkout_session TEXT; 