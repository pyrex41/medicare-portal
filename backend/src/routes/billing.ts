import { Elysia } from 'elysia';
import { Database } from '../database';
import { logger } from '../logger';
import { config } from '../config';
import { requireAuth } from '../middleware/auth';
import Stripe from 'stripe';

const stripe = new Stripe(config.stripeSecretKey || '', {
  apiVersion: '2025-02-24.acacia'
});

interface PlanInfo {
  name: string;
  contactCount: number;
  totalClients: number;
  priceDescription?: string;
  basePrice?: number;
  contactsIncluded?: number;
  pricePerAdditionalContact?: number;
}

interface PaymentMethod {
  id: string;
  type: string;
  last4: string;
  brand: string;
  expiryMonth: number;
  expiryYear: number;
  email: string;
}

interface Invoice {
  id: string;
  number: string;
  date: string;
  status: 'pending' | 'paid' | 'failed';
  amount: number;
  currency: string;
  plan: string;
  pdfUrl?: string;
}

export const createBillingRoutes = (app: Elysia) => {
  app.use(requireAuth)
    
    // Get current plan information
    .get('/api/billing/plan', async ({ user, set }) => {
      try {
        if (!user?.organization_id) {
          set.status = 400;
          return { 
            success: false,
            error: 'No organization ID found' 
          };
        }
        logger.info(`Fetching plan for organization_id: ${user.organization_id}`);
        
        const mainDb = new Database(); // For main 'organizations' table
        
        // Get organization details (excluding contact_count) from main DB
        const orgDetailsFromMain = await mainDb.fetchOne<{
          id: number; 
          name: string;
          stripe_customer_id: string | null;
          stripe_subscription_id: string | null;
          subscription_tier: string | null;
        }>(
          `SELECT 
              id,
              name,
              stripe_customer_id,
              stripe_subscription_id,
              subscription_tier 
            FROM organizations
            WHERE id = ?`,
          [user.organization_id]
        );
        
        logger.info(`Organization details from DB for org ${user.organization_id}: ${JSON.stringify(orgDetailsFromMain)}`);

        if (!orgDetailsFromMain) {
          set.status = 404;
          logger.error(`Organization not found in main DB for id: ${user.organization_id}`);
          return { success: false, error: 'Organization not found' };
        }

        let contactCount = 0;
        try {
          // Get contact_count from org-specific DB
          // Ensure user.organization_id is passed as a string to getOrgDb
          const orgDb = await Database.getOrgDb(user.organization_id.toString());
          const contactCountResult = await orgDb.fetchOne<{ count: number }>(
            // Count all contacts in the table, as all are considered active
            "SELECT COUNT(id) as count FROM contacts"
          );
          if (contactCountResult) {
            contactCount = contactCountResult.count;
          }
          logger.info(`Fetched contact_count: ${contactCount} for org ${user.organization_id} from orgDB.`);
        } catch (orgDbError) {
          logger.error(`Error fetching contact count from orgDB for org ${user.organization_id}: ${orgDbError}`);
          // Continue with contactCount = 0 and rely on defaults.
        }
        
        // Combine org details with contact count
        const org = {
          ...orgDetailsFromMain,
          contact_count: contactCount // Add the count here
        };
        
        // Default plan info
        let planInfo: PlanInfo = {
          name: 'Monthly Plan',
          contactCount: Number(org.contact_count) || 0,
          totalClients: 500, // Default limit
          priceDescription: 'Plan details not available', // Default description
        };
        
        // If they have a Stripe subscription, get details from Stripe
        if (org.stripe_subscription_id) {
          try {
            let subscriptionIdToUse = org.stripe_subscription_id as string;
            logger.info(`Initial stripe_subscription_id from DB for org ${user.organization_id}: ${org.stripe_subscription_id}`);
            try {
              // Attempt to parse as JSON and get the id field
              const parsedSubscription = JSON.parse(org.stripe_subscription_id as string);
              if (parsedSubscription && typeof parsedSubscription.id === 'string') {
                subscriptionIdToUse = parsedSubscription.id;
              } else {
                // Log a warning if parsing succeeded but 'id' field is missing or not a string
                logger.warn(`Parsed stripe_subscription_id but 'id' field is missing or not a string for org ${user.organization_id}`);
              }
            } catch (parseError) {
              // If parsing fails, assume it's already a plain string ID. Log this for awareness.
              logger.info(`stripe_subscription_id for org ${user.organization_id} is not valid JSON, using as plain string: ${parseError}`);
            }

            logger.info(`Using subscriptionIdToUse: ${subscriptionIdToUse} for org ${user.organization_id}`);

            const subscription = await stripe.subscriptions.retrieve(
              subscriptionIdToUse
            );
            logger.info(`Full retrieved subscription object for org ${user.organization_id}: ${JSON.stringify(subscription, null, 2)}`);

            if (subscription.items.data.length > 0 && subscription.items.data[0].price) {
              // The full price object is part of the full subscription object logged above, so this specific log can be removed if desired.
            }
            
            // Get the price details
            if (subscription.items.data.length > 0) {
              const item = subscription.items.data[0];
              const price = item.price; // This is a Stripe.Price object
              
              // Update plan name from Stripe product
              if (price.product) {
                try {
                  const productObjectOrId = price.product;
                  const productId = typeof productObjectOrId === 'string' ? productObjectOrId : productObjectOrId.id;
                  
                  const stripeProduct = await stripe.products.retrieve(productId);
                  
                  logger.info(`Full retrieved product object for org ${user.organization_id} (product ID ${productId}): ${JSON.stringify(stripeProduct, null, 2)}`);

                  if (stripeProduct && stripeProduct.name) {
                    planInfo.name = stripeProduct.name;
                  }

                  // Initialize price fields before attempting to parse or use defaults
                  planInfo.basePrice = undefined;
                  planInfo.contactsIncluded = undefined;
                  planInfo.pricePerAdditionalContact = undefined;

                  if (price.unit_amount) {
                    planInfo.basePrice = price.unit_amount / 100;
                  } else if (price.billing_scheme === 'tiered' || price.billing_scheme === 'per_unit') {
                    planInfo.basePrice = 0; // Indicates usage-based or tiers, to be refined by description
                    logger.warn(`Price object for org ${user.organization_id} has null unit_amount (billing_scheme: ${price.billing_scheme}). Base price set to 0 initially.`);
                  }

                  // Try to parse detailed pricing from the product description.
                  if (stripeProduct && stripeProduct.description) {
                    planInfo.priceDescription = stripeProduct.description; // Store the raw description first
                    
                    const priceRegex = /\$(\d+(?:\.\d{1,2})?)[^\d]*?(\d+)\s+contacts\D*\$(\d+(?:\.\d{1,2})?)\s*\/\s*contact/i;
                    const matches = stripeProduct.description.match(priceRegex);

                    if (matches && matches.length >= 4) {
                      planInfo.basePrice = parseFloat(matches[1]); // Override if parsed
                      planInfo.contactsIncluded = parseInt(matches[2], 10);
                      planInfo.pricePerAdditionalContact = parseFloat(matches[3]);
                      planInfo.priceDescription = `\$${planInfo.basePrice}/month for first ${planInfo.contactsIncluded} contacts, then \$${planInfo.pricePerAdditionalContact}/contact.`;
                      logger.info(`Parsed detailed pricing for org ${user.organization_id}: Base: ${planInfo.basePrice}, Included: ${planInfo.contactsIncluded}, Additional: ${planInfo.pricePerAdditionalContact}`);
                    } else {
                      logger.warn(`Could not parse detailed pricing structure from product description for org ${user.organization_id}: ${stripeProduct.description}. Using full description.`);
                      // Fallback for basePrice if not parsed from complex structure but a simple price exists in description
                      if (planInfo.basePrice === 0 || planInfo.basePrice === undefined) { // Only if not set by unit_amount or complex parse
                        const singlePriceMatch = stripeProduct.description.match(/\$(\d+(?:\.\d{1,2})?)/);
                        if (singlePriceMatch && singlePriceMatch[1]) {
                          planInfo.basePrice = parseFloat(singlePriceMatch[1]);
                          logger.info(`Fallback: Parsed base price $${planInfo.basePrice} from product description for org ${user.organization_id}`);
                        }
                      }
                    }
                  } else {
                    planInfo.priceDescription = 'Pricing details not available in product description.';
                  }

                } catch (productError) {
                  logger.error(`Error fetching product details from Stripe: ${productError} for product ID: ${price.product}`);
                  // Continue with default plan name or previously set name
                }
              }

              if (price.unit_amount) {
                // planInfo.pricePerMonth = price.unit_amount / 100; // Convert from cents
              } else if (price.billing_scheme === 'tiered' || price.billing_scheme === 'per_unit') {
                // For tiered or metered plans without a simple unit_amount, actual price is usage-dependent.
                // Set to 0 or a specific indicator. For now, 0 and log.
                // planInfo.pricePerMonth = 0;
                logger.warn(`Price object for org ${user.organization_id} has null unit_amount (billing_scheme: ${price.billing_scheme}). Display price set to 0. Full pricing is usage-dependent or tiered.`);
              }
              // If unit_amount is null and it's not explicitly tiered/per_unit, it will keep the default 60, 
              // which might indicate an issue or a different pricing model not yet handled.
            }
            
            // You can customize the total clients based on the plan
            if (org.subscription_tier === 'premium') {
              planInfo.totalClients = 1000;
            } else if (org.subscription_tier === 'enterprise') {
              planInfo.totalClients = 5000;
            }
          } catch (stripeError) {
            logger.error(`Error fetching subscription from Stripe: ${stripeError}`);
            // Continue with default values
          }
        }
        
        logger.info(`Final planInfo for org ${user.organization_id}: ${JSON.stringify(planInfo)}`);
        return {
          success: true,
          data: planInfo
        };
      } catch (error) {
        logger.error(`Error in plan info route: ${error}`);
        set.status = 500;
        return { 
          success: false,
          error: 'Failed to fetch plan information'
        };
      }
    })
    
    // Get payment method information
    .get('/api/billing/payment-method', async ({ user, set }) => {
      try {
        if (!user?.organization_id) {
          set.status = 400;
          return { 
            success: false,
            error: 'No organization ID found' 
          };
        }
        
        const db = new Database();
        const client = db.getClient();
        
        const orgResult = await client.execute({
          sql: 'SELECT stripe_customer_id FROM organizations WHERE id = ?',
          args: [user.organization_id]
        });
        
        if (orgResult.rows.length === 0 || !orgResult.rows[0].stripe_customer_id) {
          return {
            success: true,
            data: null // No Stripe customer ID, so no payment method
          };
        }
        
        const stripeCustomerId = orgResult.rows[0].stripe_customer_id as string;

        // Retrieve the customer, expanding the default payment method
        const customer = await stripe.customers.retrieve(stripeCustomerId, {
          expand: ['invoice_settings.default_payment_method']
        });

        if (!customer || customer.deleted) {
          logger.warn(`Stripe customer not found or deleted for customer ID: ${stripeCustomerId}`);
          return {
            success: true,
            data: null
          };
        }
        
        const defaultPaymentMethod = customer.invoice_settings?.default_payment_method;

        if (defaultPaymentMethod && typeof defaultPaymentMethod !== 'string' && defaultPaymentMethod.type === 'card' && defaultPaymentMethod.card) {
          const paymentMethodDetails: PaymentMethod = {
            id: defaultPaymentMethod.id,
            type: defaultPaymentMethod.type,
            last4: defaultPaymentMethod.card.last4 || '',
            brand: defaultPaymentMethod.card.brand || '',
            expiryMonth: defaultPaymentMethod.card.exp_month || 0,
            expiryYear: defaultPaymentMethod.card.exp_year || 0,
            email: (customer as Stripe.Customer).email || ''
          };
          return {
            success: true,
            data: paymentMethodDetails
          };
        } else {
          // No default payment method, or it's not a card, or card details are missing
          logger.info(`No default card payment method found or details incomplete for customer ${stripeCustomerId}`);
          return {
            success: true,
            data: null // No suitable payment method found
          };
        }
      } catch (error) {
        logger.error(`Error in payment method route: ${error}`);
        set.status = 500;
        return { 
          success: false,
          error: 'Failed to fetch payment method information' 
        };
      }
    })
    
    // Get invoices list
    .get('/api/billing/invoices', async ({ user, set, query }) => {
      try {
        if (!user?.organization_id) {
          set.status = 400;
          return { 
            success: false,
            error: 'No organization ID found' 
          };
        }
        
        const db = new Database();
        const client = db.getClient();
        
        // Get organization's Stripe customer ID
        const orgResult = await client.execute({
          sql: 'SELECT stripe_customer_id FROM organizations WHERE id = ?',
          args: [user.organization_id]
        });
        
        if (orgResult.rows.length === 0 || !orgResult.rows[0].stripe_customer_id) {
          return {
            success: true,
            data: [] // No invoices
          };
        }
        
        const stripeCustomerId = orgResult.rows[0].stripe_customer_id as string;
        
        // Get invoices from Stripe
        const limit = query?.limit ? parseInt(query.limit as string) : 10;
        const stripeInvoices = await stripe.invoices.list({
          customer: stripeCustomerId,
          limit: limit
        });
        
        const invoices: Invoice[] = stripeInvoices.data.map(inv => ({
          id: inv.id,
          number: inv.number || `#${inv.id.slice(-4).toUpperCase()}`,
          date: new Date(inv.created * 1000).toISOString(),
          status: inv.status === 'paid' ? 'paid' : 
                  inv.status === 'open' ? 'pending' : 'failed',
          amount: (inv.amount_paid || inv.amount_due) / 100, // Convert from cents
          currency: inv.currency.toUpperCase(),
          plan: 'Monthly', // You can customize this based on subscription
          pdfUrl: inv.invoice_pdf || undefined
        }));
        
        return {
          success: true,
          data: invoices
        };
      } catch (error) {
        logger.error(`Error in invoices route: ${error}`);
        set.status = 500;
        return { 
          success: false,
          error: 'Failed to fetch invoices'
        };
      }
    })
    
    // Update payment method
    .post('/api/billing/update-payment-method', async ({ user, body, set }) => {
      try {
        if (!user?.organization_id) {
          set.status = 400;
          return { 
            success: false,
            error: 'No organization ID found' 
          };
        }
        
        const { paymentMethodId } = body as { paymentMethodId: string };
        
        if (!paymentMethodId) {
          set.status = 400;
          return {
            success: false,
            error: 'Payment method ID is required'
          };
        }
        
        const db = new Database();
        const client = db.getClient();
        
        // Get organization's Stripe customer ID
        const orgResult = await client.execute({
          sql: 'SELECT stripe_customer_id, stripe_subscription_id FROM organizations WHERE id = ?',
          args: [user.organization_id]
        });
        
        if (orgResult.rows.length === 0 || !orgResult.rows[0].stripe_customer_id) {
          set.status = 400;
          return {
            success: false,
            error: 'No Stripe customer found for organization'
          };
        }
        
        const stripeCustomerId = orgResult.rows[0].stripe_customer_id as string;
        const stripeSubscriptionId = orgResult.rows[0].stripe_subscription_id as string;
        
        // Attach the payment method to the customer
        await stripe.paymentMethods.attach(paymentMethodId, {
          customer: stripeCustomerId
        });
        
        // Set as default payment method for the customer
        await stripe.customers.update(stripeCustomerId, {
          invoice_settings: {
            default_payment_method: paymentMethodId
          }
        });
        
        // If there's a subscription, update its default payment method too
        if (stripeSubscriptionId) {
          await stripe.subscriptions.update(stripeSubscriptionId, {
            default_payment_method: paymentMethodId
          });
        }
        
        return {
          success: true,
          message: 'Payment method updated successfully'
        };
      } catch (error) {
        logger.error(`Error updating payment method: ${error}`);
        set.status = 500;
        return { 
          success: false,
          error: 'Failed to update payment method'
        };
      }
    })
    
    // Download invoice
    .get('/api/billing/invoices/:invoiceId/download', async ({ user, params, set }) => {
      try {
        if (!user?.organization_id) {
          set.status = 400;
          return { 
            success: false,
            error: 'No organization ID found' 
          };
        }
        
        const { invoiceId } = params;
        
        // Retrieve the invoice from Stripe
        const invoice = await stripe.invoices.retrieve(invoiceId);
        
        // Verify the invoice belongs to the user's organization
        const db = new Database();
        const client = db.getClient();
        
        const orgResult = await client.execute({
          sql: 'SELECT stripe_customer_id FROM organizations WHERE id = ?',
          args: [user.organization_id]
        });
        
        if (orgResult.rows.length === 0 || 
            orgResult.rows[0].stripe_customer_id !== invoice.customer) {
          set.status = 403;
          return {
            success: false,
            error: 'Unauthorized to access this invoice'
          };
        }
        
        if (!invoice.invoice_pdf) {
          set.status = 404;
          return {
            success: false,
            error: 'Invoice PDF not available'
          };
        }
        
        // Return the PDF URL for the client to download
        return {
          success: true,
          data: {
            url: invoice.invoice_pdf
          }
        };
      } catch (error) {
        logger.error(`Error downloading invoice: ${error}`);
        set.status = 500;
        return { 
          success: false,
          error: 'Failed to download invoice'
        };
      }
    });

  return app;
}; 