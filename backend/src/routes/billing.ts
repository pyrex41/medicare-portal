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
  pricePerMonth: number;
  totalClients: number;
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
        
        const db = new Database();
        const client = db.getClient();
        
        // Get organization details including Stripe IDs
        const orgResult = await client.execute({
          sql: `SELECT 
                  o.id,
                  o.name,
                  o.stripe_customer_id,
                  o.stripe_subscription_id,
                  o.subscription_status,
                  o.subscription_tier,
                  COUNT(DISTINCT c.id) as contact_count
                FROM organizations o
                LEFT JOIN contacts c ON c.organization_id = o.id AND c.is_active = 1
                WHERE o.id = ?
                GROUP BY o.id`,
          args: [user.organization_id]
        });
        
        if (orgResult.rows.length === 0) {
          set.status = 404;
          return { success: false, error: 'Organization not found' };
        }
        
        const org = orgResult.rows[0];
        
        // Default plan info
        let planInfo: PlanInfo = {
          name: 'Monthly Plan',
          contactCount: Number(org.contact_count) || 0,
          pricePerMonth: 60, // Base price
          totalClients: 500 // Default limit
        };
        
        // If they have a Stripe subscription, get details from Stripe
        if (org.stripe_subscription_id) {
          try {
            const subscription = await stripe.subscriptions.retrieve(
              org.stripe_subscription_id as string
            );
            
            // Get the price details
            if (subscription.items.data.length > 0) {
              const item = subscription.items.data[0];
              const price = item.price;
              
              if (price.unit_amount) {
                planInfo.pricePerMonth = price.unit_amount / 100; // Convert from cents
              }
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
        
        // Get organization's Stripe customer ID
        const orgResult = await client.execute({
          sql: 'SELECT stripe_customer_id FROM organizations WHERE id = ?',
          args: [user.organization_id]
        });
        
        if (orgResult.rows.length === 0 || !orgResult.rows[0].stripe_customer_id) {
          return {
            success: true,
            data: null // No payment method on file
          };
        }
        
        const stripeCustomerId = orgResult.rows[0].stripe_customer_id as string;
        
        // Get payment methods from Stripe
        const paymentMethods = await stripe.paymentMethods.list({
          customer: stripeCustomerId,
          type: 'card'
        });
        
        if (paymentMethods.data.length === 0) {
          return {
            success: true,
            data: null
          };
        }
        
        // Get the default payment method (usually the first one)
        const pm = paymentMethods.data[0];
        const card = pm.card;
        
        if (!card) {
          return {
            success: true,
            data: null
          };
        }
        
        const paymentMethod: PaymentMethod = {
          id: pm.id,
          type: 'card',
          last4: card.last4,
          brand: card.brand,
          expiryMonth: card.exp_month,
          expiryYear: card.exp_year,
          email: pm.billing_details?.email || user.email || ''
        };
        
        return {
          success: true,
          data: paymentMethod
        };
      } catch (error) {
        logger.error(`Error in payment method route: ${error}`);
        set.status = 500;
        return { 
          success: false,
          error: 'Failed to fetch payment method'
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