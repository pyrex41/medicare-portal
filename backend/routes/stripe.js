const express = require('express');
const router = express.Router();
const db = require('../db');

require('dotenv').config();
const stripe = process.env.STRIPE_API_KEY ? 
  require('stripe')(process.env.STRIPE_API_KEY) : 
  { checkout: { sessions: { create: async () => ({ id: 'test_session_id' }) } } };

// Create a Stripe checkout session
router.post('/create-checkout-session', async (req, res) => {
  try {
    const { orgSlug, tierId, extraAgents, extraContacts } = req.body;
    
    // Get the organization
    const organization = await db.get(
      'SELECT id, name, stripe_customer_id FROM organizations WHERE slug = ?',
      [orgSlug]
    );
    
    if (!organization) {
      return res.status(404).json({ error: 'Organization not found' });
    }
    
    // Get the subscription tier pricing
    const tier = await db.get(
      'SELECT id, name, price_monthly, agent_limit, contact_limit FROM subscription_tiers WHERE id = ?',
      [tierId]
    );
    
    if (!tier) {
      return res.status(404).json({ error: 'Subscription tier not found' });
    }
    
    // Calculate the total price
    const basePriceInCents = tier.price_monthly;
    const extraAgentPriceInCents = extraAgents * 2000; // $20 per agent
    const extraContactsPackages = Math.ceil(extraContacts / 5000);
    const extraContactsPriceInCents = extraContactsPackages * 5000; // $50 per 5000 contacts
    
    const totalPriceInCents = basePriceInCents + extraAgentPriceInCents + extraContactsPriceInCents;
    
    let sessionConfig = {
      payment_method_types: ['card'],
      line_items: [
        {
          price_data: {
            currency: 'usd',
            product_data: {
              name: tier.name + ' Plan',
              description: `Includes ${tier.agent_limit} agents and ${tier.contact_limit} contacts`
            },
            unit_amount: basePriceInCents
          },
          quantity: 1
        }
      ],
      mode: 'subscription',
      success_url: process.env.CLIENT_URL + '/dashboard?payment_success=true',
      cancel_url: process.env.CLIENT_URL + '/change-plan?payment_canceled=true',
    };
    
    // Add extra agents if any
    if (extraAgents > 0) {
      sessionConfig.line_items.push({
        price_data: {
          currency: 'usd',
          product_data: {
            name: 'Extra Agents',
            description: `${extraAgents} additional agents at $20/agent/month`
          },
          unit_amount: 2000,
        },
        quantity: extraAgents
      });
    }
    
    // Add extra contacts if any
    if (extraContacts > 0) {
      sessionConfig.line_items.push({
        price_data: {
          currency: 'usd',
          product_data: {
            name: 'Extra Contacts',
            description: `${extraContactsPackages} packages of 5,000 contacts at $50/package/month`
          },
          unit_amount: 5000,
        },
        quantity: extraContactsPackages
      });
    }
    
    // If organization already has a Stripe customer ID, use it
    if (organization.stripe_customer_id) {
      sessionConfig.customer = organization.stripe_customer_id;
    } else {
      sessionConfig.customer_email = req.session.user?.email;
    }

    // Create the checkout session
    let session;
    if (process.env.STRIPE_API_KEY) {
      session = await stripe.checkout.sessions.create(sessionConfig);
    } else {
      // For development without Stripe API key
      console.log('Creating test checkout session with config:', sessionConfig);
      session = { id: 'test_session_id_' + Date.now() };
    }
    
    // Store the checkout session ID in the database for later verification
    await db.run(
      `UPDATE organizations 
       SET stripe_checkout_session = ? 
       WHERE id = ?`,
      [session.id, organization.id]
    );
    
    // Return the session ID to the client
    res.json({ sessionId: session.id });
    
  } catch (error) {
    console.error('Error creating checkout session:', error);
    res.status(500).json({ error: 'Failed to create checkout session' });
  }
});

// Redirect to Stripe checkout
router.get('/redirect-to-checkout', (req, res) => {
  const { session_id } = req.query;
  
  if (!session_id) {
    return res.status(400).json({ error: 'Session ID is required' });
  }
  
  // In production, you would use Stripe's client-side SDK to redirect
  // For this demo, we'll simulate by redirecting to a success page
  if (process.env.STRIPE_API_KEY) {
    res.redirect(`https://checkout.stripe.com/pay/${session_id}`);
  } else {
    // Simulate successful payment in development
    res.redirect('/dashboard?payment_success=true&test_mode=true');
  }
});

// Webhook for Stripe events (payment completion, etc.)
router.post('/webhook', express.raw({ type: 'application/json' }), async (req, res) => {
  let event;
  
  try {
    const signature = req.headers['stripe-signature'];
    
    if (process.env.STRIPE_WEBHOOK_SECRET && process.env.STRIPE_API_KEY) {
      event = stripe.webhooks.constructEvent(
        req.body,
        signature,
        process.env.STRIPE_WEBHOOK_SECRET
      );
    } else {
      // For development without Stripe
      event = { type: 'test.event', data: { object: req.body } };
    }
    
    // Handle the event
    switch (event.type) {
      case 'checkout.session.completed':
        await handleCheckoutComplete(event.data.object);
        break;
      default:
        console.log(`Unhandled event type ${event.type}`);
    }
    
    res.status(200).send();
  } catch (error) {
    console.error('Error processing webhook:', error);
    res.status(400).send(`Webhook Error: ${error.message}`);
  }
});

async function handleCheckoutComplete(session) {
  try {
    // Find the organization by checkout session ID
    const organization = await db.get(
      `SELECT id, slug FROM organizations WHERE stripe_checkout_session = ?`,
      [session.id]
    );
    
    if (!organization) {
      console.error('Organization not found for checkout session:', session.id);
      return;
    }
    
    // Update the organization with the subscription info
    await db.run(
      `UPDATE organizations 
       SET stripe_subscription_id = ?, 
           stripe_customer_id = ?,
           stripe_checkout_session = NULL
       WHERE id = ?`,
      [session.subscription, session.customer, organization.id]
    );
    
    console.log(`Subscription updated for organization: ${organization.slug}`);
  } catch (error) {
    console.error('Error handling checkout completion:', error);
  }
}

module.exports = router; 