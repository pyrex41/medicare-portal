/**
 * Stripe integration for Medicare Portal
 * 
 * This module provides the JavaScript functions needed to integrate Stripe Elements
 * with our Elm application for subscription payments.
 */

// Initialize Stripe Elements
let stripe;
let elements;
let cardElement;

/**
 * Initialize Stripe with the publishable key
 * @param {string} publishableKey - The Stripe publishable key
 */
export function initializeStripe(publishableKey) {
  if (!publishableKey) {
    console.error('Stripe publishable key is required');
    return;
  }

  stripe = Stripe(publishableKey);
  
  // Create the Elements instance
  elements = stripe.elements();
  
  // Create the Card Element and mount it to the DOM
  cardElement = elements.create('card', {
    style: {
      base: {
        fontSize: '16px',
        color: '#32325d',
        fontFamily: '-apple-system, BlinkMacSystemFont, Segoe UI, Roboto, sans-serif',
        '::placeholder': {
          color: '#aab7c4',
        },
      },
      invalid: {
        color: '#fa755a',
        iconColor: '#fa755a',
      },
    },
  });
  
  // Mount the Card Element to the DOM
  const cardElementMount = document.getElementById('card-element');
  if (cardElementMount) {
    cardElement.mount('#card-element');
  } else {
    console.error('Card element mount point not found');
  }
  
  // Add event listener for change events on the Card Element
  cardElement.on('change', (event) => {
    const displayError = document.getElementById('card-errors');
    if (displayError) {
      if (event.error) {
        displayError.textContent = event.error.message;
      } else {
        displayError.textContent = '';
      }
    }
  });
  
  return true;
}

/**
 * Process payment with the provided client secret
 * @param {string} clientSecret - The client secret from the server
 * @returns {Promise<Object>} - The result of the payment confirmation
 */
export async function processPayment(clientSecret) {
  if (!stripe || !elements || !cardElement) {
    console.error('Stripe not initialized');
    return { success: false, error: 'Stripe not initialized' };
  }
  
  try {
    const result = await stripe.confirmCardPayment(clientSecret, {
      payment_method: {
        card: cardElement,
        billing_details: {
          // You can collect these from the user if needed
          // name: 'Jenny Rosen',
        },
      },
    });
    
    if (result.error) {
      console.error('Payment error:', result.error.message);
      return { 
        success: false, 
        error: result.error.message 
      };
    } else if (result.paymentIntent.status === 'succeeded') {
      console.log('Payment succeeded');
      return { 
        success: true,
        paymentIntentId: result.paymentIntent.id
      };
    } else {
      console.log('Payment status:', result.paymentIntent.status);
      return { 
        success: false, 
        error: `Payment status: ${result.paymentIntent.status}` 
      };
    }
  } catch (error) {
    console.error('Payment exception:', error);
    return { 
      success: false, 
      error: error.message 
    };
  }
}

/**
 * Clean up Stripe Elements
 */
export function cleanupStripe() {
  if (cardElement) {
    cardElement.unmount();
    cardElement = null;
  }
  elements = null;
  stripe = null;
}

/**
 * Load the Stripe.js script and pricing table component
 */
export function loadStripeScript() {
  // Check if Stripe.js is already loaded
  if (document.querySelector('script[src*="js.stripe.com/v3/"]')) {
    return;
  }
  
  // Load the main Stripe.js script if not already loaded
  const stripeScript = document.createElement('script');
  stripeScript.src = 'https://js.stripe.com/v3/';
  stripeScript.async = true;
  document.head.appendChild(stripeScript);
  
  // Load the pricing table component
  const pricingTableScript = document.createElement('script');
  pricingTableScript.src = 'https://js.stripe.com/v3/pricing-table.js';
  pricingTableScript.async = true;
  document.head.appendChild(pricingTableScript);
  
  // Set up event listeners for the pricing table
  setupPricingTableListeners();
}

/**
 * Set up event listeners for the Stripe pricing table
 */
function setupPricingTableListeners() {
  // Listen for the pricing table to be initialized
  window.addEventListener('stripe-pricing-table-ready', () => {
    console.log('Stripe pricing table is ready');
    
    // Listen for price selection events
    document.addEventListener('stripe-pricing-table-price-selected', (event) => {
      // Create a custom event that we can listen for in main.ts
      const customEvent = new CustomEvent('priceSelected', {
        detail: {
          priceId: event.detail.priceId
        }
      });
      
      // Dispatch the event
      document.dispatchEvent(customEvent);
      console.log('Price selected:', event.detail.priceId);
    });
  });
}

// Expose these functions to Elm via app.ports
window.stripeIntegration = {
  initializeStripe,
  processPayment,
  cleanupStripe
};