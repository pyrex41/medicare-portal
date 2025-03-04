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

// Expose these functions to Elm via app.ports
window.stripeIntegration = {
  initializeStripe,
  processPayment,
  cleanupStripe
};