// Load and manage the Stripe.js library
let stripeInstance: any = null;

// Load the Stripe.js script dynamically
export function loadStripeScript() {
  if (document.getElementById('stripe-js')) {
    console.log('Stripe script already loaded');
    return;
  }

  console.log('Loading Stripe script');
  const script = document.createElement('script');
  script.id = 'stripe-js';
  script.src = 'https://js.stripe.com/v3/';
  script.async = true;
  document.head.appendChild(script);
}

// Initialize Stripe with the provided publishable key
function initializeStripe(publishableKey: string) {
  try {
    if (!window.Stripe) {
      console.error('Stripe not loaded yet');
      return false;
    }
    
    stripeInstance = window.Stripe(publishableKey);
    console.log('Stripe initialized successfully');
    return true;
  } catch (error) {
    console.error('Error initializing Stripe:', error);
    return false;
  }
}

// Redirect to Stripe Checkout using sessionId
async function redirectToCheckout(sessionId: string) {
  try {
    if (!stripeInstance) {
      console.error('Stripe not initialized');
      return { success: false, error: 'Stripe not initialized' };
    }
    
    console.log('Redirecting to Checkout with session:', sessionId);
    const { error } = await stripeInstance.redirectToCheckout({ sessionId });
    
    if (error) {
      console.error('Checkout error:', error);
      return { success: false, error: error.message };
    }
    
    return { success: true };
  } catch (error) {
    console.error('Redirect to checkout error:', error);
    return { success: false, error: error instanceof Error ? error.message : 'Unknown error' };
  }
}

// Process a payment using Stripe Elements
async function processPayment(clientSecret: string) {
  try {
    if (!stripeInstance) {
      console.error('Stripe not initialized');
      return { success: false, error: 'Stripe not initialized' };
    }
    
    // Set up payment element
    const elements = stripeInstance.elements({
      clientSecret,
      appearance: {
        theme: 'stripe',
      },
    });
    
    const paymentElement = elements.create('payment');
    
    // Create a container for the payment element
    const paymentContainer = document.createElement('div');
    paymentContainer.id = 'payment-element';
    document.body.appendChild(paymentContainer);
    
    // Mount the payment element
    paymentElement.mount('#payment-element');
    
    // Handle the form submission
    const { error } = await stripeInstance.confirmPayment({
      elements,
      confirmParams: {
        return_url: window.location.origin + '/dashboard?payment_success=true',
      },
    });
    
    // Clean up
    document.body.removeChild(paymentContainer);
    
    if (error) {
      console.error('Payment error:', error);
      return { success: false, error: error.message };
    }
    
    return { success: true };
  } catch (error) {
    console.error('Process payment error:', error);
    return { success: false, error: error instanceof Error ? error.message : 'Unknown error' };
  }
}

// Clean up Stripe resources
function cleanupStripe() {
  stripeInstance = null;
}

// Export the Stripe integration object to be attached to the window
const stripeIntegration = {
  initializeStripe,
  redirectToCheckout,
  processPayment,
  cleanupStripe
};

// Attach to window for access from Elm ports
if (typeof window !== 'undefined') {
  (window as any).stripeIntegration = stripeIntegration;
}

export { stripeIntegration };

// Add TypeScript declarations for Stripe
declare global {
  interface Window {
    Stripe: any;
  }
} 