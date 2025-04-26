import './styles.css'
import { Elm } from './Main.elm'

// Declare Stripe for TypeScript
declare const Stripe: any;

const root = document.querySelector('#app')
if (!root) {
  console.error('Could not find root element')
  throw new Error('Could not find root element')
}

// Define the Stripe Checkout custom element
customElements.define('stripe-checkout', class extends HTMLElement {
  private stripe: any;
  private checkout: any;

  constructor() {
    super();
    console.log('[Stripe] Creating new stripe-checkout element');
    this.stripe = null;
    this.checkout = null;
  }

  async connectedCallback() {
    console.log('[Stripe] Element connected to DOM');
    // Check if there's already an active instance
    if ((this.constructor as any).activeInstance) {
      console.log('[Stripe] Cleaning up previous Stripe Checkout instance');
      const prevInstance = (this.constructor as any).activeInstance;
      if (prevInstance.checkout) {
        await prevInstance.checkout.destroy();
      }
      prevInstance.remove();
    }

    // Set this as the active instance
    (this.constructor as any).activeInstance = this;
    console.log('[Stripe] Set as active checkout instance');

    // Apply a wider style to the element
    this.style.width = '100%';
    this.style.maxWidth = '800px';
    this.style.minHeight = '500px';
    
    await this.initializeStripe();
    await this.mountCheckout();
  }

  attributeChangedCallback(name: string, oldValue: string, newValue: string) {
    console.log(`[Stripe] Attribute changed: ${name} from "${oldValue}" to "${newValue}"`);
    if (this.isConnected) {
      this.mountCheckout();
    }
  }

  static get observedAttributes() {
    return ['price-id', 'metered-price-id', 'return-url', 'first-name', 'last-name', 'email'];
  }

  async initializeStripe() {
    if (!this.stripe) {
      console.log('[Stripe] Loading Stripe.js script');
      const stripeScript = document.createElement('script');
      stripeScript.src = 'https://js.stripe.com/v3/';
      document.head.appendChild(stripeScript);
      await new Promise(resolve => stripeScript.onload = resolve);
      console.log('[Stripe] Stripe.js script loaded');
      
      // Use environment variable if available, otherwise fallback to the hardcoded key
      const publishableKey = import.meta.env.VITE_STRIPE_PUBLISHABLE_KEY || 
        'pk_test_51Qyh7RCBUPXAZKNGAvsWikdxCCaV1R9Vc79IgPqCul8AJsln69ABDQZE0zzOtOlH5rqrlw2maRebndvPl8xDaIVl00Nn2OOBCX';
      
      console.log('[Stripe] Initializing Stripe with publishable key:', publishableKey.substring(0, 20) + '...');
      this.stripe = Stripe(publishableKey);
      console.log('[Stripe] Stripe initialized successfully');
    }
  }

  async mountCheckout() {
    console.log('[Stripe] Mounting checkout form');
    const priceId = this.getAttribute('price-id');
    const meteredPriceId = this.getAttribute('metered-price-id');
    const firstName = this.getAttribute('first-name');
    const lastName = this.getAttribute('last-name');
    const email = this.getAttribute('email');

    console.log('[Stripe] Checkout attributes:', { 
      priceId, 
      meteredPriceId, 
      firstName, 
      lastName, 
      email: email ? `${email.substring(0, 3)}...${email.split('@')[1] ? '@' + email.split('@')[1] : ''}` : null 
    });

    if (!priceId || !firstName || !lastName || !email) {
      console.error('[Stripe] Missing required attributes for checkout');
      this.textContent = 'Error: Missing required attributes';
      return;
    }

    try {
      console.log(`[Stripe] Creating checkout session with priceId: ${priceId}, meteredPriceId: ${meteredPriceId || 'none'}, email: ${email}`);
      
      console.log('[Stripe] Sending request to /api/create-checkout-session');
      const response = await fetch('/api/create-checkout-session', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          priceId,
          meteredPriceId,
          customerEmail: email,
          customerName: `${firstName} ${lastName}`
        }),
      });

      console.log(`[Stripe] Create checkout session response status: ${response.status}`);
      const data = await response.json();
      console.log('[Stripe] Checkout session response data:', data);
      
      if (!response.ok) {
        console.error('[Stripe] Failed to create checkout session:', data.message || 'Unknown error');
        throw new Error(data.message || 'Failed to create checkout session');
      }

      const { clientSecret } = data;
      console.log('[Stripe] Got client secret:', clientSecret ? `${clientSecret.substring(0, 10)}...` : 'null');

      // Destroy existing checkout if it exists
      if (this.checkout) {
        console.log('[Stripe] Destroying existing checkout instance');
        await this.checkout.destroy();
      }

      // Create the checkout form
      console.log('[Stripe] Initializing embedded checkout with client secret');
      this.checkout = await this.stripe.initEmbeddedCheckout({
        clientSecret,
        onComplete: async () => {
          console.log('[Stripe] Checkout complete callback triggered');
          try {
            console.log('[Stripe] Checkout complete! Fetching session data with clientSecret:', clientSecret.substring(0, 10) + '...');
            
            // Extract the session ID from the client secret (format: cs_<id>_secret_<secret>)
            const sessionId = clientSecret.split('_secret_')[0];
            console.log('[Stripe] Extracted session ID:', sessionId);
            
            // Poll for session status to ensure payment is complete
            console.log('[Stripe] Starting to poll for session status');
            this.pollSessionStatus(sessionId, email, firstName, lastName, clientSecret);
            
          } catch (error) {
            console.error('[Stripe] Error handling checkout completion:', error);
            
            const errorData = {
              success: false,
              message: error instanceof Error ? error.message : 'Payment completion failed',
            };

            console.log('[Stripe] Sending error data to Elm:', errorData);

            // Find the Elm app instance
            const elmApp = (window as any).elmApp;
            if (elmApp && elmApp.ports && elmApp.ports.checkoutError) {
              console.log('[Stripe] Sending checkout error to Elm via port');
              elmApp.ports.checkoutError.send(errorData);
            } else {
              // Fall back to event dispatching
              console.log('[Stripe] Elm app or port not found for error, dispatching event instead');
              const event = new CustomEvent('checkout-error', {
                detail: { error: errorData }
              });
              document.dispatchEvent(event);
            }
          }
        }
      });

      // Mount the checkout form to the element
      console.log('[Stripe] Mounting checkout to DOM element');
      await this.checkout.mount(this);
      console.log('[Stripe] Checkout mounted successfully');

    } catch (error) {
      console.error('[Stripe] Error mounting checkout:', error);
      this.textContent = `Error: ${error instanceof Error ? error.message : 'Failed to load payment form'}`;
    }
  }

  // Poll for session status to confirm payment completion
  async pollSessionStatus(sessionId: string, email: string, firstName: string, lastName: string, clientSecret: string, attempts = 0) {
    if (attempts > 5) {
      console.error('[Stripe] Max polling attempts reached for session:', sessionId);
      
      const errorData = {
        success: false,
        message: 'Timeout waiting for payment confirmation',
      };
      
      // Find the Elm app instance
      const elmApp = (window as any).elmApp;
      if (elmApp && elmApp.ports && elmApp.ports.checkoutError) {
        console.log('[Stripe] Sending checkout timeout error to Elm via port');
        elmApp.ports.checkoutError.send(errorData);
      }
      
      return;
    }
    
    try {
      console.log(`[Stripe] Polling attempt ${attempts + 1} for session status: ${sessionId}`);
      
      // Check the status of the session
      const response = await fetch(`/api/session-status?session_id=${sessionId}`, {
        method: 'GET',
        headers: { 'Content-Type': 'application/json' }
      });
      
      if (!response.ok) {
        console.error(`[Stripe] Error checking session status: ${response.status}`);
        setTimeout(() => this.pollSessionStatus(sessionId, email, firstName, lastName, clientSecret, attempts + 1), 2000);
        return;
      }
      
      const sessionData = await response.json();
      console.log('[Stripe] Session status response:', sessionData);
      
      if (sessionData.status === 'complete') {
        console.log('[Stripe] Session is complete, fetching detailed session data');
        
        // Get checkout session data which includes customer and subscription IDs
        const sessionDetailResponse = await fetch(`/api/checkout-session?clientSecret=${encodeURIComponent(sessionId + '_secret_' + clientSecret.split('_secret_')[1])}`, {
          method: 'GET',
          headers: { 'Content-Type': 'application/json' }
        });
        
        if (!sessionDetailResponse.ok) {
          console.error(`[Stripe] Error getting session details: ${sessionDetailResponse.status}`);
          setTimeout(() => this.pollSessionStatus(sessionId, email, firstName, lastName, clientSecret, attempts + 1), 2000);
          return;
        }
        
        const sessionDetailData = await sessionDetailResponse.json();
        console.log('[Stripe] Session details retrieved:', sessionDetailData);
        
        // Prepare the payment completion data
        const paymentData = {
          email,
          firstName,
          lastName,
          stripeCustomerId: sessionDetailData.customer,
          stripeSubscriptionId: sessionDetailData.subscription,
          stripeUsageItemId: sessionDetailData.subscription_item
        };
        
        console.log('[Stripe] Sending payment completion data:', JSON.stringify(paymentData));
        
        // Send data to payment-complete endpoint
        const paymentResponse = await fetch('/api/stripe/payment-complete', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(paymentData)
        });
        
        if (!paymentResponse.ok) {
          console.error(`[Stripe] Error completing payment: ${paymentResponse.status}`);
          const errorText = await paymentResponse.text();
          console.error('[Stripe] Payment completion error details:', errorText);
          
          const errorData = {
            success: false,
            message: `Payment processing error: ${paymentResponse.status}`,
          };
          
          // Send to Elm
          const elmApp = (window as any).elmApp;
          if (elmApp && elmApp.ports && elmApp.ports.checkoutError) {
            elmApp.ports.checkoutError.send(errorData);
          }
          
          return;
        }
        
        const paymentResult = await paymentResponse.json();
        console.log('[Stripe] Payment completion result:', paymentResult);
        
        // Send success to Elm
        const paymentStatus = {
          success: paymentResult.success,
          message: paymentResult.success ? 'Payment completed successfully' : 'Payment processing failed',
          paymentCompleted: paymentResult.success
        };
        
        const elmApp = (window as any).elmApp;
        if (elmApp && elmApp.ports && elmApp.ports.paymentCompleted) {
          console.log('[Stripe] Sending payment completion status to Elm via port:', paymentStatus);
          elmApp.ports.paymentCompleted.send(paymentStatus);
        } else {
          console.log('[Stripe] Elm app or port not found, dispatching event instead');
          const event = new CustomEvent('payment-completed', {
            detail: { status: paymentStatus }
          });
          document.dispatchEvent(event);
        }
        
      } else if (sessionData.status === 'open') {
        // Session still in progress, poll again
        console.log('[Stripe] Session still open, polling again in 2 seconds');
        setTimeout(() => this.pollSessionStatus(sessionId, email, firstName, lastName, clientSecret, attempts + 1), 2000);
      } else {
        // Other status (e.g., 'expired')
        console.log(`[Stripe] Session has status: ${sessionData.status}, stopping polling`);
        
        const errorData = {
          success: false,
          message: `Payment session ${sessionData.status}`,
        };
        
        // Send to Elm
        const elmApp = (window as any).elmApp;
        if (elmApp && elmApp.ports && elmApp.ports.checkoutError) {
          elmApp.ports.checkoutError.send(errorData);
        }
      }
      
    } catch (error) {
      console.error('[Stripe] Error polling session status:', error);
      setTimeout(() => this.pollSessionStatus(sessionId, email, firstName, lastName, clientSecret, attempts + 1), 2000);
    }
  }

  async createCheckoutSession(priceId: string, meteredPriceId: string | null, email?: string, name?: string) {
    try {
      console.log(`[Stripe] Creating checkout session for base price: ${priceId}, metered price: ${meteredPriceId}, email: ${email || 'not provided'}`);
      const response = await fetch('/api/create-checkout-session', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ 
          priceId,
          meteredPriceId,
          customerEmail: email,
          customerName: name
        })
      });
      
      console.log(`[Stripe] Create checkout session response status: ${response.status}`);
      
      if (!response.ok) {
        const errorText = await response.text();
        console.error(`[Stripe] Checkout session creation failed with status ${response.status}:`, errorText);
        throw new Error(`Failed to create checkout session: ${response.status} ${errorText}`);
      }
      
      const data = await response.json();
      console.log('[Stripe] Checkout session created successfully:', { success: data.success, clientSecret: data.clientSecret ? `${data.clientSecret.substring(0, 10)}...` : null });
      return data;
    } catch (error: any) {
      console.error('[Stripe] Error in createCheckoutSession:', error);
      throw error;
    }
  }

  async disconnectedCallback() {
    console.log('[Stripe] Stripe Checkout element disconnected, cleaning up...');
    if (this.checkout) {
      try {
        await this.checkout.destroy();
        this.checkout = null;
        console.log('[Stripe] Checkout instance destroyed successfully');
      } catch (error) {
        console.error('[Stripe] Error destroying checkout:', error);
      }
    }
    // Clear the active instance if this element is being removed
    if ((this.constructor as any).activeInstance === this) {
      (this.constructor as any).activeInstance = null;
      console.log('[Stripe] Cleared active instance reference');
    }
  }
});

// Initialize Elm app
try {
  console.log('Initializing Elm application...');
  
  if (!Elm) {
    throw new Error('Elm object is not defined');
  }
  
  if (!Elm.Main) {
    throw new Error('Elm.Main is not defined. Make sure the Elm application is correctly compiled.');
  }
  
  if (typeof Elm.Main.init !== 'function') {
    throw new Error('Elm.Main.init is not a function. The Elm application might not be correctly compiled.');
  }
  
  console.log('Elm available:', !!Elm);
  console.log('Elm.Main available:', !!Elm.Main);
  console.log('Elm.Main.init available:', !!(Elm.Main && typeof Elm.Main.init === 'function'));
  
  const app = Elm.Main.init({
    node: root  
  });
  
  (window as any).elmApp = app;
  (window as any).elmDebug = Elm;
  
  // Setup IntersectionObserver for phone section
  if (app.ports && app.ports.viewingPhone) {
    setTimeout(() => {
      const phoneSection = document.querySelector('.relative.h-\\[400px\\].w-\\[280px\\].rounded-\\[30px\\].overflow-hidden')
      
      if (phoneSection) {
        console.log('Found phone section, setting up IntersectionObserver')
        
        const observer = new IntersectionObserver((entries) => {
          entries.forEach(entry => {
            if (entry.isIntersecting) {
              console.log('Phone section is now visible')
              app.ports.viewingPhone.send(true)
            } else {
              console.log('Phone section is no longer visible')
              app.ports.viewingPhone.send(false)
            }
          })
        }, {
          threshold: 0.2
        })
        
        observer.observe(phoneSection)
      } else {
        console.warn('Could not find phone section for carousel')
      }
    }, 1000)
  }

  // Get org slug
  if (app.ports && app.ports.getOrgSlug) {
    app.ports.getOrgSlug.subscribe(() => {
      app.ports.receiveOrgSlug.send("")
    })
  }
    
  // Copy to clipboard
  if (app.ports && app.ports.copyToClipboard) {
    app.ports.copyToClipboard.subscribe((text: string) => {
      console.log('Copying to clipboard:', text.substring(0, 20) + '...')
      try {
        navigator.clipboard.writeText(text)
          .then(() => {
            console.log('Text copied to clipboard')
            if (app.ports.onCopyResult) {
              app.ports.onCopyResult.send(true)
            }
          })
          .catch((error) => {
            console.error('Failed to copy text to clipboard:', error)
            if (app.ports.onCopyResult) {
              app.ports.onCopyResult.send(false)
            }
          })
      } catch (error) {
        console.error('Clipboard API not available:', error)
        if (app.ports.onCopyResult) {
          app.ports.onCopyResult.send(false)
        }
      }
    })
  }

  // Listen for payment completion from stripe-checkout
  document.addEventListener('payment-completed', (e: any) => {
    if (app.ports && app.ports.paymentCompleted) {
      console.log('Sending payment completion status to Elm via port:', e.detail.status);
      app.ports.paymentCompleted.send(e.detail.status);
    }
  });

} catch (error) {
  console.error('Error initializing Elm application:', error);
  
  if (root) {
    root.innerHTML = `
      <div style="max-width: 800px; margin: 50px auto; padding: 20px; font-family: sans-serif; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px;">
        <h1 style="color: #721c24;">Elm Initialization Error</h1>
        <p style="margin-bottom: 20px; font-size: 16px;">There was a problem initializing the application:</p>
        <pre style="background: #f8f9fa; padding: 15px; border-radius: 5px; overflow: auto; max-height: 300px;">${error}</pre>
        <p style="margin-top: 20px; font-size: 14px;">Try refreshing the page or clearing your browser cache.</p>
        <button onclick="window.location.reload()" style="padding: 10px 20px; background: #dc3545; color: white; border: none; border-radius: 5px; cursor: pointer; margin-top: 10px;">Refresh Page</button>
      </div>
    `;
  }
}