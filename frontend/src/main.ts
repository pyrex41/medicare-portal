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
    this.stripe = null;
    this.checkout = null;
  }

  async connectedCallback() {
    // Apply a wider style to the element
    this.style.width = '100%';
    this.style.maxWidth = '800px';
    this.style.minHeight = '500px';
    
    await this.initializeStripe();
    await this.mountCheckout();
  }

  attributeChangedCallback() {
    if (this.isConnected) {
      this.mountCheckout();
    }
  }

  static get observedAttributes() {
    return ['price-id', 'metered-price-id', 'return-url', 'first-name', 'last-name', 'email'];
  }

  async initializeStripe() {
    if (!this.stripe) {
      const stripeScript = document.createElement('script');
      stripeScript.src = 'https://js.stripe.com/v3/';
      document.head.appendChild(stripeScript);
      await new Promise(resolve => stripeScript.onload = resolve);
      
      // Use environment variable if available, otherwise fallback to the hardcoded key
      const publishableKey = import.meta.env.VITE_STRIPE_PUBLISHABLE_KEY || 
        'pk_test_51Qyh7RCBUPXAZKNGAvsWikdxCCaV1R9Vc79IgPqCul8AJsln69ABDQZE0zzOtOlH5rqrlw2maRebndvPl8xDaIVl00Nn2OOBCX';
      
      console.log('Initializing Stripe with publishable key:', publishableKey.substring(0, 20) + '...');
      this.stripe = Stripe(publishableKey);
    }
  }

  async mountCheckout() {
    const priceId = this.getAttribute('price-id');
    const meteredPriceId = this.getAttribute('metered-price-id');
    const returnUrl = this.getAttribute('return-url') || window.location.href;
    const firstName = this.getAttribute('first-name');
    const lastName = this.getAttribute('last-name');
    const email = this.getAttribute('email');

    if (!priceId) {
      this.textContent = 'Error: Missing price-id attribute';
      return;
    }

    // Check for required user information
    if (!firstName || !lastName || !email) {
      this.textContent = 'Error: Missing user information';
      return;
    }

    try {
      // Set the element to loading state
      console.log(`Creating checkout session with priceId: ${priceId}, meteredPriceId: ${meteredPriceId || 'none'}, email: ${email}`);
      
      // Create a checkout session with the API
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

      const data = await response.json();
      
      if (!response.ok) {
        throw new Error(data.message || 'Failed to create checkout session');
      }

      const { clientSecret } = data;

      // Create the checkout form
      const checkout = await this.stripe!.initEmbeddedCheckout({
        clientSecret,
      });

      // Mount the checkout form to the element
      await checkout.mount(this);

    } catch (error) {
      console.error('Error mounting checkout:', error);
      this.textContent = `Error: ${error instanceof Error ? error.message : 'Failed to load payment form'}`;
    }
  }

  async createCheckoutSession(priceId: string, meteredPriceId: string | null, returnUrl: string, email?: string, name?: string) {
    try {
      console.log(`Creating checkout session for base price: ${priceId}, metered price: ${meteredPriceId}, email: ${email || 'not provided'}`);
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
      
      if (!response.ok) {
        const errorText = await response.text();
        console.error(`Checkout session creation failed with status ${response.status}:`, errorText);
        throw new Error(`Failed to create checkout session: ${response.status} ${errorText}`);
      }
      
      const data = await response.json();
      console.log('Checkout session created successfully:', { success: data.success });
      return data;
    } catch (error: any) {
      console.error('Error in createCheckoutSession:', error);
      throw error;
    }
  }

  disconnectedCallback() {
    if (this.checkout) {
      this.checkout.destroy();
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

  // Listen for custom events from stripe-checkout
  document.addEventListener('payment-completed', (e: any) => {
    if (app.ports && app.ports.paymentCompleted) {
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