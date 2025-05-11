import './styles.css'
import { Elm } from './Main.elm'
import * as Chartist from 'chartist';
import 'chartist/dist/index.css';

// Declare Stripe for TypeScript
declare const Stripe: any;
// Declare Chartist for TypeScript
// declare const Chartist: any;

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

// Define the Chartist Bar chart custom element (existing implementation)
customElements.define('chartist-bar', class extends HTMLElement {
  static get observedAttributes() { return ['data']; }
  connectedCallback() { this.renderChart(); }
  attributeChangedCallback() { this.renderChart(); }
  renderChart() {
    const dataAttr = this.getAttribute('data');
    if (!dataAttr) return;
    let chartData;
    try {
      chartData = JSON.parse(dataAttr);
    } catch (e) {
      this.textContent = 'Invalid chart data';
      return;
    }
    this.innerHTML = '';
    const chartDiv = document.createElement('div');
    chartDiv.style.height = '100%';
    chartDiv.style.width = '100%';
    this.appendChild(chartDiv);
    
    // Add custom CSS for series colors
    const style = document.createElement('style');
    style.textContent = `
      .ct-series-a .ct-bar, .ct-series-a .ct-line, .ct-series-a .ct-point { stroke: #03045e !important; }
      .ct-series-b .ct-bar, .ct-series-b .ct-line, .ct-series-b .ct-point { stroke: #0077b6 !important; }
      .ct-series-c .ct-bar, .ct-series-c .ct-line, .ct-series-c .ct-point { stroke: #00b4d8 !important; }
      .ct-series-d .ct-bar, .ct-series-d .ct-line, .ct-series-d .ct-point { stroke: #48cae4 !important; }
    `;
    this.appendChild(style);
    
    new Chartist.BarChart(chartDiv, chartData, {
      stackBars: false,
      axisY: { onlyInteger: true }
    });
  }
});

// Define the Chartist Line chart custom element (new)
customElements.define('chartist-line', class extends HTMLElement {
  static get observedAttributes() { return ['data']; }
  connectedCallback() { this.renderChart(); }
  attributeChangedCallback() { this.renderChart(); }
  renderChart() {
    const dataAttr = this.getAttribute('data');
    if (!dataAttr) return;
    let chartData;
    try {
      chartData = JSON.parse(dataAttr);
    } catch (e) {
      this.textContent = 'Invalid chart data';
      return;
    }
    this.innerHTML = '';
    const chartDiv = document.createElement('div');
    chartDiv.style.height = '100%';
    chartDiv.style.width = '100%';
    this.appendChild(chartDiv);
    
    // Add custom CSS for series colors
    const style = document.createElement('style');
    style.textContent = `
      .ct-series-a .ct-bar, .ct-series-a .ct-line, .ct-series-a .ct-point { stroke: #03045e !important; }
      .ct-series-b .ct-bar, .ct-series-b .ct-line, .ct-series-b .ct-point { stroke: #0077b6 !important; }
      .ct-series-c .ct-bar, .ct-series-c .ct-line, .ct-series-c .ct-point { stroke: #00b4d8 !important; }
      .ct-series-d .ct-bar, .ct-series-d .ct-line, .ct-series-d .ct-point { stroke: #48cae4 !important; }
    `;
    this.appendChild(style);
    
    new Chartist.LineChart(chartDiv, chartData, {
      fullWidth: true,
      chartPadding: {
        right: 40
      },
      lineSmooth: Chartist.Interpolation.cardinal({
        tension: 0.2
      }),
      axisY: {
        onlyInteger: true
      }
    });
  }
});

// Define the Chartist Funnel chart custom element
customElements.define('chartist-funnel', class extends HTMLElement {
  static get observedAttributes() { return ['data']; }
  connectedCallback() { this.renderChart(); }
  attributeChangedCallback() { this.renderChart(); }
  renderChart() {
    const dataAttr = this.getAttribute('data');
    if (!dataAttr) return;
    let chartData;
    try {
      chartData = JSON.parse(dataAttr);
    } catch (e) {
      this.textContent = 'Invalid chart data';
      return;
    }
    
    this.innerHTML = '';
    
    // Custom parent container for better layout control
    const chartContainer = document.createElement('div');
    chartContainer.className = 'funnel-chart-container';
    chartContainer.style.height = '100%';
    chartContainer.style.width = '100%';
    chartContainer.style.display = 'flex';
    chartContainer.style.flexDirection = 'column';
    chartContainer.style.justifyContent = 'space-between';
    this.appendChild(chartContainer);
    
    // Add custom CSS with improved styling
    const style = document.createElement('style');
    style.textContent = `
      .funnel-chart-container {
        font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
        padding: 10px 20px;
        height: 100%;
        box-sizing: border-box;
        display: flex;
        flex-direction: column;
        justify-content: space-around;
      }
      .funnel-bar {
        height: 35px;
        border-radius: 4px;
        margin: 10px 0;
        position: relative;
        overflow: visible;
        transition: width 0.3s ease-in-out;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .funnel-bar-label {
        position: absolute;
        left: -140px;
        width: 130px;
        text-align: right;
        font-size: 14px;
        font-weight: 500;
        line-height: 35px;
        color: #333;
      }
      .funnel-bar-value {
        position: absolute;
        right: -45px;
        font-size: 14px;
        font-weight: 500;
        line-height: 35px;
        color: #666;
      }
      .funnel-bar-quotes-sent { background-color: #03045e; }
      .funnel-bar-quotes-viewed { background-color: #0077b6; }
      .funnel-bar-health-completed { background-color: #48cae4; }
    `;
    this.appendChild(style);
    
    // For a clean funnel visualization, create custom HTML bars
    if (chartData && chartData.labels && chartData.series) {
      console.log('Funnel chart data:', chartData);

      // Extract values from the chartData
      // Each series should be a single data point from the most recent data
      let values = [];

      // Try to extract values from series in the correct format
      if (Array.isArray(chartData.series)) {
        if (chartData.series.length === 1 && Array.isArray(chartData.series[0])) {
          // Format: { series: [[v1, v2, v3, v4]] }
          values = chartData.series[0].map(v => parseFloat(v) || 0);
        } else if (chartData.series.length > 0 && Array.isArray(chartData.series[0]) && chartData.series[0].length === 1) {
          // Format: { series: [[v1], [v2], [v3], [v4]] }
          values = chartData.series.map(s => parseFloat(s[0]) || 0);
        } else {
          // Format: { series: [v1, v2, v3, v4] }
          values = chartData.series.map(v => parseFloat(v) || 0);
        }
      }

      // Ensure we have 4 values
      while (values.length < 4) values.push(0);
      values = values.slice(0, 4);

      console.log('Funnel values:', values);

      // Find the max value for scaling
      // Use 2x the Quotes Sent value as the maximum scale to make the drop-off less steep
      const quotesSentValue = values[0] || 1;
      const maxValue = Math.max(quotesSentValue * 2, 1); // 2x the Quotes Sent value, with minimum of 1

      // Define custom bar data with actual values - in reverse order for proper funnel flow
      // Health Completed (smallest) at the top, Quotes Sent (largest) at the bottom
      const barData = [
        { label: 'Health Completed', class: 'funnel-bar-health-completed', value: values[3] || 0, display: Math.round(values[3] || 0) },
        { label: 'Quotes Viewed', class: 'funnel-bar-quotes-viewed', value: values[1] || 0, display: Math.round(values[1] || 0) },
        { label: 'Quotes Sent', class: 'funnel-bar-quotes-sent', value: values[0] || 0, display: Math.round(values[0] || 0) }
      ];

      // Container for better spacing
      const innerContainer = document.createElement('div');
      innerContainer.style.paddingLeft = '140px'; // Space for labels
      innerContainer.style.paddingRight = '60px'; // Space for values
      innerContainer.style.width = '100%';
      innerContainer.style.position = 'relative';
      chartContainer.appendChild(innerContainer);

      // Create a bar for each item
      barData.forEach(item => {
        const barContainer = document.createElement('div');
        barContainer.style.display = 'flex';
        barContainer.style.alignItems = 'center';
        barContainer.style.position = 'relative';
        barContainer.style.marginBottom = '25px';

        const barLabel = document.createElement('div');
        barLabel.className = 'funnel-bar-label';
        barLabel.textContent = item.label;
        barContainer.appendChild(barLabel);

        const bar = document.createElement('div');
        bar.className = `funnel-bar ${item.class}`;

        // Scale the width based on value (relative to max value)
        const percentage = Math.max(5, (item.value / maxValue) * 100);
        bar.style.width = `${percentage}%`;

        // Add the value as text inside the bar if it's large enough
        if (percentage > 15) {
          bar.style.color = 'white';
          bar.style.paddingLeft = '12px';
          bar.style.display = 'flex';
          bar.style.alignItems = 'center';
          bar.textContent = item.display.toString();
        }

        // Add a separate value label outside the bar
        const valueLabel = document.createElement('div');
        valueLabel.className = 'funnel-bar-value';
        valueLabel.textContent = item.display.toString();
        barContainer.appendChild(valueLabel);

        barContainer.appendChild(bar);
        innerContainer.appendChild(barContainer);
      });
    }
  }
});

const root = document.querySelector('#app');
if (!root) {
  console.error('Could not find root element');
  throw new Error('Could not find root element');
}

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
      const phoneSection = document.getElementById('phone-experience-section')
      
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