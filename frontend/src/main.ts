import './styles.css'
import { Elm } from './Main.elm'
import './stripe-integration.js'

const root = document.querySelector('#app')
if (!root) {
  console.error('Could not find root element')
  throw new Error('Could not find root element')
}



// Add a hook to capture redirects in case we're still having issues
const originalPushState = history.pushState;
history.pushState = function(data: any, unused: string, url?: string | URL | null) {
  console.warn('NAVIGATION:', 'pushState', url);
  // Also save to debug info storage
  const redirectInfo = `Redirect: ${url}`;
  localStorage.setItem('redirect_debug_info', redirectInfo);
  sessionStorage.setItem('redirect_debug_info', redirectInfo);
  return originalPushState.apply(this, [data, unused, url]);
};

const originalReplaceState = history.replaceState;
history.replaceState = function(data: any, unused: string, url?: string | URL | null) {
  console.warn('NAVIGATION:', 'replaceState', url);
  // Also save to debug info storage
  const redirectInfo = `Redirect (replace): ${url}`;
  localStorage.setItem('redirect_debug_info', redirectInfo);
  sessionStorage.setItem('redirect_debug_info', redirectInfo);
  return originalReplaceState.apply(this, [data, unused, url]);
};

// Check if we're on an error page and display debug info if available
const urlParams = new URLSearchParams(window.location.search);
const errorMsg = urlParams.get('message');
if (window.location.pathname === '/error' && errorMsg) {
  console.error('Error page detected:', errorMsg);
  
  // Get debug info
  const debugInfo = localStorage.getItem('selfservice_debug_info') || 
                   sessionStorage.getItem('selfservice_debug_info') || 
                   'No debug information available';
                   
  // Get redirect info
  const redirectInfo = localStorage.getItem('redirect_debug_info') || 
                      sessionStorage.getItem('redirect_debug_info') || 
                      'No redirect information available';
  
  // Show debugging information on the page
  root.innerHTML = `
    <div style="max-width: 800px; margin: 50px auto; padding: 20px; font-family: sans-serif;">
      <h1 style="color: #e53e3e;">Error</h1>
      <p style="margin-bottom: 20px; font-size: 18px;">${errorMsg}</p>
      
      <h2 style="margin-top: 30px;">Debug Information</h2>
      <pre style="background: #f7fafc; padding: 15px; border-radius: 5px; overflow: auto; max-height: 300px;">${debugInfo}</pre>
      
      <h2 style="margin-top: 30px;">Redirect Information</h2>
      <pre style="background: #f7fafc; padding: 15px; border-radius: 5px; overflow: auto; max-height: 300px;">${redirectInfo}</pre>
      
      <div style="margin-top: 30px;">
        <button onclick="window.history.back()" style="padding: 10px 20px; background: #4299e1; color: white; border: none; border-radius: 5px; cursor: pointer;">Go Back</button>
        <button onclick="window.location.href='/'" style="margin-left: 10px; padding: 10px 20px; background: #718096; color: white; border: none; border-radius: 5px; cursor: pointer;">Go Home</button>
      </div>
    </div>
  `;
  
  // Don't initialize Elm on error pages
  throw new Error('Skipping Elm initialization on error page');
}

// Get session cookie if it exists
const getCookie = (name: string) => {
  const value = `; ${document.cookie}`
  const parts = value.split(`; ${name}=`)
  if (parts.length === 2) return parts.pop()?.split(';').shift()
  return null
}

const sessionCookie = getCookie('session')
console.log('Found session cookie:', sessionCookie)

// Favicon is now set directly in index.html

try {
  // @ts-ignore - Will be used for ports in the future
  const app = Elm.Main.init({
    node: root,
    flags: {
      apiUrl: 'http://localhost:3000',
      initialSession: sessionCookie || null
    }
  })


  
  // Stripe integration ports
  if (app.ports) {
    // Initialize Stripe
    if (app.ports.initializeStripe) {
      app.ports.initializeStripe.subscribe((publishableKey: string) => {
        console.log('Initializing Stripe with key:', publishableKey.substring(0, 8) + '...')
        try {
          // @ts-ignore - stripeIntegration is attached to window
          const initialized = window.stripeIntegration.initializeStripe(publishableKey)
          if (app.ports.stripeInitialized) {
            app.ports.stripeInitialized.send(initialized)
          }
        } catch (error) {
          console.error('Failed to initialize Stripe:', error)
          if (app.ports.stripeInitialized) {
            app.ports.stripeInitialized.send(false)
          }
        }
      })
    }

    // get org slug
    if (app.ports.getOrgSlug) {
      app.ports.getOrgSlug.subscribe(() => {
        console.log('Getting org slug')
        const orgSlug = getCookie('orgSlug')
        if (orgSlug) {
          app.ports.receiveOrgSlug.send(orgSlug)
        } else {
          console.log('No orgSlug cookie found')
          if (app.ports.receiveOrgSlug) {
            app.ports.receiveOrgSlug.send("")
          }
        }
      })
    }

    // Process payment
    if (app.ports.processPayment) {
      app.ports.processPayment.subscribe((clientSecret: string) => {
        console.log('Processing payment with client secret:', clientSecret.substring(0, 8) + '...')
        try {
          // @ts-ignore - stripeIntegration is attached to window
          window.stripeIntegration.processPayment(clientSecret)
            .then((result: any) => {
              if (app.ports.paymentProcessed) {
                app.ports.paymentProcessed.send(result)
              }
            })
            .catch((error: Error) => {
              console.error('Payment processing error:', error)
              if (app.ports.paymentProcessed) {
                app.ports.paymentProcessed.send({ success: false, error: error.message })
              }
            })
        } catch (error) {
          console.error('Failed to process payment:', error)
          if (app.ports.paymentProcessed) {
            app.ports.paymentProcessed.send({ success: false, error: 'Failed to process payment' })
          }
        }
      })
    }

    // Clean up Stripe
    if (app.ports.cleanupStripe) {
      app.ports.cleanupStripe.subscribe(() => {
        console.log('Cleaning up Stripe')
        try {
          // @ts-ignore - stripeIntegration is attached to window
          window.stripeIntegration.cleanupStripe()
        } catch (error) {
          console.error('Failed to clean up Stripe:', error)
        }
      })
    }
    
    // Copy to clipboard
    if (app.ports.copyToClipboard) {
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

    // Debug info ports
    if (app.ports.saveDebugInfo) {
      app.ports.saveDebugInfo.subscribe((debugInfo: string) => {
        console.log('Debug info:', debugInfo);
        localStorage.setItem('selfservice_debug_info', debugInfo);
        
        // Write to console with more details for better visibility
        console.warn('SELF-SERVICE DEBUG:', debugInfo);
        
        // Also log to session storage in case localStorage is cleared
        sessionStorage.setItem('selfservice_debug_info', debugInfo);
      });
    }

    if (app.ports.clearDebugInfo) {
      app.ports.clearDebugInfo.subscribe(() => {
        console.log('Clearing debug info');
        localStorage.removeItem('selfservice_debug_info');
        sessionStorage.removeItem('selfservice_debug_info');
      });
    }
  }
} catch (error) {
  console.error('Failed to initialize Elm application:', error)
  root.innerHTML = 'Failed to load application. Please try refreshing the page.'
}
