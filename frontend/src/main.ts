import './styles.css'
import { Elm } from './Main.elm'
import './stripe-integration.js'

const root = document.querySelector('#app')
if (!root) {
  console.error('Could not find root element')
  throw new Error('Could not find root element')
}



// Favicon is now set directly in index.html

try {
  console.log('Initializing Elm application...');
  
  // Verify the Elm object exists and has required properties
  if (!Elm) {
    throw new Error('Elm object is not defined');
  }
  
  if (!Elm.Main) {
    throw new Error('Elm.Main is not defined. Make sure the Elm application is correctly compiled.');
  }
  
  if (typeof Elm.Main.init !== 'function') {
    throw new Error('Elm.Main.init is not a function. The Elm application might not be correctly compiled.');
  }
  
  // Log Elm details for debugging
  console.log('Elm available:', !!Elm);
  console.log('Elm.Main available:', !!Elm.Main);
  console.log('Elm.Main.init available:', !!(Elm.Main && typeof Elm.Main.init === 'function'));
  
  // Create the flags object with only what's needed by Elm

    
  // @ts-ignore - Will be used for ports in the future
  const app = Elm.Main.init({
    node: root  
  });
  
  // Expose app and Elm objects for debugging
  (window as any).elmApp = app;
  (window as any).elmDebug = Elm;
  
  // Setup IntersectionObserver for phone section
  if (app.ports && app.ports.viewingPhone) {
    // Wait for the DOM to be fully rendered
    setTimeout(() => {
      // Find the phone section container - being more specific with the selector
      const phoneSection = document.querySelector('.relative.h-\\[400px\\].w-\\[280px\\].rounded-\\[30px\\].overflow-hidden')
      
      if (phoneSection) {
        console.log('Found phone section, setting up IntersectionObserver')
        
        // Create an observer
        const observer = new IntersectionObserver((entries) => {
          entries.forEach(entry => {
            // When the section becomes visible, send true through the port
            if (entry.isIntersecting) {
              console.log('Phone section is now visible')
              app.ports.viewingPhone.send(true)
            } else {
              console.log('Phone section is no longer visible')
              app.ports.viewingPhone.send(false)
            }
          })
        }, {
          threshold: 0.2 // Fire when at least 20% of the element is visible
        })
        
        // Start observing the phone section
        observer.observe(phoneSection)
      } else {
        console.warn('Could not find phone section for carousel')
      }
    }, 1000) // Give the app time to render
  }
  
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
        app.ports.receiveOrgSlug.send("")
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
  console.error('Error initializing Elm application:', error);
  
  // Display a user-friendly error message
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
