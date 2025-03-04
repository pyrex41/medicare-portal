import './styles.css'
import { Elm } from './Main.elm'
import './stripe-integration.js'

const root = document.querySelector('#app')
if (!root) {
  console.error('Could not find root element')
  throw new Error('Could not find root element')
}

// Get session cookie if it exists
const getCookie = (name: string) => {
  const value = `; ${document.cookie}`
  const parts = value.split(`; ${name}=`)
  if (parts.length === 2) return parts.pop()?.split(';').shift()
  return null
}

// Function to delete a cookie by setting its expiration date to the past
const deleteCookie = (name: string) => {
  // Delete with various path and domain combinations to ensure it's removed
  document.cookie = `${name}=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;`
  document.cookie = `${name}=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/; domain=${window.location.hostname};`
  document.cookie = `${name}=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/; domain=.${window.location.hostname};`
  console.log(`Cookie '${name}' deletion attempted`)
}

const sessionCookie = getCookie('session')
console.log('Found session cookie:', sessionCookie)

try {
  // @ts-ignore - Will be used for ports in the future
  const app = Elm.Main.init({
    node: root,
    flags: {
      apiUrl: 'http://localhost:3000',
      initialSession: sessionCookie || null
    }
  })

  // Add any ports here if needed
  // app.ports.sendToJs.subscribe((data) => {
  //   console.log('From Elm:', data)
  // })
  
  // Port to clear the session cookie when logging out
  if (app.ports && app.ports.clearSessionCookie) {
    app.ports.clearSessionCookie.subscribe(() => {
      console.log('Clearing session cookie from JS')
      console.log('Current cookies before deletion:', document.cookie)
      deleteCookie('session')
      
      // Check if cookie was deleted
      setTimeout(() => {
        console.log('Cookies after deletion attempt:', document.cookie)
        const stillExists = getCookie('session')
        if (stillExists) {
          console.warn('Session cookie still exists after deletion attempt:', stillExists)
        } else {
          console.log('Session cookie successfully deleted')
        }
      }, 100)
    })
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
  }
} catch (error) {
  console.error('Failed to initialize Elm application:', error)
  root.innerHTML = 'Failed to load application. Please try refreshing the page.'
}
