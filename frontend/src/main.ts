import './styles.css'
import { Elm } from './Main.elm'

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
} catch (error) {
  console.error('Failed to initialize Elm application:', error)
  root.innerHTML = 'Failed to load application. Please try refreshing the page.'
}
