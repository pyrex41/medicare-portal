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

const sessionCookie = getCookie('session')
console.log('Found session cookie:', sessionCookie)

try {
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
} catch (error) {
  console.error('Failed to initialize Elm application:', error)
  root.innerHTML = 'Failed to load application. Please try refreshing the page.'
}
