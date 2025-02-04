import './styles.css'
import { Elm } from './Main.elm'

const root = document.querySelector('#app')
if (root) {
  const app = Elm.Main.init({
    node: root,
    flags: {
      apiUrl: 'http://localhost:3000'  // Pass the API URL as a flag
    }
  })

  // Add any ports here if needed
  // app.ports.sendToJs.subscribe((data) => {
  //   console.log('From Elm:', data)
  // })
}
