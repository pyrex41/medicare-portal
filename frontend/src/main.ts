import './styles.css'
import { Elm } from './Main.elm'

const root = document.querySelector('#app')
if (root) {
  Elm.Main.init({
    node: root
  })
}
