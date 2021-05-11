import Vue from 'vue'
import App from './App.vue'

import { library } from '@fortawesome/fontawesome-svg-core'
import { faPlayCircle, faPauseCircle, faStepForward, faStepBackward } from '@fortawesome/free-solid-svg-icons'
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome'

import AudioVisual from 'vue-audio-visual'

library.add(faPlayCircle, faPauseCircle, faStepForward, faStepBackward)
Vue.component('font-awesome-icon', FontAwesomeIcon)

Vue.config.productionTip = false

Vue.use(AudioVisual)

new Vue({
  render: h => h(App),
}).$mount('#app')
