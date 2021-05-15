import Vue from 'vue'
import App from './App.vue'

import { library } from '@fortawesome/fontawesome-svg-core'
import { faPlayCircle, faPauseCircle, faStepForward, faStepBackward } from '@fortawesome/free-solid-svg-icons'
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome'

import store from "./store/index.js"

import VueEllipseProgress from 'vue-ellipse-progress';

library.add(faPlayCircle, faPauseCircle, faStepForward, faStepBackward)
Vue.component('font-awesome-icon', FontAwesomeIcon)

Vue.use(VueEllipseProgress);

Vue.config.productionTip = false

new Vue({
  render: h => h(App),
  store
}).$mount('#app')
