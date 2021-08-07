import Vue from 'vue'
import App from './App.vue'

import AudioVisual from 'vue-audio-visual'
Vue.use(AudioVisual)

Vue.config.productionTip = false

new Vue({
  render: h => h(App),
}).$mount('#app')
