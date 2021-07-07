import Vue from 'vue'
import App from './App.vue'
import store from "./store/index.js"

import AudioVisual from 'vue-audio-visual'
import Icon from 'vue-svg-icon/Icon.vue';

Vue.component('icon', Icon);
Vue.use(AudioVisual)

Vue.config.productionTip = false

new Vue({
  render: h => h(App),
  store
}).$mount('#app')
