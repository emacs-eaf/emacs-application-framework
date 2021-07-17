import Vue from 'vue'
import App from './App.vue'
import Icon from 'vue-svg-icon/Icon.vue'

Vue.config.productionTip = false

Vue.component('icon', Icon);

new Vue({
  render: h => h(App),
}).$mount('#app')
