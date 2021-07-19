<template>
  <div id="app">
    <Main msg="EAF can now use Vue.js to write applications!"/>
  </div>
</template>

<script>
 import Main from './components/Main.vue'

 export default {
   name: 'App',
   components: {
     Main
   },
   mounted() {
   }
 }
</script>

<style>
 #app {
   display: flex;
   flex-direction: column;
   align-items: center;
   justify-content: center;
   width: 100%;
   height: 100%;
 }

 html, body {
   width: 100%;
   height: 100%;
   margin: 0;
   padding: 0;
 }
</style>
