<template>
  <div id="app">
    <HelloWorld msg="EAF can now use Vue.js to write applications!"/>
  </div>
</template>

<script>
 import HelloWorld from './components/HelloWorld.vue'

 export default {
   name: 'App',
   components: {
     HelloWorld
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
