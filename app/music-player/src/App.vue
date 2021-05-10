<template>
  <div id="app">
    <Playlist/>
  </div>
</template>

<script>
 import Playlist from './components/Playlist.vue'

 export default {
   name: 'App',
   components: {
     Playlist
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
