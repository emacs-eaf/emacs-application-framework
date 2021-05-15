<template>
  <div id="app">
    <Playlist
      class="playlist"
      :style="{ 'margin-bottom': panelHeight }"/>
    <Panel
      :style="{ 'height': panelHeight }">
    </Panel>
  </div>
</template>

<script>
 import Playlist from './components/Playlist.vue'
 import Panel from './components/Panel.vue'

 export default {
   name: 'App',
   components: {
     Playlist,
     Panel
   },
   data() {
     return {
       panelHeight: "90px"
     }
   },
   props: {
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

   position: relative;
 }

 html, body {
   width: 100%;
   height: 100%;
   margin: 0;
   padding: 0;
 }

 ::-webkit-scrollbar {
   display: none;
 }

 .playlist {
   overflow: scroll;
 }
</style>
