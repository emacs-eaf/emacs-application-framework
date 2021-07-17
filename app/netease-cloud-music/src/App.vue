<template>
  <div id="app">
    <UserInfo
      :borderColor="foregroundColor"
      :style="{ 'z-index': '350', 'background-color': backgroundColor }" />
    <Playlist
      :backgroundColor="backgroundColor"
      :foregroundColor="foregroundColor"
      style="z-index: 300;"/>
    <Panel
      :borderColor="foregroundColor"
      :style="{ 'z-index': '500', 'background-color': backgroundColor }" />
  </div>
</template>

<script>
 import Playlist from './components/Playlist.vue'
 import UserInfo from './components/UserInfo.vue'
 import Panel from './components/Panel.vue'

 export default {
   name: 'App',
   components: {
     Playlist,
     UserInfo,
     Panel
   },

   data() {
     return {
       backgroundColor: '',
       foregroundColor: ''
     }
   },

   mounted() {
     window.initColor = this.initColor;
   },

   methods: {
     initColor(backgroundColor, foregroundColor) {
       this.backgroundColor = backgroundColor;
       this.foregroundColor = foregroundColor;
     },
   }
 }
</script>

<style>
 #app {
   display: flex;
   position: absolute;
   align-items: center;
   width: 100%;
   height: 100%;
 }

 * {
   margin: 0;
   padding: 0;
 }

 html, body {
   width: 100%;
   height: 100%;
   user-select: none;
 }

 ::-webkit-scrollbar {
   display: none;
 }
</style>
