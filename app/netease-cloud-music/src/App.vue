<template>
  <div id="app" :style="{ 'background-color': backgroundColor }">
    <UserInfo :borderColor="foregroundColor" />
    <Playlist :backgroundColor="backgroundColor" :foregroundColor="foregroundColor" />
    <Panel :borderColor="foregroundColor" />
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
       backgroundColor: '#EEF0F3',
       foregroundColor: '#5A5E65'
     }
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
