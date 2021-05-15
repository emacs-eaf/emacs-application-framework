<template>
  <div id="app">
    <Panel class="panel"/>
    <Processlist class="processlist"/>
  </div>
</template>

<script>
 import Panel from './components/Panel.vue'
 import Processlist from './components/Processlist.vue'

 export default {
   name: 'App',
   components: {
     Processlist,
     Panel
   },
   data() {
     return {
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
   flex-direction: row;
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

 .panel {
   width: 20%;
 }

 .processlist {
   overflow: scroll;
 }
</style>
