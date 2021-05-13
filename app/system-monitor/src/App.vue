<template>
  <div id="app">
    <Processlist class="processlist"/>
  </div>
</template>

<script>
 import Processlist from './components/Processlist.vue'

 export default {
   name: 'App',
   components: {
     Processlist,
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

 .processlist {
   overflow: scroll;
 }
</style>
