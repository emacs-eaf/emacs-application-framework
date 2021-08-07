<template>
  <video
    class="video"
    :key="dynamicKey"
    controls>
    <source :src="file">
  </video>
</template>

<script>
 export default {
   name: 'PreviewVideo',
   props: {
     file: String
   },
   watch: {
     file: function() {
       this.dynamicKey = Math.random(10000);
     }
   },
   data() {
     return {
       dynamicKey: ""
     }
   },
   mounted() {
   },
   created() {
   },
   methods: {
   }
 }
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
 .video {
   width: 100%;
   border: none;
   outline: none;
 }
</style>
