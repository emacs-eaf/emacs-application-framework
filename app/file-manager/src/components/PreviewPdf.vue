<template>
  <pdf
    :src="file"
    :key="dynamicKey"/>
</template>

<script>
 import pdf from 'pdfvuer'

 export default {
   name: 'PreviewPdf',
   components: {
     pdf,
   },
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
</style>
