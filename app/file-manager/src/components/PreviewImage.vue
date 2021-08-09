<template>
  <img
    class="image"
    :src="file"/>
</template>

<script>
 export default {
   name: 'PreviewImage',
   components: {
   },
   props: {
     file: String
   },
   watch: {
   },
   data() {
     return {
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
 .image {
   width: 100%;
 }
</style>
