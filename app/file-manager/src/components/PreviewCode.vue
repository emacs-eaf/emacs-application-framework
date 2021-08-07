<template>
  <pre class="code">
    {{ content }}
  </pre>
</template>

<script>
 export default {
   name: 'PreviewCode',
   components: {
   },
   props: {
     content: String
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
 .code {
   padding-left: 20px;
   padding-right: 20px;
   margin: 0;
 }
</style>
