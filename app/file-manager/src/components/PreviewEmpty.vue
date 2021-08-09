<template>
  <div class="empty-directory">
    <div class="empty-directory-info">
      Empty directory.
    </div>
  </div>
</template>

<script>
 export default {
   name: 'PreviewEmpty',
   props: {
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
 .empty-directory {
   width: 100%;
   height: 100%;
   display: flex;
   flex-direction: column;
   justify-content: center;
 }

 .empty-directory-info {
   text-align: center;
 }
</style>
