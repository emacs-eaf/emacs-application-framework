<template>
  <div class="box">
    <div
      class="file"
      v-for="file in files"
      :key="file.path"
      :style="{ 'background': itemBackgroundColor(file), 'color': itemForegroundColor(file) }">
      <div class="file-name">
        {{ file.name }}
      </div>
      <div class="file-size">
        {{ file.size }}
      </div>
    </div>
  </div>
</template>

<script>
 export default {
   name: 'PreviewDirectory',
   props: {
     files: Array,
     itemBackgroundColor: Function,
     itemForegroundColor: Function,
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
 .box {
   width: 100%;
   height: 100%;
   overflow: hidden;
 }

 .file {
   font-size: 16px;
   padding-left: 20px;
   padding-top: 2px;
   padding-bottom: 2px;

   display: flex;
   flex-direction: row;
 }

 .file-name {
   flex: 1;
 }

 .file-size {
   padding-right: 20px;
 }
</style>
