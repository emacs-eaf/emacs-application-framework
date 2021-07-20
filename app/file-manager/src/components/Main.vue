<template>
  <div class="page">
    <div class="current-path">
      {{ path }}
    </div>
    <div class="file-list">
      <div
        class="item"
        v-for="file in files"
        :key="file.path">
        {{ file.name }}
        <div v-if="file.type == 'directory'">
          /
        </div>
      </div>
    </div>
  </div>
</template>

<script>
 export default {
   name: 'Main',
   props: {
     msg: String
   },
   data() {
     return {
       path: "",
       files: []
     }
   },
   mounted() {
     window.addPath = this.addPath;
     window.addFiles = this.addFiles;
   },
   methods: {
     addPath(path) {
       this.path = path;
     },

     addFiles(files) {
       this.files = files;
     }
   }
 }
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
 .page {
   width: 100%;
   height: 100%;
   display: flex;
   flex-direction: column;
 }

 .current-path {
   font-size: 20px;
   padding-left: 20px;
   padding-bottom: 5px;
   color: gold;
 }

 .file-list {
   width: 100%;
   overflow: scroll;
 }

 .item {
   font-size: 18px;
   padding-left: 20px;
   padding-top: 5px;
   padding-bottom: 5px;

   display: flex;
   flex-direction: row;
 }
</style>
