<template>
  <div class="page">
    <div
      class="current-path"
      :style="{ 'color': headerForegroundColor() }">
      {{ path }}
    </div>
    <div class="file-list">
      <div
        class="file"
        v-for="file in files"
        :key="file.path"
        :style="{ 'background': itemBackgroundColor(file), 'color': itemForegroundColor(file) }">
        {{ file.name }}
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
       files: [],
       currentIndex: 0,
       currentPath: "",
       backgroundColor: "",
       foregroundColor: "",
       headerColor: "",
       fileColor: "",
       directoryColor: "",
       symlinkColor: "",
       selectColor: "",
     }
   },
   mounted() {
     window.addPath = this.addPath;
     window.addFiles = this.addFiles;
     window.initColors = this.initColors;
     window.selectNextFile = this.selectNextFile;
     window.selectPrevFile = this.selectPrevFile;
   },
   methods: {
     addPath(path) {
       this.path = path;
     },

     addFiles(files) {
       this.files = files;
       this.currentIndex = 0;
       this.currentPath = files[this.currentIndex].path;
     },

     initColors(backgroundColor, foregroundColor, headerColor, fileColor, directoryColor, symlinkColor, selectColor) {
       this.backgroundColor = backgroundColor;
       this.foregroundColor = foregroundColor;
       this.headerColor = headerColor;
       this.fileColor = fileColor;
       this.directoryColor = directoryColor;
       this.symlinkColor = symlinkColor;
       this.selectColor = selectColor;
     },

     itemBackgroundColor(item) {
       if (item.path == this.currentPath) {
         return this.selectColor;
       } else {
         return this.backgroundColor;
       }
     },

     itemForegroundColor(item) {
       if (item.type == "directory") {
         return this.directoryColor;
       } else if (item.type == "file") {
         return this.fileColor;
       } else if (item.type == "symlink") {
         return this.symlinkColor;
       }
     },

     headerForegroundColor() {
       return this.headerColor;
     },

     selectNextFile() {
       if (this.currentIndex < this.files.length - 1) {
         this.currentIndex += 1;
       }

       this.currentPath = this.files[this.currentIndex].path;
     },

     selectPrevFile() {
       if (this.currentIndex > 0) {
         this.currentIndex -= 1;
       }

       this.currentPath = this.files[this.currentIndex].path;
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
   font-size: 18px;
   padding-left: 20px;
   padding-bottom: 5px;
 }

 .file-list {
   width: 100%;
   overflow: scroll;
 }

 .file {
   font-size: 16px;
   padding-left: 20px;
   padding-top: 2px;
   padding-bottom: 2px;

   display: flex;
   flex-direction: row;
 }
</style>
