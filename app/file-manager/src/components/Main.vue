<template>
  <div class="page">
    <div
      class="current-path"
      :style="{ 'color': headerForegroundColor() }">
      {{ path }}
    </div>
    <div
      ref="filelist"
      class="file-list">
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
  </div>
</template>

<script>
 import { QWebChannel } from "qwebchannel";

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
     window.changePath = this.changePath;
     window.initColors = this.initColors;
     window.selectNextFile = this.selectNextFile;
     window.selectPrevFile = this.selectPrevFile;
     window.openFile = this.openFile;
     window.upDirectory = this.upDirectory;
   },
   created() {
     // eslint-disable-next-line no-undef
     new QWebChannel(qt.webChannelTransport, channel => {
       window.pyobject = channel.objects.pyobject;
     });
   },
   methods: {
     changePath(path, files, index) {
       this.path = path;
       this.files = files;
       this.currentIndex = index;
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

       this.$refs.filelist.children[this.currentIndex].scrollIntoViewIfNeeded(false);
     },

     selectPrevFile() {
       if (this.currentIndex > 0) {
         this.currentIndex -= 1;
       }

       this.currentPath = this.files[this.currentIndex].path;

       this.$refs.filelist.children[this.currentIndex].scrollIntoViewIfNeeded(false);
     },

     openFile() {
       var currentFile = this.files[this.currentIndex];

       if (currentFile.type == "directory") {
         window.pyobject.change_directory(currentFile.path, "");
       } else if (currentFile.type == "file") {
         window.pyobject.open_file(currentFile.path);
       }
     },

     upDirectory() {
       window.pyobject.change_up_directory(this.currentPath);
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
