<template>
  <div class="page">
    <div
      class="current-path"
      :style="{ 'color': headerForegroundColor() }">
      {{ path }}
    </div>
    <div class="content">
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
      <div class="preview">
        <div v-if="previewType == 'file'">
        </div>
        <div
          v-if="previewType == 'directory' && previewFiles.length > 0"
          class="preview-file-list">
          <div
            class="file"
            v-for="file in previewFiles"
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
        <div
          v-if="previewType == 'directory'"
          class="preview-empty-directory">
          <div class="empty-directory-info-warp">
            <div class="empty-directory-info">
              Empty directory.
            </div>
          </div>
        </div>
        <div v-if="previewType == 'symlink'">
        </div>
      </div>
    </div>
  </div>
</template>

<script>
 import { QWebChannel } from "qwebchannel";

 export default {
   name: 'Main',
   components: {
   },
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

       previewLastPath: "",
       previewPath: "",
       previewType: "",
       previewFiles: [],
     }
   },
   mounted() {
     window.changePath = this.changePath;
     window.initColors = this.initColors;
     window.selectNextFile = this.selectNextFile;
     window.selectPrevFile = this.selectPrevFile;
     window.openFile = this.openFile;
     window.upDirectory = this.upDirectory;
     window.setPreview = this.setPreview;
     window.tryUpdatePreview = this.tryUpdatePreview;
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

       this.keepSelectVisible();

       this.updatePreview();
     },

     selectPrevFile() {
       if (this.currentIndex > 0) {
         this.currentIndex -= 1;
       }

       this.currentPath = this.files[this.currentIndex].path;

       this.keepSelectVisible();

       this.updatePreview();
     },

     keepSelectVisible() {
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
     },

     updatePreview() {
       this.previewLastPath = this.files[this.currentIndex].path;
     },

     tryUpdatePreview() {
       console.log("*****", this.previewLastPath);

       if (this.previewLastPath != "") {
         window.pyobject.update_preview(this.previewLastPath);
         this.previewLastPath = "";
       }
     },

     setPreview(filePath, fileType, fileInfos) {
       this.previewPath = filePath;
       this.previewType = fileType;
       this.previewFiles = fileInfos;
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
   width: 50%;
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

 .preview {
   width: 50%;
   height: 100%;
   overflow: hidden;

   border-left: 1px solid;
 }

 .content {
   width: 100%;
   height: 100%;
   overflow: hidden;

   display: flex;
   flex-direction: row;
 }

 .preview-file-list {
   width: 100%;
   height: 100%;
   overflow: hidden;
 }

 .preview-empty-directory {
   width: 100%;
   height: 100%;
   overflow: hidden;
 }

 .empty-directory-info-warp {
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
