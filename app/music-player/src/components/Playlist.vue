<template>
  <div class="playlist">
    <div
      class="item"
      v-for="(item, index) in fileInfos"
      @click="playItem(item)"
      :key="item.path"
      :style="{ 'background': itemBackgroundColor(item), 'color': itemForegroundColor(item) }">
      <div class="item-index">
        {{ padNumber(index + 1, numberWidth) }}
      </div>
      <div class="item-name">
        {{ item.name }}
      </div>
      <div class="item-artist">
        {{ item.artist }}
      </div>
      <div class="item-album">
        {{ item.album }}
      </div>
    </div>
  </div>
</template>

<script>
 export default {
   name: 'Playlist',
   data() {
     return {
       fileInfos: [],
       currentTrack: "",
       numberWidth: 0,
       backgroundColor: "",
       foregroundColor: "",
     }
   },
   computed: {

   },
   props: {
   },
   mounted() {
     window.initPlaylistColor = this.initPlaylistColor;
     window.addFiles = this.addFiles;

     this.$root.$on("changeCurrentTrack", currentTrack => {
       this.currentTrack = currentTrack;
     });
   },
   methods: {
     initPlaylistColor(backgroundColor, foregroundColor) {
       this.backgroundColor = backgroundColor;
       this.foregroundColor = foregroundColor;
     },

     addFiles(files) {
       this.fileInfos = files;
       this.$root.$emit("addFiles", files);

       this.numberWidth = files.length.toString().length;
     },

     playItem(item) {
       this.$root.$emit("playItem", item);
     },

     padNumber(num, size) {
       var s = num+"";
       while (s.length < size) s = "0" + s;

       return s;
     },

     itemBackgroundColor(item) {
       if (item.path == this.currentTrack) {
         return this.foregroundColor;
       } else {
         return this.backgroundColor;
       }
     },

     itemForegroundColor(item) {
       if (item.path == this.currentTrack) {
         return this.backgroundColor;
       } else {
         return this.foregroundColor;
       }
     }
   }
 }
</script>

<style scoped>
 .playlist {
   width: 100%;
   height: 100%;
 }

 .item {
   padding-left: 20px;
   padding-right: 20px;
   padding-top: 5px;
   padding-bottom: 5px;

   display: flex;
   flex-direction: row;
   align-items: center;

   user-select: none;
 }

 .item-index {
   margin-right: 10px;
 }

 .item-name {
   margin-right: 10px;
   width: 40%;
 }

 .item-artist {
   margin-left: 30px;
   width: 20%;
 }

 .item-album {
   width: 30%;
 }
</style>
