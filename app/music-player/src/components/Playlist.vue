<template>
  <div class="playlist">
    <div
      class="item"
      v-for="item in fileInfos"
      @click="playItem(item)"
      :key="item.path"
      :style="{ color: foregroundColor }">
      {{ item.name }}
    </div>
    <audio ref="player">
      <source :src="currentTrack">
    </audio>
  </div>
</template>

<script>
 export default {
   name: 'Playlist',
   data() {
     return {
       fileInfos: [],
       currentTrack: "",
       backgroundColor: "",
       foregroundColor: ""
     }
   },
   props: {
   },
   mounted() {
     window.initColors = this.initColors;
     window.addFiles = this.addFiles;
   },
   methods: {
     initColors(backgroundColor, foregroundColor) {
       this.backgroundColor = backgroundColor;
       this.foregroundColor = foregroundColor;
     },
     addFiles(files) {
       for (var i = 0; i < files.length; i++) {
         this.fileInfos.push({
           name: files[i].split(/(\\|\/)/g).pop().split('.').slice(0, -1).join('.'),
           path: files[i]
         })
       }

       this.currentTrack = files[0];

       this.$refs.player.load();
       this.$refs.player.play();
     },
     playItem(item) {
       this.currentTrack = item.path;
       this.$refs.player.load();
       this.$refs.player.play();
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
   padding-left: 10px;
   padding-right: 10px;
   padding-top: 5px;
   padding-bottom: 5px;
 }
</style>
