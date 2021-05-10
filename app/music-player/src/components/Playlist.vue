<template>
  <div class="playlist">
    <div
      class="item"
      v-for="(item, index) in fileInfos"
      @click="playItem(item)"
      :key="item.path"
      :style="{ color: foregroundColor }">
      <div class="item-index">
        {{ padNumber(index, numberWidth) }}
      </div>
      <div class="item-name">
        {{ item.name }}
      </div>
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
       numberWidth: 0,
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

       this.numberWidth = files.length.toString().length;

       this.currentTrack = files[0];

       this.$refs.player.load();
       this.$refs.player.play();
     },
     playItem(item) {
       this.currentTrack = item.path;
       this.$refs.player.load();
       this.$refs.player.play();
     },
     padNumber(num, size) {
       var s = num+"";
       while (s.length < size) s = "0" + s;

       return s;
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
 }

 .item-index {
   margin-right: 10px;
 }

 .item-name {

 }
</style>
