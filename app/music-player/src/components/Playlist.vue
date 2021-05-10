<template>
  <div class="playlist">
    <div
      class="item"
      v-for="(item, index) in fileInfos"
      @click="playItem(item)"
      :key="item.path"
      :style="{ 'background': itemBackgroundColor(item), 'color': itemForegroundColor(item) }">
      <div class="item-index">
        {{ padNumber(index, numberWidth) }}
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
       foregroundColor: "",
     }
   },
   computed: {

   },
   props: {
   },
   mounted() {
     window.initColors = this.initColors;
     window.addFiles = this.addFiles;
     window.playNextItem = this.playNextItem;
     window.playPrevItem = this.playPrevItem;
     window.forward = this.forward;
     window.backward = this.backward;
     window.toggle = this.toggle;

     let that = this;
     this.$refs.player.addEventListener("ended", function(){
       that.playNextItem();

       console.log("Got it");
     });
   },
   methods: {
     initColors(backgroundColor, foregroundColor) {
       this.backgroundColor = backgroundColor;
       this.foregroundColor = foregroundColor;
     },

     addFiles(files) {
       this.fileInfos = files;

       this.numberWidth = files.length.toString().length;

       this.playFile(files[0].path);
     },

     playItem(item) {
       this.playFile(item.path);
     },

     playPrevItem() {
       var tracks = this.fileInfos.map(function (track) { return track.path });
       var currentTrackIndex = tracks.indexOf(this.currentTrack);

       if (currentTrackIndex > 0) {
         currentTrackIndex -= 1;
       } else {
         currentTrackIndex = tracks.length - 1;
       }

       this.playFile(tracks[currentTrackIndex]);
     },

     playNextItem() {
       var tracks = this.fileInfos.map(function (track) { return track.path });
       var currentTrackIndex = tracks.indexOf(this.currentTrack);

       if (currentTrackIndex < tracks.length - 1) {
         currentTrackIndex += 1;
       } else {
         currentTrackIndex = 0;
       }

       this.playFile(tracks[currentTrackIndex]);
     },

     playFile(file) {
       this.currentTrack = file;
       this.$refs.player.load();
       this.$refs.player.play();
     },

     forward() {
       this.$refs.player.currentTime += 10;
     },

     backward() {
       this.$refs.player.currentTime -= 10;
     },

     toggle() {
       if (this.$refs.player.paused) {
         this.$refs.player.play();
       } else {
         this.$refs.player.pause()
       }
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
