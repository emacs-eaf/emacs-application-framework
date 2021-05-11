<template>
  <div
    class="panel"
    :style="{ 'background': backgroundColor }">
    <div
      class="info"
      :style="{ 'color': foregroundColor }">
      <div>
        {{ name }}
      </div>
      <div>
        {{ artist }}
      </div>
    </div>
    <div class="control">
      <font-awesome-icon
        class="backward"
        :style="{ 'color': foregroundColor }"
        icon="step-backward"
        @click="playPrevItem"
      />
      <font-awesome-icon
        class="play"
        :style="{ 'color': foregroundColor }"
        :icon="playIcon"
        @click="toggle"
      />
      <font-awesome-icon
        class="forward"
        :style="{ 'color': foregroundColor }"
        icon="step-forward"
        @click="playNextItem"
      />
    </div>
    <div class="visual">
      <audio ref="player">
        <source :src="currentTrack">
      </audio>
      <av-bars
        class="visual-bar"
        ref-link="player"
        caps-color="#FFF"
        :bar-color="['#f00', '#ff0', '#0f0']"
        :caps-height="2"
      />
    </div>
  </div>
</template>

<script>
 export default {
   name: 'Panel',
   data() {
     return {
       currentTrack: "",
       fileInfos: [],
       name: "",
       artist: "",
       backgroundColor: "",
       foregroundColor: "",
       playIcon: "play-circle"
     }
   },
   computed: {
   },
   props: {
   },
   mounted() {
     window.initPanelColor = this.initPanelColor;
     window.forward = this.forward;
     window.backward = this.backward;
     window.toggle = this.toggle;
     window.playNextItem = this.playNextItem;
     window.playPrevItem = this.playPrevItem;

     this.$root.$on("playItem", this.playItem);

     this.$root.$on("addFiles", files => {
       this.fileInfos = files;
       this.playItem(files[0]);
     });

     let that = this;
     this.$refs.player.addEventListener("ended", function(){
       that.playNextItem();
     });
   },
   methods: {
     playItem(item) {
       this.currentTrack = item.path;
       this.playIcon = "pause-circle";

       this.$root.$emit("changeCurrentTrack", this.currentTrack);

       this.name = item.name;
       this.artist = item.artist;

       this.$refs.player.load();
       this.$refs.player.play();
     },

     initPanelColor(backgroundColor, foregroundColor) {
       this.backgroundColor = backgroundColor;
       this.foregroundColor = foregroundColor;
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
         this.playIcon = "pause-circle";
       } else {
         this.$refs.player.pause();
         this.playIcon = "play-circle";
       }
     },

     playPrevItem() {
       var tracks = this.fileInfos.map(function (track) { return track.path });
       var currentTrackIndex = tracks.indexOf(this.currentTrack);

       if (currentTrackIndex > 0) {
         currentTrackIndex -= 1;
       } else {
         currentTrackIndex = tracks.length - 1;
       }

       this.playItem(this.fileInfos[currentTrackIndex]);
     },

     playNextItem() {
       var tracks = this.fileInfos.map(function (track) { return track.path });
       var currentTrackIndex = tracks.indexOf(this.currentTrack);

       if (currentTrackIndex < tracks.length - 1) {
         currentTrackIndex += 1;
       } else {
         currentTrackIndex = 0;
       }

       this.playItem(this.fileInfos[currentTrackIndex]);
     },
   }
 }
</script>

<style scoped>
 .panel {
   position: absolute;
   bottom: 0;

   width: 100%;

   box-shadow: 0px -4px 3px rgba(30, 30, 30, 0.75);

   display: flex;
   flex-direction: row;
   align-items: center;
 }

 .info {
   width: 30%;
   user-select: none;
   padding-left: 30px;
 }

 .control {
   display: flex;
   flex-direction: row;
   align-items: center;
   width: 40%;
   justify-content: center;
 }

 .visual {
   width: 30%;
   padding-right: 30px;
 }

 .visual-bar {
   display: flex;
   justify-content: flex-end;
 }

 .backward {
   font-size: 24px;
   cursor: pointer;
 }

 .forward {
   font-size: 24px;
   cursor: pointer;
 }

 .play {
   font-size: 40px;
   margin-left: 10px;
   margin-right: 10px;
   cursor: pointer;
 }
</style>
