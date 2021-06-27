<template>
  <div
    class="panel"
    :style="{ 'background': backgroundColor }">
    <img
      v-if="currentCover"
      class="cover"
      :src="currentCover"/>
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
    <div
      class="control"
      :style="{ 'color': foregroundColor }">
      <icon
        class="repeat"
        :style="{ 'color': foregroundColor }"
        :name="playOrder"
        :scale="2.5"
        @click.native="togglePlayOrder"
      ></icon>
      <icon
        class="backward"
        :style="{ 'color': foregroundColor }"
        name="step-backward"
        :scale="4"
        @click.native="playPrevItem"
      ></icon>
      <icon
        class="play"
        :style="{ 'color': foregroundColor }"
        :name="playIcon"
        :scale="6"
        @click.native="toggle"
      ></icon>
      <icon
        class="forward"
        :style="{ 'color': foregroundColor }"
        name="step-forward"
        :scale="4"
        @click.native="playNextItem"
      ></icon>
      <div class="current-time">
        {{ currentTime }}
      </div>
      /
      <div class="duration">
        {{ duration }}
      </div>
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
 import { mapState } from "vuex";
 import albumArt from "album-art"

 export default {
   name: 'Panel',
   data() {
     return {
       currentTime: "",
       currentCover: "",
       duration: "",
       name: "",
       artist: "",
       backgroundColor: "",
       foregroundColor: "",
       /* Download icon from https://www.iconfont.cn/collections/detail?spm=a313x.7781069.0.da5a778a4&cid=18739 */
       playIcon: "play-circle",
       playOrder: "list"
     }
   },
   computed: mapState([
     "currentTrack",
     "currentTrackIndex",
     "fileInfos"
   ]),
   watch: {
     "fileInfos": function() {
       this.playItem(this.fileInfos[0]);
     }
   },
   props: {
   },
   mounted() {
     window.initPanelColor = this.initPanelColor;
     window.initPlayOrder = this.initPlayOrder;
     window.forward = this.forward;
     window.backward = this.backward;
     window.toggle = this.toggle;
     window.playNextItem = this.playNextItem;
     window.playPrevItem = this.playPrevItem;
     window.togglePlayOrder = this.togglePlayOrder;

     let that = this;

     this.$root.$on("playItem", this.playItem);

     this.$refs.player.addEventListener("ended", this.handlePlayFinish);

     this.$refs.player.addEventListener('timeupdate', () => {
       that.currentTime = that.formatTime(that.$refs.player.currentTime);
       that.duration = that.formatTime(that.$refs.player.duration);
     });
   },
   methods: {
     playItem(item) {
       this.$store.commit("updateCurrentTrack", item.path);

       this.playIcon = "pause-circle";

       this.name = item.name;
       this.artist = item.artist;

       this.$refs.player.load();
       this.$refs.player.play();

       albumArt(item.artist, (error, response) => {
         if (error) {
           this.currentCover = "";
         } else {
           this.currentCover = response;
         }
       })
     },

     togglePlayOrder() {
       if (this.playOrder === "list") {
         this.playOrder = "random";
       } else if (this.playOrder === "random") {
         this.playOrder = "repeat";
       } else if (this.playOrder === "repeat") {
         this.playOrder = "list";
       }
     },

     handlePlayFinish() {
       if (this.playOrder === "list") {
         this.playNextItem();
       } else if (this.playOrder === "random") {
         this.playRandom();
       } else if (this.playOrder === "repeat") {
         this.playAgain();
       }
     },

     initPanelColor(backgroundColor, foregroundColor) {
       this.backgroundColor = backgroundColor;
       this.foregroundColor = foregroundColor;
     },

     initPlayOrder(playOrder) {
       this.playOrder = playOrder;
     },

     formatTime(seconds) {
       if (isNaN(seconds)) {
         return "00:00"
       } else {
         var minutes = Math.floor(seconds / 60);
         minutes = (minutes >= 10) ? minutes : "0" + minutes;
         seconds = Math.floor(seconds % 60);
         seconds = (seconds >= 10) ? seconds : "0" + seconds;
         return minutes + ":" + seconds;
       }
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
       var currentTrackIndex = this.currentTrackIndex;

       if (currentTrackIndex > 0) {
         currentTrackIndex -= 1;
       } else {
         currentTrackIndex = this.fileInfos.length - 1;
       }

       this.playItem(this.fileInfos[currentTrackIndex]);
     },

     playNextItem() {
       var currentTrackIndex = this.currentTrackIndex;

       if (currentTrackIndex < this.fileInfos.length - 1) {
         currentTrackIndex += 1;
       } else {
         currentTrackIndex = 0;
       }

       this.playItem(this.fileInfos[currentTrackIndex]);
     },

     playRandom() {
       var min = 0;
       var max = this.fileInfos.length;
       var randomIndex = Math.floor(Math.random() * (max - min + 1)) + min;

       this.playItem(this.fileInfos[randomIndex]);
     },

     playAgain() {
       this.playItem(this.fileInfos[this.currentTrackIndex]);
     }
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

 .cover {
   height: 80%;
   margin-left: 30px;
 }

 .info {
   width: 30%;
   user-select: none;
   padding-left: 30px;

   overflow: hidden;
   white-space: nowrap;
   text-overflow: ellipsis;
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
   cursor: pointer;
 }

 .forward {
   cursor: pointer;
 }

 .play {
   margin-left: 10px;
   margin-right: 10px;
   cursor: pointer;
 }

 .play-order {
   margin-right: 20px;
 }

 .repeat {
   margin-right: 20px;
 }

 .current-time {
   margin-left: 20px;
   margin-right: 5px;
 }

 .duration {
   margin-left: 5px;
 }
</style>
