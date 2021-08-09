<template>
  <div
    id="panel"
    :style="{ 'border-top': '2px solid ' + borderColor }">
    <div id="song-info">
      <h1 v-if="song">
        {{ song }}
        <span :style="{ 'color': borderColor }">{{ artist }}</span>
      </h1>
    </div>
    <div id="control">
      <icon
        id="play-prev-icon"
        :style="{ 'color': borderColor }"
        name="step-backward"
        scale="6"
        @click.native="playPrev"></icon>
      <icon
        id="play-pause-icon"
        :style="{ 'color': borderColor }"
        :name="playIcon"
        scale="8"
        @click.native="pauseOrContinue"></icon>
      <icon
        id="play-next-icon"
        :style="{ 'color': borderColor }"
        name="step-forward"
        scale="6"
        @click.native="playNext"></icon>
      <icon
        id="repeat-icon"
        :style="{ 'color': borderColor }"
        :name="repeatMode"
        scale="4"
        @click.native="changeRepeatMode"></icon>
    </div>
  </div>
</template>

<script>
 export default {
   name: 'Panel',
   data() {
     return {
       playIcon: 'play-circle',
       song: '',
       artist: '',
       repeatMode: 'song'
     }
   },

   props: {
     borderColor: String
   },

   mounted() {
     window.setRepeatMode = this.setRepeatMode;
     window.setPanelSongInfo = this.setPanelSongInfo;
     window.setPlayIconStatus = this.setPlayIconStatus;
     window.playNext = this.playNext;
     window.playPrev = this.playPrev;
   },

   methods: {
     changeRepeatMode() {
       window.pyobject.eval_emacs_function("netease-cloud-music-change-repeat-mode", [])
     },

     playPrev() {
       window.pyobject.eval_emacs_function("netease-cloud-music-play-previous-song", [])
     },

     playNext() {
       window.pyobject.eval_emacs_function("netease-cloud-music-play-next-song", [])
     },

     setRepeatMode(mode) {
       if (mode != '') {
         this.repeatMode = mode;
       }
     },

     setPanelSongInfo(info) {
       this.song = info[0];
       this.artist = info[1];
     },

     pauseOrContinue() {
       /* NOTE: You should check eaf-netease-cloud-music-play-status at Elisp side */
       window.pyobject.eval_emacs_function("netease-cloud-music-pause-or-continue", [])
     },

     setPlayIconStatus(status) {
       this.playIcon = status;
     }
   }
 }
</script>

<style scoped>
 #panel {
   position: fixed;
   width: 100%;
   height: 18%;
   bottom: 0;
 }

 #song-info {
   position: absolute;
   width: 20%;
   height: 100%;
   left: 0;
   display: flex;
 }

 #song-info > h1 {
   margin: auto;
 }

 #song-info > h1 > span {
   font-size: 23px;
   display: block;
   font-weight: normal;
 }

 #control {
   position: absolute;
   width: 100%;
   height: 100%;
   bottom: 0;
   display: flex;
   flex-direction: row;
   align-items: center;
   justify-content: center;
 }

 #control > icon {
   cursor: pointer;
 }

 #repeat-icon {
   margin-left: 10px;
 }

 #play-pause-icon {
   margin-left: 10px;
   margin-right: 10px;
 }
</style>
