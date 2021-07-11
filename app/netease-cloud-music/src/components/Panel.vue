<template>
  <div
    id="panel"
    :style="{ 'border-bottom': '2px solid ' + borderColor, 'border-right': '2px solid ' + borderColor }">
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
        @click="playNextOrPrevSong(true)"></icon>
      <icon
        id="play-pause-icon"
        :style="{ 'color': borderColor }"
        :name="playIcon"
        scale="8"
        @click="window.pyobject.play_or_pause()"></icon>
      <icon
        id="play-next-icon"
        :style="{ 'color': borderColor }"
        name="step-forward"
        scale="6"
        @click="playNextOrPrevSong(false)"></icon>
      <icon
        id="repeat-icon"
        :style="{ 'color': borderColor }"
        :name="repeatMode"
        scale="4"
        @click="changeRepeatMode()"></icon>
    </div>
  </div>
</template>

<script>
 export default {
   name: 'Panel',
   data() {
     return {
       playIcon: 'play-circle',
       song: 'Lemon',
       artist: 'Kenshi Yorezi',
       repeatMode: 'repeat'
     }
   },

   props: {
     borderColor: String
   },

   mounted() {
     window.setRepeatMode = this.setRepeatMode;
   },

   methods: {
     changeRepeatMode() {
       window.pyobject.changeRepeatMode();
     },

     setRepeatMode(mode) {
       this.repeatMode = mode;
     }
   }
 }
</script>

<style scoped>
 #panel {
   position: absolute;
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
