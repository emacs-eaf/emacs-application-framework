<template>
  <div
    ref="box"
    class="visual">
    <av-circle
      ref-link="player"
      playtime-font="30px monospace"
      :key="dynamicKey"
      :canv-width="visualCanvasWidth"
      :canv-height="visualCanvasWidth"
      :bar-color="barColor"
      :progress-color="barColor"
      :playtime-color="barColor"
      :outline-width="0"
      :progress-width="5"
      :outline-meter-space="5"
      :playtime="true"
    ></av-circle>
    <audio
      ref="player"
      class="audio"
      :key="dynamicKey"
      autoplay
      controls>
      <source :src="file">
    </audio>
  </div>
</template>

<script>
 export default {
   name: 'PreviewAudio',
   props: {
     file: String,
     barColor: String,
   },
   watch: {
     file: function() {
       this.dynamicKey = Math.random(10000);
     }
   },
   data() {
     return {
       dynamicKey: "",
       visualCanvasWidth: 0
     }
   },
   mounted() {
   },
   created() {
     this.visualCanvasWidth = window.innerWidth / 3;
   },
   methods: {
   }
 }
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
 .visual {
   display: flex;
   flex-direction: column;
   align-items: center;
 }

 .audio {
   width: 100%;
   border: none;
   outline: none;
   display: none;
 }
</style>
