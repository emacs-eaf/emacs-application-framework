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
        icon="backward" />
      <font-awesome-icon
        class="play"
        :style="{ 'color': foregroundColor }"
        icon="play-circle" />
      <font-awesome-icon
        class="forward"
        :style="{ 'color': foregroundColor }"
        icon="forward" />
    </div>
    <div class="visual">
    </div>
  </div>
</template>

<script>
 export default {
   name: 'Panel',
   data() {
     return {
       name: "",
       artist: "",
       backgroundColor: "",
       foregroundColor: ""
     }
   },
   computed: {
   },
   props: {
   },
   mounted() {
     window.initPanelColor = this.initPanelColor;

     this.$root.$on("playItem", item => {
       this.name = item.name;
       this.artist = item.artist;
     });
   },
   methods: {
     initPanelColor(backgroundColor, foregroundColor) {
       this.backgroundColor = backgroundColor;
       this.foregroundColor = foregroundColor;
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

   padding-left: 60px;
 }

 .info {
   width: 30%;
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
 }

 .backward {
   font-size: 24px;
 }

 .forward {
   font-size: 24px;
 }

 .play {
   font-size: 40px;
   margin-left: 10px;
   margin-right: 10px;
 }
</style>
