<template>
  <div
    class="panel"
    :style="{ 'background': backgroundColor }">
    <div :style="{ 'color': foregroundColor }">
      <vue-ellipse-progress
        class="memory-info"
        :progress="info.memory.percent"
        :dot="classObject"
        id="timer-example"
        :determinate="determinate"
        line="butt"
        :color="emptyColor"
        :empty-color="foregroundColor"
        :emptyColorFill="emptyColorFill"
        thickness="46"
        emptyThickness="8"
        :size="180"
        dash="strict 60 0.8"
        lineMode="in -8"
        :legend="false"
        legendClass="legend-custom-style"
        fontSize="1.5rem"
        font-color="white"
        :loading="false"
        :no-data="noData">
        <span slot="legend-caption">
          {{ info.memory.percent }} %
        </span>
      </vue-ellipse-progress>
      <div class="memory-number">
        {{ info.memory.used }} / {{ info.memory.total }}
      </div>
    </div>
  </div>
</template>

<script>
 export default {
   name: 'Panel',
   components: {
   },
   data() {
     return {
       info: {},
       backgroundColor: "",
       foregroundColor: "",
       progress: 0,
       emptyColor: {
         radial: true,
         colors: [],
       },
       emptyColorFill: {
         radial: true,
         colors: [],
       },
     }
   },
   computed: {
     classObject: function () {
       return {
         size: 46,
         backgroundColor: this.foregroundColor,
         width: '2px',
       }
     }
   },
   props: {
   },
   mounted() {
     window.initPanelColor = this.initPanelColor;
     window.updatePanelInfo = this.updatePanelInfo;
   },
   methods: {
     initPanelColor(backgroundColor, foregroundColor) {
       this.backgroundColor = backgroundColor;
       this.foregroundColor = foregroundColor;

       var emptyColors = [
         {
           color: this.foregroundColor,
           offset: "50",
           opacity: "0.15",
         },
         {
           color: this.foregroundColor,
           offset: "70",
           opacity: "0.15",
         },
         {
           color: this.foregroundColor,
           offset: "70",
           opacity: "0.1",
         },
         {
           color: this.foregroundColor,
           offset: "90",
           opacity: "1",
         },
         {
           color: this.foregroundColor,
           offset: "60",
           opacity: "1",
         },
         {
           color: this.foregroundColor,
           offset: "0",
           opacity: "0",
         },
       ]
       this.emptyColor.colors = emptyColors;

       var emptyFillColors = [
         {
           color: this.foregroundColor,
           offset: "50",
           opacity: "0.2",
         },
         {
           color: this.foregroundColor,
           offset: "50",
           opacity: "0.15",
         },
         {
           color: this.foregroundColor,
           offset: "70",
           opacity: "0.15",
         },
         {
           color: this.foregroundColor,
           offset: "70",
           opacity: "0.1",
         },
         {
           color: this.foregroundColor,
           offset: "90",
           opacity: "0.1",
         },
         {
           color: "transparent",
           offset: "90",
           opacity: "0.1",
         },
         {
           color: "transparent",
           offset: "95",
           opacity: "0.1",
         },
         {
           color: "transparent",
           offset: "95",
           opacity: "0.1",
         },
       ]
       this.emptyColorFill.colors = emptyFillColors;
     },

     updatePanelInfo(info) {
       this.info = info;
     }
   }
 }
</script>

<style scoped>
 .panel {
   height: 100%;

   display: flex;
   flex-direction: column;
   align-items: center;
 }

 .memory-info {
   margin-top: 30px;
 }

 .memory-number {
   display: flex;
   flex-direction: column;
   align-items: center;
   margin-top: 10px;
   margin-bottom: 30px;
 }
</style>
