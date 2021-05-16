<template>
  <div
    class="panel"
    :style="{ 'background': backgroundColor }">
    <div :style="{ 'color': foregroundColor }">
      <vue-ellipse-progress
        class="memory-info"
        :progress="info.memory.percent"
        :dot="memoryDotClass"
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

    <div>
      <apexchart
        ref="chart"
        width="100%"
        type="line"
        :options="chartOptions">
      </apexchart>
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

       chartOptions: {
         chart: {
           id: 'realtime',
           toolbar: {
             show: false
           },
           animations: {
             enabled: true,
             easing: 'linear',
             speed: 1000,
             animateGradually: {
               enabled: false,
               delay: 150
             },
             dynamicAnimation: {
               enabled: false,
               speed: 350
             }
           }
         },
         dataLabels: {
           enabled: false
         },
         stroke: {
           curve: 'smooth',
           width: 2
         },

         yaxis: {
           show: false,
           showAlways: true,
           showForNullSeries: true,
           seriesName: undefined,
           opposite: false,
           reversed: false,
           logarithmic: false,
           tickAmount: 6,
           min: 0,
           max: 100,
           forceNiceScale: false,
           floating: false,
           decimalsInFloat: undefined,
           labels: {
             show: false,
             align: 'right',
             minWidth: 0,
             maxWidth: 160,
             style: {
               colors: [],
               fontSize: '12px',
               fontFamily: 'Helvetica, Arial, sans-serif',
               fontWeight: 400,
               cssClass: 'apexcharts-yaxis-label',
             },
             offsetX: 0,
             offsetY: 0,
             rotate: 0,
             formatter: (value) => { return value },
           },
           axisBorder: {
             show: false,
             color: '#78909C',
             offsetX: 0,
             offsetY: 0
           },
           axisTicks: {
             show: false,
             borderType: 'solid',
             color: '#78909C',
             width: 6,
             offsetX: 0,
             offsetY: 0
           },
           title: {
             text: undefined,
             rotate: -90,
             offsetX: 0,
             offsetY: 0,
             style: {
               color: undefined,
               fontSize: '12px',
               fontFamily: 'Helvetica, Arial, sans-serif',
               fontWeight: 600,
               cssClass: 'apexcharts-yaxis-title',
             },
           },
           crosshairs: {
             show: false,
             position: 'back',
             stroke: {
               color: '#b6b6b6',
               width: 1,
               dashArray: 0,
             },
           },
           tooltip: {
             enabled: false,
             offsetX: 0,
           },

         },
         xaxis: {
           type: 'category',
           categories: [],
           labels: {
             show: false,
             rotate: -45,
             rotateAlways: false,
             hideOverlappingLabels: true,
             showDuplicates: false,
             trim: false,
             minHeight: undefined,
             maxHeight: 120,
             style: {
               colors: [],
               fontSize: '12px',
               fontFamily: 'Helvetica, Arial, sans-serif',
               fontWeight: 400,
               cssClass: 'apexcharts-xaxis-label',
             },
             offsetX: 0,
             offsetY: 0,
             format: undefined,
             formatter: undefined,
             datetimeUTC: false,
             datetimeFormatter: {
               year: 'yyyy',
               month: "MMM 'yy",
               day: 'dd MMM',
               hour: 'HH:mm',
             },
           },
           axisBorder: {
             show: false,
             color: '#78909C',
             height: 1,
             width: '100%',
             offsetX: 0,
             offsetY: 0
           },
           axisTicks: {
             show: false,
             borderType: 'solid',
             color: '#78909C',
             height: 6,
             offsetX: 0,
             offsetY: 0
           },
           tickAmount: undefined,
           tickPlacement: 'between',
           min: undefined,
           max: undefined,
           range: undefined,
           floating: false,
           decimalsInFloat: undefined,
           position: 'bottom',
           title: {
             text: undefined,
             offsetX: 0,
             offsetY: 0,
             style: {
               color: undefined,
               fontSize: '12px',
               fontFamily: 'Helvetica, Arial, sans-serif',
               fontWeight: 600,
               cssClass: 'apexcharts-xaxis-title',
             },
           },
           crosshairs: {
             show: false,
             width: 1,
             position: 'back',
             opacity: 0.9,
             stroke: {
               color: '#b6b6b6',
               width: 0,
               dashArray: 0,
             },
             fill: {
               type: 'solid',
               color: '#B1B9C4',
               gradient: {
                 colorFrom: '#D8E3F0',
                 colorTo: '#BED1E6',
                 stops: [0, 100],
                 opacityFrom: 0.4,
                 opacityTo: 0.5,
               },
             },
             dropShadow: {
               enabled: false,
               top: 0,
               left: 0,
               blur: 1,
               opacity: 0.4,
             },
           },
           tooltip: {
             enabled: false,
             formatter: undefined,
             offsetY: 0,
             style: {
               fontSize: 0,
               fontFamily: 0,
             },
           },
         },
         legend: {
           show: false,
         },
         grid: {
           show: false,
           borderColor: '#90A4AE',
           strokeDashArray: 0,
           position: 'back',
           xaxis: {
             lines: {
               show: false
             }
           },
           yaxis: {
             lines: {
               show: false
             }
           },
           row: {
             colors: undefined,
             opacity: 0.5
           },
           column: {
             colors: undefined,
             opacity: 0.5,
           },
           padding: {
             top: 0,
             right: 0,
             bottom: 0,
             left: 0
           },
         }
       },

       cpuSampling: 10,

       series: []
     }
   },
   computed: {
     memoryDotClass: function () {
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
       var i;

       if (this.series.length > 0) {
         for (i = 0; i < this.info.cpu.percents.length; i++) {
           this.series[i].data.push(this.info.cpu.percents[i]);

           if (this.series[i].data.length > this.cpuSampling) {
             this.series[i].data.shift();
           }
         }
       } else {
         this.series = []
         for (i = 0; i < this.info.cpu.percents.length; i++) {
           var dataArray = Array.apply(null, new Array(this.cpuSampling - 1)).map(Number.prototype.valueOf, 0);
           dataArray.push(this.info.cpu.percents[i]);

           var obj = {
             name: i,
             data: dataArray
           }

           this.series.push(obj);
         }
       }

       this.$refs.chart.updateSeries(this.series);
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
