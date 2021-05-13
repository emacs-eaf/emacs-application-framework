<template>
  <div
    ref="processlist"
    class="processlist">
    <div
      class="item"
      v-for="item in processInfos"
      @click="selectProcess(item)"
      :key="item.path"
      :style="{ 'background': itemBackgroundColor(item), 'color': itemForegroundColor(item) }">
      <div class="item-cpu">
        {{ item.cpu_percent }}%
      </div>
      <div class="item-memory">
        {{ item.memory }}
      </div>
      <div class="item-pid">
        {{ item.pid }}
      </div>
      <div class="item-username">
        {{ item.username }}
      </div>
      <div class="item-name">
        {{ item.name }}
      </div>
      <div class="item-cmdline">
        {{ item.cmdline }}
      </div>
    </div>
  </div>
</template>

<script>
 import { mapState } from "vuex";

 export default {
   name: 'Processlist',
   data() {
     return {
       backgroundColor: "",
       foregroundColor: "",
     }
   },
   computed: mapState([
     "currentProcess",
     "currentProcessIndex",
     "processInfos"
   ]),
   props: {
   },
   mounted() {
     window.initProcesslistColor = this.initProcesslistColor;
     window.updateProcessInfo = this.updateProcessInfo;
     window.scrollUp = this.scrollUp;
     window.scrollDown = this.scrollDown;
     window.scrollUpPage = this.scrollUpPage;
     window.scrollDownPage = this.scrollDownPage;
     window.scrollToBegin = this.scrollToBegin;
     window.scrollToBottom = this.scrollToBottom;
   },
   methods: {
     initProcesslistColor(backgroundColor, foregroundColor) {
       this.backgroundColor = backgroundColor;
       this.foregroundColor = foregroundColor;
     },

     updateProcessInfo(infos) {
       this.$store.commit("updateProcessInfos", infos);
     },

     selectProcess(item) {
       this.$store.commit("updateCurrentProcess", item);
     },

     itemBackgroundColor(item) {
       if (item.pid == this.currentProcess) {
         return this.foregroundColor;
       } else {
         return this.backgroundColor;
       }
     },

     itemForegroundColor(item) {
       if (item.pid == this.currentProcess) {
         return this.backgroundColor;
       } else {
         return this.foregroundColor;
       }
     },

     scrollUp() {
       this.$refs.processlist.scrollTop += 30;
     },

     scrollDown() {
       this.$refs.processlist.scrollTop -= 30;
     },

     scrollUpPage() {
       this.$refs.processlist.scrollTop += this.$refs.processlist.offsetHeight;
     },

     scrollDownPage() {
       this.$refs.processlist.scrollTop -= this.$refs.processlist.offsetHeight;
     },

     scrollToBegin() {
       this.$refs.processlist.scrollTop = 0;
     },

     scrollToBottom() {
       this.$refs.processlist.scrollTop = this.$refs.processlist.scrollHeight;
     },
   }
 }
</script>

<style scoped>
 .processlist {
   width: 100%;
   height: 100%;

   white-space: nowrap;
   text-overflow: ellipsis;
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

 .item-cpu {
   width: 60px;
   text-align: right;
   padding-right: 10px;
 }

 .item-memory {
   width: 5%;
   text-align: right;
   padding-right: 10px;
 }

 .item-pid {
   width: 60px;
   text-align: right;
   padding-right: 10px;
 }

 .item-username {
   width: 10%;
   margin-left: 10px;
   margin-right: 10px;
 }

 .item-name {
   width: 20%;
 }

 .item-cmdline {
   width: 40%;
 }
</style>
