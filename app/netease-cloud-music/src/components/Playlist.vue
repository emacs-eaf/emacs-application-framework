<template>
  <div
    id="playlist-section"
    ref="playlists">
    <div
      class="playlist"
      v-for="song in playlists"
      :key="playlists.indexOf(song)"
      @click="playSong(song)"
      :style="{ 'color': foregroundColor }">
      <p>
        <span class="song-index">{{ playlists.indexOf(song) + 1 }}</span>
        .
        <span class="song">{{ song[1] }}</span>
        &nbsp;-&nbsp;
        <span class="artist">{{ song[3] }}</span>
      </p>
    </div>
  </div>
</template>

<script>
 import { QWebChannel } from "qwebchannel"

 export default {
   name: 'Playlist',
   data() {
     return {
       playlists: [],
       prevIndex: -1
       // TODO: Debug
     }
   },
   props: {
     backgroundColor: String,
     foregroundColor: String
   },

   mounted() {
     window.setPlaylist = this.setPlaylist;
     window.changeSongStyle = this.changeSongStyle;
     window.playSong = this.playSong;
     window.AllPlaylists = this.playlists;
     window.resetSongStyle = this.resetSongStyle;
     window.resetPrevIndex = this.resetPrevIndex;
     window.scrollUp = this.scrollUp;
     window.scrollDown = this.scrollDown;
     window.scrollUpPage = this.scrollUpPage;
     window.scrollDownPage = this.scrollDownPage;
     window.scrollToBegin = this.scrollToBegin;
     window.scrollToBottom = this.scrollToBottom;
   },

   created() {
     new QWebChannel(qt.webChannelTransport, channel => {
       window.pyobject = channel.objects.pyobject;
     });
   },

   methods: {
     setPlaylist(songs) {
       this.playlists = songs;
     },

     playSong(songInfo) {
       window.pyobject.play_song([Number(songInfo[0]), songInfo[1], songInfo[3]]);
     },

     changeSongStyle(index) {
       /* Change the song's style to show that the song is playing */
       var target = this.$refs.playlists.getElementsByClassName('playlist')[index];
       if (this.prevIndex != -1) {
         var prevTarget = this.$refs.playlists.getElementsByClassName('playlist')[this.prevIndex];

         if (prevTarget.style.backgroundColor != this.backgroundColor) {
           prevTarget.style.backgroundColor = this.backgroundColor;
           prevTarget.style.color = this.foregroundColor;
         }
       }

       target.style.backgroundColor = this.foregroundColor;
       target.style.color = this.backgroundColor;
       target.scrollIntoView({block: 'center'});
       this.prevIndex = index;
     },

     resetSongStyle() {
       var target = this.$refs.playlists.getElementsByClassName('playlist')[this.prevIndex];
       if (target != undefined) {
         target.style.backgroundColor = this.backgroundColor;
         target.style.color = this.foregroundColor;
       }
     },

     resetPrevIndex() {
       this.resetSongStyle();
       this.prevIndex = -1;
     },

     scrollUp() {
       this.$refs.playlists.scrollTop += 30;
     },

     scrollDown() {
       this.$refs.playlists.scrollTop -= 30;
     },

     scrollUpPage() {
       this.$refs.playlists.scrollTop += this.$refs.playlists.offsetHeight;
     },

     scrollDownPage() {
       this.$refs.playlists.scrollTop -= this.$refs.playlists.offsetHeight;
     },

     scrollToBegin() {
       this.$refs.playlists.scrollTop = 0;
     },

     scrollToBottom() {
       this.$refs.playlists.scrollTop = this.$refs.playlists.scrollHeight;
     }
   }
 }
</script>

<style scoped>
 #playlist-section {
   position: absolute;
   width: 80%;
   height: 82%;
   top: 0;
   right: 0;
   overflow-y: scroll;
 }

 .playlist {
   display: flex;
   padding: 0.5%;
   padding-left: 10px;
 }

 .playlist > p {
   font-size: 20px;
 }
</style>
