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

     changeSongStyle(index, isPlay) {
       /* Change the song's style to show that the song is playing */
       var target = this.$refs.playlists.getElementsByClassName('playlist')[index];
       if (isPlay) {
         target.style.backgroundColor = this.foregroundColor;
         target.style.color = this.backgroundColor;
       } else {
         target.style.backgroundColor = this.backgroundColor;
         target.style.color = this.foregroundColor;
       }
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
