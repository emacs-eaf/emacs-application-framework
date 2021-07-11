<template>
  <div
    id="playlist-section"
    ref="playlists"
    :style="{ 'border-bottom': '2px solid ' + foregroundColor }">
    <div
      class="playlist"
      v-for="song in playlists"
      :key="song[0]"
      @click="playSong(song)"
      :style="{ 'color': foregroundColor }">
      <p>
        <span class="song-id">{{ song[0] + 1 }}</span>
        .
        <span class="song">{{ song[1] }}</span>
        &nbsp;-&nbsp;
        <span class="artist">{{ song[2] }}</span>
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
       playlists: [[0, "Lemon", "Kenshi Yorezi", 10923],
                   [1, "Loser", "Kenshi Yorezi", 921038]],
       // TODO: Debug
     }
   },
   props: {
     backgroundColor: String,
     foregroundColor: String
   },

   mounted() {
     window.initColor = this.initColor;
     window.addSong = this.addSong;
     window.setPlaylist = this.setPlaylist;
     window.changeSongStyle = this.changeSongStyle;
     window.playNextOrPrevSong = this.playNextOrPrevSong;
   },

   created() {
     new QWebChannel(qt.webChannelTransport, channel => {
       window.pyobject = channel.objects.pyobject;
     });
   },

   methods: {
     initColor(backgroundColor, foregroundColor) {
       this.backgroundColor = backgroundColor;
       this.foregroundColor = foregroundColor;
     },

     addSong(name, artist) {
       this.playlists.push([this.playlists.length, name, artist]);
     },

     setPlaylist(songs) {
       this.playlists = songs;
     },

     playSong(songInfo) {
       window.pyobject.play_song([songInfo[3], songInfo[1], songInfo[2]]);
     },

     playNextOrPrevSong(prev) {
       window.pyobject.play_next_or_prev(prev);
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
