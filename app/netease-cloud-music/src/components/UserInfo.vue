<template>
  <div id="user-info" :style="{ 'border-bottom': '2px solid ' + borderColor, 'border-right': '2px solid ' + borderColor }">
    <div id="user-status">
      <img alt="No user" :src="avatarUrl" v-if="avatarUrl" />
      <h1>{{ username }}</h1>
    </div>
    <div id="playlists" ref="allplaylists">
      <div class="playlist"
           v-for="playlist in userPlaylists"
           :key="playlist[0]"
           @click="selectPlaylist(playlist[2])"
           :style="{ 'color': borderColor }">
        <p>{{ playlist[1] }}</p>
      </div>
    </div>
  </div>
</template>

<script>
 export default {
   name: 'UserInfo',
   data() {
     return {
       /* <!-- NOTE: DEBUG--> */
       avatarUrl: "http://p1.music.126.net/cxHXzOcY-wKq72WF5-HSdg==/109951165429433434.jpg",
       username: 'SpringHan',
       userPlaylists: [[0, "Like", 209103]]
     }
   },

   props: {
     borderColor: String
   },

   mounted() {
     window.changePlaylistStyle = this.changePlaylistStyle;
   },

   methods: {
     selectPlaylist(pid) {
       /* .. */
     },

     changePlaylistStyle(index, isCurrent) {
       /* Change the song's style to show that the song is playing */
       var target = this.$refs.allplaylists.getElementsByClassName('playlist')[index];
       var backgroundColor = document.getElementById("app").style.backgroundColor;
       if (isCurrent) {
         target.style.backgroundColor = this.borderColor;
         target.style.color = backgroundColor;
       } else {
         target.style.backgroundColor = backgroundColor;
         target.style.color = this.borderColor;
       }
     }
   }
 }
</script>

<style scoped>
 #user-info {
   position: absolute;
   width: 20%;
   height: 82%;
   top: 0;
   left: 0;
   text-align: center;
   text-overflow: ellipsis;
 }

 #user-status {
   margin-top: 10%;
 }

 #user-status > img {
   width: 50%;
   height: 50%;
   border-radius: 50%;
 }

 #user-status > h1 {
   margin-top: 8%;
 }

 #playlists {
   margin-top: 15%;
 }

 .playlist > p {
   font-size: 25px;
 }
</style>
