<template>
  <div
    id="user-info"
    :style="{ 'border-bottom': '2px solid ' + borderColor, 'border-right': '2px solid ' + borderColor }"
    ref="userinfo">
    <div id="user-status">
      <img alt="No user" :src="avatarUrl" v-if="avatarUrl" />
      <h1>{{ username }}</h1>
    </div>
    <div id="playlists" ref="allplaylists">
      <div class="playlist"
           v-for="playlist in userPlaylists"
           :key="userPlaylists.indexOf(playlist)"
           @click="selectPlaylist(playlist[1])"
           :style="{ 'color': borderColor }">
        <span v-if="showIndex">{{ userPlaylists.indexOf(playlist) + 1 }}.</span>
        <p>{{ playlist[0] }}</p>
      </div>
    </div>
  </div>
</template>

<script>
 export default {
   name: 'UserInfo',
   data() {
     return {
       avatarUrl: '',
       username: '',
       userPlaylists: [["Local Playlist", 0]],
       currentPlaylistId: 0,
       showIndex: false
     }
   },

   props: {
     borderColor: String
   },

   mounted() {
     window.changePlaylistStyle = this.changePlaylistStyle;
     window.setUserPlaylists = this.setUserPlaylists;
     window.updateUserInfo = this.updateUserInfo;
     window.selectPlaylist = this.selectPlaylist;
     window.setIndexStyle = this.setIndexStyle;
     window.scrollPlaylistUp = this.scrollPlaylistUp;
     window.scrollPlaylistDown = this.scrollPlaylistDown;
   },

   methods: {
     selectPlaylist(pid) {
       window.pyobject.change_playlist([Number(pid)]);
     },

     changePlaylistStyle(index) {
       /* Change the song's style to show that the song is playing */
       var init = undefined
       if (index instanceof Array) {
         init = true;
         index = index[0];
       }

       if (index != this.currentPlaylistId || init) {
         var target = this.$refs.allplaylists.getElementsByClassName('playlist')[index];
         var prev = this.$refs.allplaylists.getElementsByClassName('playlist')[this.currentPlaylistId]
         var backgroundColor = document.body.style.backgroundColor;

         target.style.backgroundColor = this.borderColor;
         target.style.color = backgroundColor;
         if (!init) {
           prev.style.backgroundColor = backgroundColor;
           prev.style.color = this.borderColor;
           window.resetPrevIndex();
         }
         this.currentPlaylistId = index;
       }
     },

     setUserPlaylists(playlists) {
       this.userPlaylists = [["Local Playlist", 0]];
       for (var i = 0; i < playlists.length; i++) {
         this.userPlaylists.push(playlists[i]);
       }
     },

     updateUserInfo(info) {
       this.username = info[0];
       this.avatarUrl = info[1];
     },

     setIndexStyle(show) {
       this.showIndex = show;
     },

     scrollPlaylistUp() {
       this.$refs.userinfo.scrollTop += 30;
     },

     scrollPlaylistDown() {
       this.$refs.userinfo.scrollTop -= 30;
     }
   }
 }
</script>

<style scoped>
 #user-info {
   position: fixed;
   width: 20%;
   height: 82%;
   top: 0;
   left: 0;
   text-align: center;
   /* text-overflow: ellipsis; */
   overflow-y: scroll;
 }

 #user-status {
   margin-top: 10%;
 }

 #user-status > img {
   width: 50%;
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
