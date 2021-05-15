import Vue from "vue"
import Vuex from "vuex"

Vue.use(Vuex)

const store = new Vuex.Store({
  state: {
    currentTrack: "",
    currentTrackIndex: 0,
    numberWidth: 0,
    fileInfos: []
  },
  getters: {
    currentTrack: state => {
      return state.currentTrack;
    },
    currentTrackIndex: state => {
      return state.currentTrackIndex;
    },
    fileInfos: state => {
      return state.fileInfos;
    }
  },
  mutations: {
    updateCurrentTrack(state, track) {
      state.currentTrack = track;

      var tracks = state.fileInfos.map(function (track) { return track.path });
      state.currentTrackIndex = tracks.indexOf(state.currentTrack);
    },
    updateFileInfos(state, infos) {
      state.fileInfos = infos;

      state.numberWidth = state.fileInfos.length.toString().length;
    }
  }
})

export default store
