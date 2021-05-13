import Vue from "vue"
import Vuex from "vuex"

Vue.use(Vuex)

const store = new Vuex.Store({
  state: {
    currentProcess: 0,
    currentProcessIndex: 0,
    processInfos: []
  },
  getters: {
    currentProcess: state => {
      return state.currentProcess;
    },
    currentProcessIndex: state => {
      return state.currentProcessIndex;
    },
    processInfos: state => {
      return state.processInfos;
    }
  },
  mutations: {
    updateCurrentProcess(state, process) {
      state.currentProcess = process.pid;

      var processes = state.processInfos.map(function (process) { return process.pid });
      state.currentProcessIndex = processes.indexOf(state.currentProcess);
    },
    updateProcessInfos(state, infos) {
      state.processInfos = infos;
    }
  }
})

export default store
