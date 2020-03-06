// define global logging function
function log(msg) {
  var d = new Date(), ts;
  ts = d.toLocaleTimeString().split(' ');
  ts = ts[0] + '.' + d.getMilliseconds() + ' ' + ts[1];
  
  console.log('[' + ts + '] ' + msg);
}


// log DOMContentLoaded event
document.addEventListener('DOMContentLoaded', function(ev) {
  log('DOMContentLoaded fired');
});


// log window onload event
window.addEventListener('load', function(ev) {
  log('window load event fired');
});
