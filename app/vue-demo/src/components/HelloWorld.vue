<template>
  <div class="hello">
    <h1>{{ msg }}</h1>
    <ul>
      <li>Copy vue-demo to directory emacs-applications-framework/app/</li>
      <li>Write vue.js code and use command <div class="command">npm run serve</div> for test</li>
      <li>Send PR, EAF will run command <div class="command">npm run build</div> before run app</li>
    </ul>
  </div>
</template>

<script>
export default {
  name: 'HelloWorld',
  props: {
    msg: String
  }
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
 h1 {
   margin-bottom: 60px;
   font-size: 30px;
 }

 .command {
   font-size: 18px;
   display: inline-block;
   color: #490AEF;
 }
</style>
