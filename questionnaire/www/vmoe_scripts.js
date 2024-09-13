
$(document).on('shiny:sessioninitialized', function(event) {
  var Agent = navigator.userAgent;
  var message = {data : Agent};
  //var isChrome = /Chrome/.test(navigator.userAgent);
  //var isFirefox = /Firefox/.test(navigator.userAgent);
  //var message = {data : [Agent, isChrome, isFirefox] };
  Shiny.onInputChange("usrAgt", message);
});
