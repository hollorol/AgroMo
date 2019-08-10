// $(document).on('shiny:recalculated', function(event) {
//   // append a character string to the output value
//     '#showdiv-outputSelection'
//     $('.even').css("color","green");
// });

// $(document).on('shiny:value', function(event) {
//   // cancel the output of the element with id 'foo'
//   if (event.target.id === 'foo') {
//     event.preventDefault();
//   }
// });
// $(document).on("shiny:value", function(e) {
//   if (e.name == "showdiv-outputSelection") {  // mytable is the name / id of the output element
//       a=e.value;
//       // e.preventDefault();
//      $("#showdiv-outputSelection")
//       .removeClass("shiny-output-error");  // get rid of potential previous error styling
//       // $('.even').css("color","green");
//   }
// });

Shiny.addCustomMessageHandler("getTable", function(message) {
    Shiny.onInputChange("showdiv-outTable",getJSONFromDataTable());
    console.log(getJSONFromDataTable());
});
