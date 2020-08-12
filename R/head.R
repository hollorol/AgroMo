#' baseHead
#'
#' This function provides the header for the AgroMo web-app.
#' importFrom shiny tags
#' @keywords internal


baseHead<-function(){
  shiny::tags$head(
         shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/myway.css"),
         shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/site.css"),
         shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/base.css"),
         shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/show.css"),
         shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/grid.css"),
         shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/map.css"),
         shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/sitegen.css"),
         shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/parana.css"),
         shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/dbman.css"),
         shiny::tags$link(rel = "stylesheet", type = "text/css",
                   href = "www/showTableOutput.css"),
                   shiny::tags$script(HTML("
                                          var dictionary;
                                          let xhttp = new XMLHttpRequest();
                                          xhttp.onreadystatechange = function(){
                                              dictionary = JSON.parse(this.responseText);
                                          }
                                          xhttp.open(\"GET\", \"www/jsonfiles/agromo_languages.json\",
                                                     true);
                                          xhttp.send();
                                          ")),
         shiny::tags$script(src = "www/jsTable.js"),
         shiny::tags$script(src = "www/customFunctions.js"),
         shiny::tags$script(src = "www/getIndexesFromSelection.js"),
         shiny::tags$script(src="www/jquery-ui-min.js"))
}
"inst/"
