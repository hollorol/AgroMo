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
         shiny::tags$link(rel = "stylesheet", type = "text/css",
                   href = "www/showTableOutput.css"),
         shiny::tags$script(src = "www/jsTable.js"),
         shiny::tags$script(src = "www/customFunctions.js"),
         shiny::tags$script(src = "www/getIndexesFromSelection.js"),
         shiny::tags$script(src="www/jquery-ui-min.js"))
}
