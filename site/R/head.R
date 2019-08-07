baseHead<-function(){
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "myway.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "site.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "base.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "show.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "grid.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "showTableOutput.css"),
            tags$script(src = "jsTable.js"),
            tags$script(src = "customFunctions.js"),
            tags$script(src="jquery-ui-min.js"))
}
