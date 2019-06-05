baseHead<-function(){
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "myway.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "site.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "base.css"),
            tags$script(src = "customFunctions.js"),
            tags$script(src="https://code.jquery.com/ui/1.12.1/jquery-ui.js"))
}
