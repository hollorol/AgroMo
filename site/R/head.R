baseHead<-function(){
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "myway.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "site.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "base.css"),
            tags$script(src = "customFunctions.js"))
  
}
