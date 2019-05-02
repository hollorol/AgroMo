baseWindow <- function(...){
  args<-list(...)
  tags$div(id = "mainWindow",
           class = "baseWindow",
           tags$div(
             uiOutput("banners"),
             fluidRow(class= "innerGui", div(id = "root",args))))
}




## mainButtons <- function(){
    
##
