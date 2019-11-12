#' createLayout
#'
#' This function creates the layout of the App
#' @importFrom shinyjs hidden
#' @importFrom shiny fluidRow
#' @keywords internal

createLayout <- function(...){
  args<-list(...)
  shiny::tags$div(id = "mainWindow",
      class = "baseWindow",
      shiny::tags$div(
             shiny::tags$div("banners",
                      tagList(
                        shiny::tags$div(id = "Base-banner-div", class = "banner",
                            shiny::tags$img("",src = "www/img/banner/Base.svg")),
                        hidden(shiny::tags$div(id = "Show-banner-div", class = "banner",
                                   shiny::tags$img("",src = "www/img/banner/Show.png"))),
                        hidden(shiny::tags$div(id = "Site-banner-div", class = "banner",
                                   shiny::tags$img("",src="www/img/banner/Site.png"))))),
             fluidRow(class= "innerGui", shiny::tags$div(id = "root",args))))
}
