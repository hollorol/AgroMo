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
                            shiny::tags$img("",src = "www/img/banner/Base.svg", draggable = FALSE)),
                        hidden(shiny::tags$div(id = "Show-banner-div", class = "banner",
                                   shiny::tags$img("",src = "www/img/banner/Show.svg", draggable = FALSE))),
                        hidden(shiny::tags$div(id = "Site-banner-div", class = "banner",
                                   shiny::tags$img("",src="www/img/banner/Site.svg", draggable = FALSE))),
                        hidden(shiny::tags$div(id = "Map-banner-div", class = "banner",
                                  shiny::tags$img("",src="www/img/banner/Map.svg", draggable = FALSE))),
                        hidden(shiny::tags$div(id = "Sitegen-banner-div", class = "banner",
                                               shiny::tags$img("",src="www/img/banner/SiteGeneratorBanner.png", draggable = FALSE))),
                        hidden(shiny::tags$div(id = "Parana-banner-div", class = "banner",
                                               shiny::tags$img("",src="www/img/banner/ParAnaBanner.png", draggable = FALSE))),
                        hidden(shiny::tags$div(id = "Grid-banner-div", class = "banner",
                                    shiny::tags$img("",src="www/img/banner/Grid.svg", draggable = FALSE))),
                        hidden(shiny::tags$div(id = "BDB-banner-div", class = "banner",
                                    shiny::tags$img("",src="www/img/banner/BDB.png", draggable = FALSE)))))),
             fluidRow(class= "innerGui", shiny::tags$div(id = "root",args)))
}
