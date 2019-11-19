  agroMoMapUI <- function(id){
    ns <- NS(id)
    tags$div(id = ns(id),

    tags$div(
    id = paste0(ns("invert"),"_container"),
    checkboxInput(ns("invert"), label = "inverted", value = FALSE)
    ),
    tags$div(
      id = paste0(ns("countrycont"),"_container"),
      checkboxInput(ns("countrycont"), label = "add country contour to the background", value = TRUE)
    ),
      tags$div(
      id = paste0(ns("datasource"),"_container"),
      selectInput(ns("datasource"),"data source:",NA)
    ),
    tags$div(
      id = paste0(ns("palette"),"_container"),
      selectInput(ns("palette"),"palette:",NA)
    ),
    tags$div(
      id = paste0(ns("colnumb"),"_container"),
      selectInput(ns("colnumb"),"number of colours:",NA)
    ),
    tags$div(
      id = paste0(ns("min"),"_container"),
      textInput(ns("min"),"minimum value:",NA)
    ),
    tags$div(
      id = paste0(ns("minprec"),"_container"),
      selectInput(ns("minprec"),"precision of rounding:",NA)
    ),
        tags$div(
      id = paste0(ns("max"),"_container"),
      textInput(ns("max"),"maximum value:",NA)
    ),
    tags$div(
      id = paste0(ns("maxprec"),"_container"),
      selectInput(ns("maxprec"),"precision of rounding:",NA)
    ),
    tags$div(
      id = paste0(ns("maskcol"),"_container"),
      selectInput(ns("maskcol"),"mask colour:",NA)
    ),
    
    tags$div(
      id = paste0(ns("maptitle"),"_container"),
      textInput(ns("maptitle"), "map title:",)
    ),
    tags$div(id="maskpreview","mask preview:"),
    tags$div(id="palettepreview","palette preview:"),
    
#itt a funkcionalitas kerdeses    
    tags$div(id = ns("Buttons"),
    actionButton(ns("create"),label = "CREATE MAP"),
    actionButton(ns("save"),label="SAVE to FILE")),             
       

  }
  
  
