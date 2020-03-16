#' agroMoParAnaUI
#'
#' Bla
#' @param id id
#' @importFrom shiny NS tags checkboxInput selectInput textInput actionButton plotOutput updateSelectInput observe imageOutput

agroMoParAnaUI <- function(id){
  
  
  
  ns <- NS(id)
  tags$div(id = ns(id),
           
           
           tags$div(
             id = paste0(ns("paranait"),"_container"), 
             textInput(ns("paranait"),"number of iterations:","")
           ),
           tags$div(id=ns("paranatable-container"),
                    tableOutput(ns("paoutputSelection"))),
           dataTableOutput(ns("paranaoutputSelection")),
           tags$div(
             id = paste0(ns("paranaini"),"_container"),
             selectInput(ns("paranaini"),"INI file:",choices=c(""))
           ),
           tags$div(
             id = paste0(ns("paranaexp"),"_container"),
             selectInput(ns("paranaexp"),"EXPERIMENT ID:",choices=c(""))
           ),
           tags$div(
             id = paste0(ns("paranatreat"),"_container"),
             selectInput(ns("paranatreat"),"TREATMENT ID:",choices=c(""))
           ),
           tags$div(
             id = paste0(ns("charfunc"),"_container"),
             selectInput(ns("charfunc"),"characterization function for sensitivity:",choices=c("mean"))
           ),
           tags$div(
             id = paste0(ns("metfunc"),"_container"),
             selectInput(ns("metfunc"),"metric function for calibration:",choices=c("minima of RMSE"))
           ),
           tags$div(id="paranatype","TYPE OF PARAMETER ANALYSIS:"),
           tags$div(
             id = paste0(ns("paranaradio"), "_container"), 
             radioButtons(ns("paranaradio"),"",choices=c("Parameter Sweep","Sensitivity Analysis","Calibration"), inline = TRUE)
           ),
           tags$div(id = ns("Buttons"),
                    actionButton(ns("paranado"),label = "DO ANALYSIS"))
           
           
           
           
  )
}

#' agroMoMap 
#' 
#' asdfasfd
#' @param input input
#' @importFrom shiny reactiveValues observe updateSelectInput observe renderPlot renderImage 
#' @importFrom DBI dbConnect


agroMoParAna <- function(input, output, session){
  
  
  ns <- session$ns
  
  
}
