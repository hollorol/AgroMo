#' agroMoDBManUI
#'
#' Bla
#' @param id id
#' @importFrom shiny NS tags checkboxInput selectInput textInput actionButton plotOutput updateSelectInput observe imageOutput

agroMoDBManUI <- function(id){
  
  
  
  ns <- NS(id)
  tags$div(id = ns(id),
           
           
           tags$div(
             id = paste0(ns("sqlquery"),"_container"), 
             textInput(ns("sqlquery"),"SQL QUERY:","")
           ),
           tags$div(
             id = paste0(ns("database"),"_container"),
             selectInput(ns("database"),"DATABASE:",choices=c(""))
           ),
           tags$div(
             id = paste0(ns("datatable"),"_container"),
             selectInput(ns("datatable"),"DATA TABLE:",choices=c(""))
           ),
           tags$div(id = ns("Buttons"),
                    actionButton(ns("showtab"),label = "SHOW TABLE")),
           tags$div(id = ns("Buttons"),
                    actionButton(ns("querytab"),label = "QUERY TABLE")),
           tags$div(id = ns("Buttons"),
                    actionButton(ns("deltab"),label = "DELETE TABLE"))
           
           
           
           
  )
}

#' agroMoDBMan 
#' 
#' asdfasfd
#' @param input input
#' @importFrom shiny reactiveValues observe updateSelectInput observe renderPlot renderImage 
#' @importFrom DBI dbConnect


agroMoDBMan <- function(input, output, session){
  
  
  ns <- session$ns
 
}
