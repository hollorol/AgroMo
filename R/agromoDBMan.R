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
             selectInput(ns("database"),"DATABASE:",choices=c("grid output", "site output", "soil"))
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
                    actionButton(ns("deltab"),label = "DELETE TABLE")),

            tableOutput(ns("result"))
           
           
           
           
  )
}

#' agroMoDBMan 
#' 
#' asdfasfd
#' @param input input
#' @importFrom shiny reactiveValues observe updateSelectInput observe renderPlot renderImage 
#' @importFrom DBI dbConnect

agroMoDBMan <- function(input, output, session, baseDir, dbGridConn, dbConn){
    ns <- session$ns

    dbconnections <- reactiveValues(soil = NULL,"grid output" = NULL, "site output" = NULL) 


    observe({
         dbconnections[["soil"]] <- dbConnect(RSQLite::SQLite(),file.path(baseDir,"database/soil.db"))
         dbconnections[["grid output"]] <- dbGridConn
         dbconnections[["site output"]] <- dbConn
     })


    observe({
        if(!is.null(dbconnections[[input$database]])){
        updateSelectInput(session,"datatable",
                          choices = grep("_error$",dbListTables(dbconnections[[input$database]]),value = TRUE, invert = TRUE))

        }
    })


    observeEvent(input$deltab,{
                     dbRemoveTable(dbConn,input$datatable)
                     updateSelectInput(session,"datatable",
                                       choices =grep("_error$",dbListTables(dbconnectios[[input$database]]),value = TRUE, invert = TRUE))

    })

    observeEvent(input$showtab,{
                     output$result <- renderTable({
                         dbGetQuery(dbconnections[[isolate(input$database)]],sprintf("SELECT * FROM %s LIMIT 365", isolate(input$datatable)))
                     })
    })

    observeEvent(input$querytab,{
                     output$result <- renderTable({
                         dbGetQuery(dbconnections[[isolate(input$database)]],sprintf(input$sqlquery, isolate(input$datatable)))
                     })

    })
}
