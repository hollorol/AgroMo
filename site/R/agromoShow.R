agroMoShowUI <- function(id){
  ns <- NS(id)
  tags$div(id = ns(id),
           tagList(
            column(4,
                   #tableOutput(ns("outputSelection"))
                   DT::dataTableOutput(ns("outputSelection"), width="100%")
                   #tags$script(src="outputSelector.js") ## This js file generates a DataTable into the #showdiv-table-output_container div. See the sourcecode for further information.
                                       ),
             column(8,
                    tags$div(id="observations","OBSERVATIONS:"),
                    tags$div(id="simres","SIMULATION RESULTS:"),
                    tags$div(id="createplot","CREATE PLOT WITH:"),
                    tags$div(id="repavg","Repetitions averaged"),
                    tags$div(id=ns("experimentID_container"),selectInput(ns("experimentID"), "EXPERIMENT ID:",choices = 'NULL')),
                    tags$div(id=ns("treatmentID_container"),selectInput(ns("treatmentID"), "TREATMENT ID:",choices = 'NULL')),
                    checkboxInput(ns("averagep"),"", value = TRUE),
                    #checkboxInput(ns("averagep"),"Repetitions averaged", value = TRUE),
                    #checkboxGroupInput(ns("createPlot"),label = "CREATE PLOT WITH:",choices = NULL),
                    tags$div(id=ns("table-output_container")), 
                    tags$script(src="showTableOutput.js"), ## This js file generates a DataTable into the #showdiv-table-output_container div. See the sourcecode for further information.
                    tags$script(src="outputSelector.js"), ## This js file generates a DataTable into the #showdiv-table-output_container div. See the sourcecode for further information.
                    actionButton(ns("show"),"PLOT"),
                    actionButton(ns("export"),"EXPORT")
                                        )
           )
       )
}
agroMoShow <- function(input, output, session){
  ns <- session$ns
  dat <- reactiveValues()
  dat[["dataenv"]] <-readRDS("output/outputs.RDS")
  modellOutputNames <- ls(dat$dataenv)
  measurement <- fread("observations/observations.csv") 
  ## updateCheckboxGroupInput(session,"outSelector",choices = modellOutputNames)

  output$outputSelection <- DT::renderDataTable({
    
  DT::datatable(data.frame(outputName = modellOutputNames), options = list(autowidth = TRUE, paginate = FALSE, scrollY = 600, scrollX = FALSE, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
  })
  #dataTableOutput(session,"outSelector",choices = modellOutputNames)
  updateSelectInput(session,"experimentID", choices = unique(measurement$experiment))
  updateSelectInput(session,"treatment", choices = unique(measurement$treatment))
  ## dataTable <- callModule(graphControl,"mainControl",reactive({input$show}))

  observeEvent(input$show,{
    print(dataTable$data)
    showModal(multiPlotUI(ns("plotka"))) 
    callModule(multiPlot,"plotka",dat$dataenv,reactive({measurement}),reactive({input$outSelector}),reactive({dataTable$data}),
               reactive({input$experimentID}),reactive({input$treatment}),repetAvg = reactive({input$averagep}))
  })


    observeEvent(input$refresh,{
      dat[["dataenv"]] <-readRDS("output/outputs.RDS")
      modellOutputNames <- ls(dat$dataenv)
    updateCheckboxGroupInput(session,"outSelector",choices = modellOutputNames)
  })
}
