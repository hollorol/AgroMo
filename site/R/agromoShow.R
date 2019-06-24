agroMoShowUI <- function(id){
  ns <- NS(id)
  tags$div(id = ns(id),
           tagList(
             column(4,
                    ## checkboxGroupInput(ns("outSelector"),label = "SIMULATION RESULTS:",choices = NULL)
                    DT::dataTableOutput(ns("outputSelection"))
                    ),
             column(8,
                    checkboxGroupInput(ns("obsevations"),label = "OBSERVATIONS:",choices = NULL),
                    selectInput(ns("experimentID"), "EXPERIMENT ID:",choices = 'NULL'),
                    selectInput(ns("treatment"), "TREATMENT ID:",choices = 'NULL'),
                    checkboxInput(ns("averagep"),"Repetitions averaged", value = TRUE),
                    checkboxGroupInput(ns("createPlot"),label = "CREATE PLOT WITH:",choices = NULL),
                    #dataTableOutput(ns("createPlot"),label = "CREATE PLOT WITH:",choices = NULL),
                    graphControlUI(ns("mainControl")), 
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

    DT::datatable(data.frame(outputName = modellOutputNames), options = list(autowidth = TRUE))


  })

  #dataTableOutput(session,"outSelector",choices = modellOutputNames)
  updateSelectInput(session,"experimentID", choices = unique(measurement$experiment))
  updateSelectInput(session,"treatment", choices = unique(measurement$treatment))
  dataTable <- callModule(graphControl,"mainControl",reactive({input$show}))

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
