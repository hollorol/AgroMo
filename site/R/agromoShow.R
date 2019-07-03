agroMoShowUI <- function(id){
  ns <- NS(id)
  tags$div(id = ns(id),
           tagList(
            column(4,
                    ## checkboxGroupInput(ns("outSelector"),label = "SIMULATION RESULTS:",choices = NULL)
                    DT::dataTableOutput(ns("outputSelection"), width="100%")
                    ),
             column(8,
                    tags$div(id="observations","OBSERVATIONS:"),
                    tags$div(id="simres","SIMULATION RESULTS:"),
                    tags$div(id="createplot","CREATE PLOT WITH:"),
                    tags$div(id="repavg","Repetitions averaged"),
                    tags$div(id="experimentid","EXPERIMENT ID:"),
                    tags$div(id="treat","TREATMENT ID:"),
                    #checkboxGroupInput(ns("observations"),label = "OBSERVATIONS:",choices = NULL),
                    tags$div(id="experimentID",selectInput(ns("experimentID"), "",choices = 'NULL')),
                    #selectInput(ns("experimentID"), "EXPERIMENT ID:",choices = 'NULL'),
                    #selectInput(ns("treatment"), "TREATMENT ID:",choices = 'NULL'),
                    selectInput(ns("treatment"), "",choices = 'NULL'),
                    checkboxInput(ns("averagep"),"", value = TRUE),
                    #checkboxInput(ns("averagep"),"Repetitions averaged", value = TRUE),
                    #checkboxGroupInput(ns("createPlot"),label = "CREATE PLOT WITH:",choices = NULL),
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

    DT::datatable(data.frame(outputName = modellOutputNames), options = list(width = "200%", paginate = FALSE, scrollY = 400))
    #DT::datatable(data.frame(outputName = modellOutputNames), options = list(autowidth = TRUE, paginate = FALSE, scrollY = 300))
    

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
