agroMoShowUI <- function(id){
  ns <- NS(id)
  tags$div(id = ns(id),
           tagList(
             column(4,
                    checkboxGroupInput(ns("outSelector"),label = "Choose output",choices = NULL),
                    actionButton(ns("refresh"), label = "refresh")
                    ),
             column(8,
                    selectInput(ns("experimentID"), "experiment ID",choices = 'NULL'),
                    selectInput(ns("treatment"), "treatment ID",choices = 'NULL'),
                    checkboxInput(ns("averagep"),"Average repetitions?", value = TRUE),
                    graphControlUI(ns("mainControl")), 
                    actionButton(ns("show"),"Show!")
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
  updateCheckboxGroupInput(session,"outSelector",choices = modellOutputNames)
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
