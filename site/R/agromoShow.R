agroMoShowUI <- function(id){
  ns <- NS(id)
  tags$div(id = ns(id),
           tagList(
             tags$img(id = ns("base_bb"),src="img/base_banner_button.svg"),
             tags$img(id = ns("map_bb"),src="img/map_banner_button.svg"),
             tags$img(id = ns("grid_bb"),src="img/grid_banner_button.svg"),
             tags$img(id = ns("show_bb"),src="img/show_banner_button.svg"),
             column(4,
                    checkboxGroupInput(ns("outSelector"),label = "SIMULATION RESULTS:",choices = NULL)
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
  updateCheckboxGroupInput(session,"outSelector",choices = modellOutputNames)
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
