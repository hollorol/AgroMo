#' agroMoShowUI
#' 
#' Bla
#' @importFrom shiny NS tagList tags column checkboxInput HTML actionButton
#' @importFrom DT renderDT DTOutput


agroMoShowUI <- function(id){
  ns <- NS(id)
  tags$div(id = ns(id),
           tagList(
            column(4,
                   DTOutput(ns("outputSelection"))
                   ),
             column(8,
                    tags$div(id="observations","OBSERVATIONS:"),
                    tags$div(id="simres","SIMULATION RESULTS:"),
                    tags$div(id="createplot","CREATE PLOT WITH:"),
                    tags$div(id="repavg","Repetitions averaged"),
                    tags$div(id=ns("experimentID_container"),selectInput(ns("experimentID"), "EXPERIMENT ID:",choices = 'NILL')),
                    tags$div(id=ns("treatmentID_container"),selectInput(ns("treatmentID"), "TREATMENT ID:",choices = 'NILL')),
                    tags$div(id=ns("groupID_container"),selectInput(ns("groupFUN"), "GROUP FUNCTION:",choices = c('NILL','minimum','maxum','variance','euclidean distance','difference'))),
                    checkboxInput(ns("averagep"),"", value = TRUE),
                    tags$div(id=ns("table-header_container")),
                    tags$div(id=ns("table-output_container")),
                    tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')),
                    tags$script(src="www/showTableOutput.js"),
                    tags$script(src="www/outputSelector.js"),

                    tags$script(HTML(
                    sprintf("
                            Shiny.addCustomMessageHandler('getTable', function(message) {
                             Shiny.onInputChange('%s',JSON.stringify(getJSONFromDataTable()));
                             Shiny.setInputValue('%s',Math.random());
                        console.log(getJSONFromDataTable());
                      });
", ns("outTable"),ns("showChanged")
)
                    )),

                    ## This js file generates a DataTable into the #showdiv-table-output_container div. See the sourcecode for further information.
                    actionButton(ns("show"),"PLOT"),
                    actionButton(ns("export"),"EXPORT")
                                        )
           )
       )
}


#' agroMoShow
#' 
#' Bla
#' @importFrom shiny reactiveValues updateSelectInput NS tagList tags column checkboxInput HTML actionButton
#' @importFrom data.table fread
#' @importFrom DT renderDT
#' @importFrom DBI dbListTables

agroMoShow <- function(input, output, session, dataenv, baseDir, connection){
  ns <- session$ns
  ## dat <- new.env()
  ## dat[["dataenv"]] <-readRDS("output/outputs.RDS")
  ## modellOutputNames <- ls(dat$dataenv)
  datas<- reactiveValues(show=0)
  initData <- reactiveValues(data = NULL,measurement = NULL)
  observe({
     initData$data <- dataenv()
  # browser()
  })
    output$outputSelection <- renderDT({
      DT::datatable(data.frame(outputName = initData$data), options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = 600, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
    })
  #
  # observe({
  #   initData$measurement <- fread(file.path(baseDir(),"observation/observations.csv"))
  #   updateSelectInput(session,"experimentID", choices = unique(initData$measurement$experiment))
  #   updateSelectInput(session,"treatmentID", choices = unique(initData$measurement$treatment))
  # })
  #
  # observeEvent(input$show,{
  #   session$sendCustomMessage(type="getTable","")
  # })
  #
  # observeEvent(input$showChanged,{
  #   modellOutputNames <- dataenv()
  #   print(input$showChanged)
  #   dat <- readRDS(file.path(baseDir(),"output/outputs.RDS"))
  #   tableForPlot <- jsonlite::fromJSON(input$outTable)
  #   runIdentifiers <- modellOutputNames[input$outputSelection_rows_selected]
  #   showModal(multiPlotUI(ns("plotka"))) 
  #
  #   callModule(multiPlot,"plotka",dat,reactive(initData$measurement),reactive({runIdentifiers}),reactive({tableForPlot}),
  #              reactive({input$experimentID}),reactive({input$treatmentID}),repetAvg = reactive({input$averagep}))
  # })
  #
  #   observeEvent(input$refresh,{
  #     dat[["dataenv"]] <-readRDS(file.path(baseDir(),"output/outputs.RDS"))
  #     modellOutputNames <- ls(dat$dataenv)
  #   updateCheckboxGroupInput(session,"outSelector",choices = modellOutputNames)
  # })
}
