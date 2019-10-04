## tryScript <- "
##     $('.even').css('color','green');
## "
agroMoShowUI <- function(id){
  ns <- NS(id)
  tags$div(id = ns(id),
           tagList(
            column(4,
                   #tableOutput(ns("outputSelection"))
                   DT::dataTableOutput(ns("outputSelection")),
                   DT::dataTableOutput(ns("gridoutputSelection"))
                   #tags$div(id=ns("outputSelection_container"),DT::dataTableOutput(ns("outputSelection")))
                   #DT::dataTableOutput(ns("outputSelection"), width = "200%")
                                        #tags$script(src="outputSelector.js") ## This js file generates a DataTable into the #showdiv-table-output_container div. See the sourcecode for further information.
                   ),
             column(8,
                    tags$div(id="observations","OBSERVATIONS:"),
                    tags$div(id="simres","SIMULATION RESULTS:"),
                    tags$div(id="createplot","CREATE PLOT WITH:"),
                    tags$div(id="repavg","Repetitions averaged"),
                    tags$div(id="gridsimres","GRID SIMULATION RESULTS:"),
                    tags$div(
                      id = paste0(ns("cellid"),"_container"),
                      textInput(ns("cellid"), "CELL ID(s):", '222, 777, 1028')
                    ),
                    tags$div(id=ns("experimentID_container"),selectInput(ns("experimentID"), "EXPERIMENT ID:",choices = 'NILL')),
                    tags$div(id=ns("treatmentID_container"),selectInput(ns("treatmentID"), "TREATMENT ID:",choices = 'NILL')),
                    tags$div(id=ns("compfunc_container"),selectInput(ns("compfunc"), "Compare function:",choices = 'NILL')),
                    tags$div(id=ns("compbase_container"),selectInput(ns("compbase"), "Compare base:",choices = 'NILL')),
                    tags$div(id=ns("varset_container"),selectInput(ns("varset"), "variable set:",choices = 'NILL')),
                    checkboxInput(ns("averagep"),"", value = TRUE),
                    #checkboxInput(ns("averagep"),"Repetitions averaged", value = TRUE),
                    #checkboxGroupInput(ns("createPlot"),label = "CREATE PLOT WITH:",choices = NULL),
                    tags$div(id=ns("table-header_container")), 
                    tags$div(id=ns("table-output_container")), 
                    tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')),
                    tags$script(src="showTableOutput.js"), ## This js file generates a DataTable into the #showdiv-table-output_container div. See the sourcecode for further information.
                    tags$script(src="outputSelector.js"), ## This js file generates a DataTable into the #showdiv-table-output_container div. See the sourcecode for further information.

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
                    actionButton(ns("export"),"EXPORT"),
                    actionButton(ns("del"),"DELETE SELECTED")
                                        )
           )
       )
}
agroMoShow <- function(input, output, session, dataenv){
  ns <- session$ns
  ## dat <- new.env()
  ## dat[["dataenv"]] <-readRDS("output/outputs.RDS")
  ## modellOutputNames <- ls(dat$dataenv)
  measurement <- fread("observations/observations.csv")
  datas<- reactiveValues(show=0)
  initData <- reactiveValues(data = NULL)
  observe({
    initData$data <- dataenv()
  })
  ## updateCheckboxGroupInput(session,"outSelector",choices = modellOutputNames)

  ## session$onFlushed(function() {
  ##   session$sendCustomMessage(type='jsCode', list(value = tryScript))
  ## })
    output$outputSelection <- renderDataTable({
      DT::datatable(data.frame(outputName = initData$data), options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = 300, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
      #DT::datatable(data.frame(outputName = initData$data), options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = 300, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE)) 
    })

    output$gridoutputSelection <- renderDataTable({
      DT::datatable(data.frame(outputName = ''), options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = 100, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
    })
    
   #DT::datatable(data.frame(outputName = modellOutputNames), options = list(autowidth = TRUE, paginate = FALSE, scrollY = 600, scrollX = FALSE, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
  #dataTableOutput(session,"outSelector",choices = modellOutputNames)
  updateSelectInput(session,"experimentID", choices = unique(measurement$experiment))
  updateSelectInput(session,"treatmentID", choices = unique(measurement$treatment))
  ## dataTable <- callModule(graphControl,"mainControl",reactive({input$show}))
  
  observeEvent(input$show,{
    session$sendCustomMessage(type="getTable","")
  })

  observeEvent(input$showChanged,{
    modellOutputNames <- dataenv()
    print(input$showChanged)
    dat <- readRDS("output/outputs.RDS")
    tableForPlot <- jsonlite::fromJSON(input$outTable)
    ## print(tableForPlot)
    runIdentifiers <- modellOutputNames[input$outputSelection_rows_selected]
    ## print(runIdentifiers)
    showModal(multiPlotUI(ns("plotka"))) 
    callModule(multiPlot,"plotka",dat,reactive({measurement}),reactive({runIdentifiers}),reactive({tableForPlot}),
               reactive({input$experimentID}),reactive({input$treatmentID}),repetAvg = reactive({input$averagep}))
  })

    observeEvent(input$refresh,{
      dat[["dataenv"]] <-readRDS("output/outputs.RDS")
      modellOutputNames <- ls(dat$dataenv)
    updateCheckboxGroupInput(session,"outSelector",choices = modellOutputNames)
  })
}
