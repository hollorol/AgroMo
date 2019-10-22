## tryScript <- "
##     $('.even').css('color','green');
## "
agroMoShowUI <- function(id){
  ns <- NS(id)
  tags$div(id = ns(id),
           tagList(
            column(4,
                   #tableOutput(ns("outputSelection"))
                   DT::dataTableOutput(ns("outputSelection"))
                   #tags$div(id=ns("outputSelection_container"),DT::dataTableOutput(ns("outputSelection")))
                   #DT::dataTableOutput(ns("outputSelection"), width = "200%")
                                        #tags$script(src="outputSelector.js") ## This js file generates a DataTable into the #showdiv-table-output_container div. See the sourcecode for further information.
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
                    actionButton(ns("export"),"EXPORT")
                                        )
           )
       )
}
agroMoShow <- function(input, output, session, dataenv, baseDir){
  ns <- session$ns
  ## dat <- new.env()
  ## dat[["dataenv"]] <-readRDS("output/outputs.RDS")
  ## modellOutputNames <- ls(dat$dataenv)
  datas<- reactiveValues(show=0)
  initData <- reactiveValues(data = NULL,measurement = NULL)
  observe({
    initData$data <- dataenv()
  })
    output$outputSelection <- renderDataTable({
      DT::datatable(data.frame(outputName = initData$data), options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = 600, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
    })

  observe({
    initData$measurement <- fread(file.path(baseDir(),"observation/observations.csv"))
    updateSelectInput(session,"experimentID", choices = unique(initData$measurement$experiment))
    updateSelectInput(session,"treatmentID", choices = unique(initData$measurement$treatment))
  })
 
  observeEvent(input$show,{
    session$sendCustomMessage(type="getTable","")
  })

  observeEvent(input$showChanged,{
    modellOutputNames <- dataenv()
    print(input$showChanged)
    dat <- readRDS(file.path(baseDir(),"output/outputs.RDS"))
    tableForPlot <- jsonlite::fromJSON(input$outTable)
    runIdentifiers <- modellOutputNames[input$outputSelection_rows_selected]
    showModal(multiPlotUI(ns("plotka"))) 

    callModule(multiPlot,"plotka",dat,reactive(initData$measurement),reactive({runIdentifiers}),reactive({tableForPlot}),
               reactive({input$experimentID}),reactive({input$treatmentID}),repetAvg = reactive({input$averagep}))
  })

    observeEvent(input$refresh,{
      dat[["dataenv"]] <-readRDS(file.path(baseDir(),"output/outputs.RDS"))
      modellOutputNames <- ls(dat$dataenv)
    updateCheckboxGroupInput(session,"outSelector",choices = modellOutputNames)
  })
}
