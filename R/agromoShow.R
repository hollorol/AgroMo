#' agroMoShowUI
#' 
#' agroMoShowUI
#' @importFrom shiny NS tagList tags column checkboxInput HTML actionButton dataTableOutput tableOutput renderDataTable
#' @importFrom DT renderDT DTOutput


agroMoShowUI <- function(id){
  ns <- NS(id)
  tags$div(id = ns(id),
           tagList(
            column(4,
                   tags$div(id=ns("outputSelection-container"),
                            tableOutput(ns("outputSelection"))),
                   dataTableOutput(ns("gridoutputSelection"))
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
                    tags$div(id=ns("compfunc_container"),selectInput(ns("compfunc"), "Compare function:",choices = c('difference','square error'))),
                    tags$div(id=ns("compbase_container"),selectInput(ns("compbase"), "Compare base:",choices = 'experiment')),
                    tags$div(id=ns("varset_container"),selectInput(ns("varset"), "filter to:",
                                        choices = c("all","user selected", "plant related","soil related","water related","carbon related","greenhouse gas","profiles"))),
                    

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


                   tags$script(HTML(
                                         " 
                                        Shiny.addCustomMessageHandler('hideHR',function(message){
                                        $(message).addClass('hidden');
                                        }
                                        )  
                                         " 
                                          )),
                   tags$script(HTML(
                                         " 
                                        Shiny.addCustomMessageHandler('showSelectionHR',function(message){
                                            $('.selected-rows_showdiv_table_output').removeClass('hidden');
                                            $('#showdiv-table-output>tbody>').not('.selected-rows_showdiv_table_output').addClass('hidden');
                                         }
                                        )  
                                         " 
                                          )),
                   tags$script(HTML(
                                         " $('#simres').on('click', function(){
                                               $('#showdiv-outputSelection td').removeClass('showdiv-selected-vars');
                                           })
                                         " 
                                          )),
                   tags$script(HTML( sprintf(
                                         "
                                         $('#%s').on('click','td', function(){
                                            $(this).toggleClass('showdiv-selected-vars');
                                            let selections = getIndexesForSelection('.showdiv-selected-vars');
                                            if(selections.length <= 5){
                                                Shiny.onInputChange('%s',selections);
                                            } else {
                                                $(this).toggleClass('showdiv-selected-vars');

                                            }
                                            // console.log(getIndexesForSelection('.showdiv-selected-vars'))
                                         })
                                        
                                         ",ns("outputSelection"),ns("tableList"))
                                          )),

                    tags$script(HTML(
                                         " 
                                        Shiny.addCustomMessageHandler('showHR',function(message){
                                        $(message).removeClass('hidden');
                                        }
                                        )  
                                         " 
                                          )),
                    ## This js file generates a DataTable into the #showdiv-table-output_container div. See the sourcecode for further information.
                    actionButton(ns("show"),"PLOT"),
                    actionButton(ns("export"),"EXPORT"),
                    actionButton(ns("del"),"DELETE SELECTED")
                                        )
           )
       )
}


#' agroMoShow
#' 
#' Bla
#' @importFrom shiny reactiveValues updateSelectInput NS tagList tags column checkboxInput HTML actionButton renderDataTable
#' @importFrom data.table fread
#' @importFrom DT renderDT
#' @importFrom DBI dbListTables dbGetQuery dbSendQuery
#' @importFrom shinyjs addClass removeClass


agroMoShow <- function(input, output, session, dataenv, baseDir, connection,centralData){
  ns <- session$ns
  ## dat <- new.env()
  ## dat[["dataenv"]] <-readRDS("output/outputs.RDS")
  ## modellOutputNames <- ls(dat$dataenv)
  datas<- reactiveValues(show=0)
  initData <- reactiveValues(data = NULL,measurement = NULL)
  observe({
     initData$data <- dataenv()
  })
  observe({
    output$outputSelection <- renderTable(data.frame(outputName=initData$data), width="100%", align="l")
  })

  observeEvent(input$del,{
                   tablesToDelete <- dbListTables(connection())[input$tableList]
                   # print(input$tableList)
                   if(length(tablesToDelete)!=0){
                       sapply(tablesToDelete,function(sqlTable){
                            dbSendQuery(connection(),sprintf("DROP TABLE IF EXISTS %s",sqlTable))   # It is sad that in SQLite DROP TABLE a,b,c; not working...  
                       })
                       initData$data <- setdiff(initData$data, tablesToDelete)
                   }
    })

  varSet <- list()
  #Defining set of variables
  varSet[["all"]] <- 0:75
  varSet[["plant related"]] <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 36, 37, 38, 39, 40, 41, 44, 45, 46, 47, 48, 49, 50, 51, 52)
  varSet[["soil related"]] <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 42, 43, 54, 55, 56, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68)
  varSet[["water related"]] <- c(26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 13, 14, 15)
  varSet[["carbon related"]] <- c(7, 8, 9,  36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 57, 58)
  varSet[["greenhouse gas"]]  <- c(69, 70, 40, 41, 42, 43, 44, 49, 50, 51, 52)
  varSet[["profiles"]]  <- 71:75


       observe({
      # print(input$varset)
      if(input$varset == "user selected"){
          session$sendCustomMessage(type="showSelectionHR","")
      } else {

       varsShow <- input$varset 
       session$sendCustomMessage(type="hideHR",
                   paste0(".",0:75,"-rowHR",collapse=",")
                                 )
       session$sendCustomMessage(type="showHR",
                   paste0(".",varSet[[input$varset]],"-rowHR",collapse=",")
                                 )
      }
      })

  
   observe({
     initData$measurement <- fread(file.path(baseDir(),"observation/observation.csv"))
     updateSelectInput(session,"experimentID", choices = unique(initData$measurement$experiment))
     updateSelectInput(session,"treatmentID", choices = unique(initData$measurement$treatment))
   })

  observe({
      if(length(input$tableList) > 0){
          updateSelectInput(session,"compbase",choices=c("none","experiment",initData$data[input$tableList]))
      } 
  })


  observeEvent(input$show,{
    session$sendCustomMessage(type="getTable","")
  })
  observeEvent(input$showChanged,{
      modellOutputNames <- initData$data[input$tableList]
      # print(input$showChanged)
      tableForPlot <- jsonlite::fromJSON(input$outTable)
      if(length(tableForPlot!=0) && (length(input$tableList)!=0)){
          showModal(multiPlotUI(ns("plotka"))) 
          # browser() 
          callModule(multiPlot,"plotka",reactive(initData$measurement),isolate(modellOutputNames),reactive({tableForPlot}),
              reactive({input$experimentID}),reactive({input$treatmentID}),repetAvg = reactive({input$averagep}),connection=connection,centralData=centralData)
      }
   })
}
