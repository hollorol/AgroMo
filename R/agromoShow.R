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
                    tags$div(id="repavg","repetitions averaged"),
                    tags$div(id="gridsimres","GRID SIMULATION RESULTS:"),
                    tags$div(
                      id = paste0(ns("cellid"),"_container"),
                      textInput(ns("cellid"), "cell ID(s):")
                    ),
                    tags$div(id=ns("experimentID_container"),selectInput(ns("experimentID"), "experiment ID:",choices = '')),
                    tags$div(id=ns("treatmentID_container"),selectInput(ns("treatmentID"), "treatment ID:",choices = '')),
                    tags$div(id=ns("compfunc_container"),selectInput(ns("compfunc"), "compare function:",choices = '')),
                    tags$div(id=ns("compbase_container"),selectInput(ns("compbase"), "compare base:",choices = '')),
                    tags$div(id=ns("alias_container"),textInput(ns("alias"), "alias:",NA)),
                    tags$div(id=ns("varset_container"),title="Narrow down the list of selectable variables ",selectInput(ns("varset"), "filter to:",
                                        choices = c("all","user selected", "plant related","soil related","water related","carbon related","greenhouse gas","profiles"))),
                    

                    checkboxInput(ns("averagep"),"", value = TRUE),
                    tags$div(id=ns("table-header_container")),
                    tags$div(id=ns("table-output_container")),
                    tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')),
                    drawOutputTable(),
                    # tags$script(src="www/showTableOutput.js"),

                    tags$script(HTML(
                    sprintf("
                            Shiny.addCustomMessageHandler('getTable', function(message) {
                             Shiny.onInputChange('%s',JSON.stringify(getJSONFromDataTable()));
                             Shiny.setInputValue('%s',Math.random());
                        //console.log(getJSONFromDataTable());
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
                                            let selections = getIndexesForSelection('#%s','.showdiv-selected-vars');
                                            if(selections.length <= 5){
                                                Shiny.onInputChange('%s',selections);
                                            } else {
                                                $(this).toggleClass('showdiv-selected-vars');

                                            }
                                            // console.log(getIndexesForSelection('.showdiv-selected-vars'))
                                         })
                                        
                                         ",ns("outputSelection"),ns("outputSelection"),ns("tableList"))
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
                    actionButton(ns("show"),"CREATE PLOT"),
                    actionButton(ns("export"),"EXPORT", title="Export plot data into an Excel file"),
                    actionButton(ns("del"),"DELETE SELECTED", title="Delet selected simulation results from the list")
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
  datas<- reactiveValues(show=0)
  initData <- reactiveValues(data = NULL,measurement = NULL, measurementConn = NULL)
  observe({
     initData$data <- dataenv()
  })
  observe({
     initData$measurementConn <-  dbConnect(RSQLite::SQLite(),file.path(baseDir(), "database/observation.db"))
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

  varSet <- readTags()

  observe({
      updateSelectInput(session,"varset",choices=rev(names(varSet)))
  })

       observe({
      # print(input$varset)
      if(input$varset == "user selected"){
          session$sendCustomMessage(type="showSelectionHR","")
      } else {

       varsShow <- input$varset 
       session$sendCustomMessage(type="hideHR",
                   paste0(".",varSet[["all"]],"-rowHR",collapse=",")
                                 )
       session$sendCustomMessage(type="showHR",
                   paste0(".",varSet[[input$varset]],"-rowHR",collapse=",")
                                 )
      }
      })


     
   observe({
       updateSelectInput(session,"experimentID", choices = c("NO OBSERVED DATA",dbGetQuery(initData$measurementConn,"
                                                                       SELECT DISTINCT experiment FROM site 
                                                                       ")[,1])
     )

   })

     observe({
         if(input$experimentID!="NO OBSERVED DATA"){
          updateSelectInput(session,"treatmentID", choices =
                            dbGetQuery(initData$measurementConn,sprintf("
                                                                         SELECT DISTINCT SUBSTR(key,6,LENGTH(key))
                                                                         FROM site
                                                                         WHERE experiment='%s' AND value!='NA'
                                                                         ", input$experimentID))[,1])
                                          }
      })

  observe({
      if(length(input$tableList) > 0){
          updateSelectInput(session,"compbase",choices='')
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
          if(is.null(input$alias)){
                measAlias <- ""
          } else {
                measAlias <- input$alias
          }
          callModule(multiPlot,"plotka",reactive(initData$measurementConn),isolate(modellOutputNames),reactive({tableForPlot}),
              reactive({input$experimentID}),reactive({input$treatmentID}),repetAvg = reactive({input$averagep}),connection=connection,centralData=centralData,reactive({measAlias}))
      }
   })
}

#' drawOutputTable
#' 
#' drawOutputTable
#' @importFrom shiny tags HTML
#' @importFrom jsonlite read_json toJSON
drawOutputTable <- function () {
   centralData <- getOption("AgroMo_centralData") 
   colnames(centralData)[c(2, 7, 8,9)] <- c("VARIABLE","T-STEP","FUNC","PLOT TYPE")

   jsonFile <- toJSON(centralData[c(2,7,8,9)])
   tags$script(HTML(sprintf("var musoVariablesToPlot = %s;

                 putObjectAsTable(musoVariablesToPlot,\"#showdiv-table-output_container\",\"showdiv-table-output\",\"showdiv-table-output-header\",\"#showdiv-table-header_container\");

                                  var columnOptions = [[\"day\",\"month\",\"year\",\"decade\"],
                                                        [\"identity\",\"var\",\"min\",\"max\",\"mean\",\"median\",\"modus\"],
                                                         [\"bar\",\"line\",\"scatter\"]];

                                                          DT(\"#showdiv-table-output\", \"selected-rows_showdiv_table_output\", columnOptions, \"#showdiv-table-header_container th:nth-child(1)\");

                                                             ",jsonFile)))
}


#' readTags
#' 
#' readTags
#' @importFrom shiny tags HTML
#' @importFrom jsonlite read_json toJSON
readTags <- function () {
   centralData <- getOption("AgroMo_centralData")
   uniqFactors <- unique(strsplit(paste(centralData[,"TAG"],collapse=","),split=",\\ *")[[1]])
   uniqFactors <- grep(".*\\-.*",uniqFactors, value=TRUE, invert=TRUE) # We dont wanna include uniq profile variables
   varSet <- lapply(uniqFactors, function(x){
        which(unlist(lapply(centralData[,"TAG"],function(y){grepl(x,y) && !grepl(".*\\-.*",y)}))) -1
   })  
   names(varSet) <- uniqFactors
   varSet[["all"]] <- seq_along(centralData[,"TAG"]) -1
   varSet
}

#' filterToProf
#' 
#' filterToProf
#' @importFrom shiny tags HTML
#' @importFrom jsonlite read_json toJSON
filterToProf <- function (profileName) {
    centralData <- getOption("AgroMo_centralData")
    keyword <- centralData[centralData[,"LABEL NAME"] == profileName,"VARIABLE"]
    rowIndexes <- which(unlist(lapply(centralData[,"TAG"],function(x){grepl(keyword,x)})))
    rowIndexes
}
