#' multiPlotUI
#'
#' multiPlotUI
#' @param id bla
multiPlotUI <- function(id){
   ## debug(multiPlot)
  ns <- NS(id)
  modalDialog(
    uiOutput(ns("plots")),
    uiOutput(ns("profilePlots")),
    size = "l",
    easyClose = TRUE
  )
}

#' multiPlot
#'
#' multiPlot
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom shiny renderUI
#' @importFrom DBI dbReadTable
#' @param input sdfasdf

multiPlot <- function(input, output, session, measurement, outputNames, outputTable, experimentID, treatmentID,repetAvg = TRUE,connection,centralData, measAlias){
  ns <- session$ns
  simplePlots <- outputTable()[grep("Profil",outputTable()$variable,invert = TRUE),] #TODO
  # browser()

  if(dim(simplePlots)[1]!=0){
      # browser()
    centralDataIndex <-   centralData[,"LABEL NAME"] %in% simplePlots[,"variable"]
    filteredCentData <- centralData[centralDataIndex,]
    simplePlots$variable <- filteredCentData[,"VARIABLE"]
    simplePlots <- cbind.data.frame(simplePlots,centralData[centralDataIndex,"CONV FACTOR"]) 
    simplePlots[,5]<- as.numeric(as.character(simplePlots[,5]))
  }
# browser()
  dataenv <- new.env()
  sapply(dbListTables(connection()),function(tableName){
   dataenv[[tableName]] <- dbReadTable(connection(),tableName)
   colnames(dataenv[[tableName]])[1:4] <- c("date","day","month","year") # browser()
  })

  ## browser()
  ## simplePlots <- simplePlots[simplePlots$select==TRUE,]
  profPlots <-centralData[centralData[,"LABEL NAME"]==grep("Profil",outputTable()$variable, value=TRUE),"VARIABLE"]
  # colnames(dataenv[[tableName]])[1:4] <- c("date","day","month","year")
  numProfile<- length(profPlots)
  numSimplePlots <- nrow(simplePlots)
  
# print(sprintf("Number of simple plots: %s",numSimplePlots))
  if(numSimplePlots != 0){
      output$plots <- renderUI({
          # print(simplePlots[,1])
          if(numSimplePlots!=0){
              plot_output_list <- lapply(simplePlots[,1],function(variab){
                                             plotlyOutput(ns(variab),height="600px")
           })
              do.call(tagList,plot_output_list)}
      })

      for(i in 1:numSimplePlots){
          # print(ls(dataenv))
          local({
              my_i <- i
              mesUnit <- ifelse(filteredCentData[i,4]=="NA","dimless",filteredCentData[i,4])
              yTitle <- sprintf("<b>%s [%s]</br> </b>",filteredCentData[i,2],mesUnit)
              output[[simplePlots[my_i,1]]] <- renderPlotly({
                   
                  # plotlyProxy(simplePlots[my_i,1], session) %>%
                  #     plotlyProxyInvoke("purge")
                  plotSingle(outputNames = outputNames,
                  dataenv = dataenv,
                  varName = simplePlots[my_i,1],
                  timeFrame = simplePlots[my_i,2],
                  groupFun = simplePlots[my_i,3],
                  plotT = simplePlots[my_i,4],
                  conversionFactor= simplePlots[my_i,5],
                  repetationsAveraged = repetAvg(),
                  measurement = measurement(),
                  experiment_id = experimentID(),
                  treatment = treatmentID(),yTitle,measAlias())})})
      }
  }


  if(numProfile != 0){
      output$profilePlots <- renderUI({
          if(numProfile!=0){ #it is necessary because outside the reactive environment the test is done just ones
              startDate <- dataenv[[names(dataenv)[1]]][nrow(dataenv[[names(dataenv)[1]]])%/%2,1]
              plot_output_list <- lapply(profPlots,function(x)(displayProfile(ns(x),startDate)))
              do.call(tagList,plot_output_list)}
      })

      for(i in 1:numProfile){
          # print(ls(dataenv))
          local({
              my_i <- i
      observeEvent(input[[sprintf("%s-ddec",profPlots[my_i])]],{
        updateDateInput(session, inputId = sprintf("%s-dateInput",profPlots[my_i]),
                        value = (input[[sprintf("%s-dateInput",profPlots[my_i])]]-1))
      })

      observeEvent(input[[sprintf("%s-dinc",profPlots[my_i])]],{
        updateDateInput(session, inputId = sprintf("%s-dateInput",profPlots[my_i]),
                        value = (input[[sprintf("%s-dateInput",profPlots[my_i])]]+1))
      })

      observeEvent(input[[sprintf("%s-wdec",profPlots[my_i])]],{
        updateDateInput(session, inputId = sprintf("%s-dateInput",profPlots[my_i]),
                        value = (input[[sprintf("%s-dateInput",profPlots[my_i])]]-7))
      })

      observeEvent(input[[sprintf("%s-winc",profPlots[my_i])]],{
        updateDateInput(session, inputId = sprintf("%s-dateInput",profPlots[my_i]),
                        value = (input[[sprintf("%s-dateInput",profPlots[my_i])]]+7))
      })

      observeEvent(input[[sprintf("%s-mdec",profPlots[my_i])]],{
        updateDateInput(session, inputId = sprintf("%s-dateInput",profPlots[my_i]),
                        value = addDate(input[[sprintf("%s-dateInput",profPlots[my_i])]],"-1m"))
      })

      observeEvent(input[[sprintf("%s-minc",profPlots[my_i])]],{
        updateDateInput(session, inputId = sprintf("%s-dateInput",profPlots[my_i]),
                        value = addDate(input[[sprintf("%s-dateInput",profPlots[my_i])]],"1m"))
      })

      observeEvent(input[[sprintf("%s-ydec",profPlots[my_i])]],{
        updateDateInput(session, inputId = sprintf("%s-dateInput",profPlots[my_i]),
                        value = addDate(input[[sprintf("%s-dateInput",profPlots[my_i])]],"-1y"))
      })

      observeEvent(input[[sprintf("%s-yinc",profPlots[my_i])]],{
        updateDateInput(session, inputId = sprintf("%s-dateInput",profPlots[my_i]),
                        value = addDate(input[[sprintf("%s-dateInput",profPlots[my_i])]],"1y"))
      })

      output[[profPlots[my_i]]] <-  renderPlotly({plotProfile(outputNames,
                                                              dataenv = dataenv,
                                                            selectedDate = input[[sprintf("%s-dateInput",profPlots[my_i])]],
                                                            profilTag=profPlots[my_i],
                                         as.numeric(c(input[[sprintf("%s-xmin",profPlots[my_i])]],input[[sprintf("%s-xmax",profPlots[my_i])]])),
                                         as.numeric(c(input[[sprintf("%s-ymax",profPlots[my_i])]],0))
                                                            )})
            
          })
      }
  }
#   if(numProfile > 0){
#     output$profilePlots <- renderUI({
#         if(numProfile!=0){
#           tagList(
#             dateInput(ns("dateInput"),"date","2000-01-01"),actionButton(ns("ddec"),"day - 1"), actionButton(ns("dinc"),"day + 1"),
#             plotlyOutput(ns("tsoil")),plotlyOutput(ns("vwc")))
#         }
#       })
#
#       observeEvent(input$ddec,{
#         updateDateInput(session, inputId = "dateInput", value = (input$dateInput-1))
#       })
#       observeEvent(input$dinc,{
#         updateDateInput(session, inputId = "dateInput", value = (input$dateInput+1))
#       })
#
#       ## plotProfile <- function(outputNames,dataenv=readRDS("output/outputs.RDS"),selectedDate,profileName = "vwc-profile"){
#       if(numProfile!=0){
#         grepvwc <- grep("SWC",profPlots[,1])
#         greptsoil <- grep("T",profPlots[,1])
#         if(length(grepvwc)!=0){
#           output$vwc <-  renderPlotly({plotProfile(outputNames, dataenv = dataenv, selectedDate = input$dateInput, profileName = "SWC profile")})
#         }
#         if(length(greptsoil)!=0){
#           output$tsoil <- renderPlotly({plotProfile(outputNames, dataenv = dataenv, selectedDate = input$dateInput, profileName = "T profile")})
#         }
#
#       }
# }
}



#' displayProfile
#'
#' display profile graphs
#' @param profName the name of the curent profilePlot
#' @keywords internal
displayProfile <- function (profName,startDate) {
    tags$div(id=sprintf("%s-container",profName),
             tags$div(id=sprintf("%s-bigholder",profName), class="bigholder",
                      tags$div(id=sprintf("%s-minx",profName),class="xyRangeInput", style="padding-left: 100px" , textInput(sprintf("%s-xmin",profName),"xmin:")),
                      tags$div(id=sprintf("%s-handlers",profName), class="profPlotCont",
                               actionButton(sprintf("%s-ydec",profName),"-y"),
                               actionButton(sprintf("%s-mdec",profName),"-m"),
                               actionButton(sprintf("%s-wdec",profName),"-w"),
                               actionButton(sprintf("%s-ddec",profName),"-d"),
                               dateInput(sprintf("%s-dateInput", profName),"date",startDate),
                               actionButton(sprintf("%s-dinc",profName),"+d"),
                               actionButton(sprintf("%s-winc",profName),"+w"),
                               actionButton(sprintf("%s-minc",profName),"+m"),
                               actionButton(sprintf("%s-yinc",profName),"+y")
                               ),
                      tags$div(id=sprintf("%s-maxx",profName),class="xyRangeInput",style="padding-right: 35px",textInput(sprintf("%s-xmax",profName),"xmax:"))
                      ),
             tags$div(id=sprintf("%s-axis",profName), class="profAxis",
                      tags$div(id=sprintf("%s-ymax",profName),class="xyRangeInput", textInput(sprintf("%s-ymax",profName),"ymax:"),style="padding-left:100px")
                      #textInput(sprintf("%s-xmin",profName),"xmin:"),style=("position: absolute; top: 38px; left: 40px; color: red;"),
                      #textInput(sprintf("%s-xmax",profName),"xmax:"),style=("position: absolute; top: 38px; left: 800px; color: green;"),
                      #textInput(sprintf("%s-ymax",profName),"ymax:"),style=("color:blue;")

                      ),
             plotlyOutput(profName,height="600px"))
}

addDate <- function(dateString, addtag){
    if(is.character(addtag)){
        myDate <- as.POSIXlt(dateString)
        if(grepl("-?[0-9]+m",addtag)){
            myDate$mon <- myDate$mon + as.numeric(gsub("(-?[0-9]+).*","\\1",addtag,perl=TRUE))
        } else {
            myDate$year <- myDate$year + as.numeric(gsub("(-?[0-9]+).*","\\1",addtag,perl=TRUE))
        }
        as.Date(myDate)
    } else {
        as.Date(dateString)+addtag
    }
}





