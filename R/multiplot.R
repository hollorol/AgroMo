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

multiPlot <- function(input, output, session, measurement, outputNames, outputTable, experimentID, treatmentID,repetAvg = TRUE,connection,centralData){
  ns <- session$ns
  simplePlots <- outputTable()[grep("profile",outputTable()$variable,invert = TRUE),]
  # browser()

  if(dim(simplePlots)[1]!=0){
    centralDataIndex <-   centralData[,"LABEL NAME"] %in% simplePlots[,"variable"]
    filteredCentData <- centralData[centralDataIndex,]
    simplePlots$variable <- filteredCentData[,"VARIABLE"]
    simplePlots <- cbind.data.frame(simplePlots,centralData[centralDataIndex,"convFactor"]) 
    simplePlots[,5]<- as.numeric(as.character(simplePlots[,5]))
  }
# browser()
  dataenv <- new.env()
  sapply(dbListTables(connection()),function(tableName){
   dataenv[[tableName]] <- dbReadTable(connection(),tableName)
   colnames(dataenv[[tableName]])[1:4] <- c("date","day","month","year")
  # browser()
  })

  ## browser()
  ## simplePlots <- simplePlots[simplePlots$select==TRUE,]
  profPlots <- outputTable()[grep("profile",outputTable()$variable),]
  # colnames(dataenv[[tableName]])[1:4] <- c("date","day","month","year")
  numProfile<- nrow(profPlots)
  numSimplePlots <- nrow(simplePlots)
  
# print(sprintf("Number of simple plots: %s",numSimplePlots))
  if(numSimplePlots != 0){
    output$plots <- renderUI({
      # print(simplePlots[,1])
      if(numSimplePlots!=0){
        plot_output_list <- lapply(simplePlots[,1],function(variab){
          plotlyOutput(ns(variab),height="600px")
        })
        return(do.call(tagList,plot_output_list))}
      return(plotlyOutput(ns("csacsi")))
    })
  }

  ## browser()
#plotSingle(outputNames, dataenv, "fruit_DM","year","max",plotT="line",10000,repetationsAveraged=TRUE, measurement,experimentID,treatment)
  # print(simplePlots)
  # print(numSimplePlots)
  if(numSimplePlots != 0)(
      for(i in 1:numSimplePlots){
          # print(ls(dataenv))
        local({
          my_i <- i
          mesUnit <- ifelse(filteredCentData[i,4]=="NA","dimless",filteredCentData[i,4])
          yTitle <- sprintf("<b>%s [%s]</br> </b>",filteredCentData[i,2],mesUnit)
          output[[simplePlots[my_i,1]]] <- renderPlotly({plotSingle(outputNames = outputNames,
                                                                    dataenv = dataenv,
                                                                    varName = simplePlots[my_i,1],
                                                                    timeFrame = simplePlots[my_i,2],
                                                                    groupFun = simplePlots[my_i,3],
                                                                    plotT = simplePlots[my_i,4],
                                                                    conversionFactor= simplePlots[my_i,5],
                                                                    repetationsAveraged = repetAvg(),
                                                                    measurement = measurement(),
                                                                    experiment_id = experimentID(),
                                                                    treatment = treatmentID(),yTitle)})})
      }
    )
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
