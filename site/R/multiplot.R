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

multiPlot <- function(input, output, session, dataenv, measurement, outputNames, outputTable, experimentID, treatmentID,repetAvg = TRUE){
  ns <- session$ns
  simplePlots <- cbind.data.frame(outputTable(),c(rep(1,3),10000,rep(1,24)))[1:26,]
  simplePlots <- simplePlots[simplePlots$select==TRUE,]
  profPlots <- outputTable()[c(27,28),]
  profPlots <- profPlots[profPlots$select == TRUE,]
  numProfile<- nrow(profPlots)
  numSimplePlots <- nrow(simplePlots)

  output$plots <- renderUI({
     print(simplePlots[,1])
     if(numSimplePlots!=0){
      plot_output_list <- lapply(simplePlots[,1],function(variab){
         plotlyOutput(ns(variab))
       })
       return(do.call(tagList,plot_output_list))}
   })
#plotSingle(outputNames, dataenv, "fruit_DM","year","max",plotT="line",10000,repetationsAveraged=TRUE, measurement,experimentID,treatment)
    for(i in 1:numSimplePlots){
      local({
        my_i <- i
        output[[simplePlots[my_i,1]]] <- renderPlotly({plotSingle(outputNames = outputNames(),
                                                 dataenv = dataenv,
                                                 varName = simplePlots[my_i,1],
                                                 timeFrame = simplePlots[my_i,3],
                                                 groupFun = simplePlots[my_i,4],
                                                 plotT = simplePlots[my_i,5],
                                                 conversionFactor= simplePlots[my_i,6],
                                                 repetationsAveraged = repetAvg(),
                                                 measurement = measurement(),
                                                 experiment_id = experimentID(),
                                                 treatment = treatmentID())})})
    }
  output$profilePlots <- renderUI({
    if(numProfile!=0){
      tagList(
        dateInput(ns("dateInput"),"date","2000-01-01"),actionButton(ns("ddec"),"day - 1"), actionButton(ns("dinc"),"day + 1"),
        plotlyOutput(ns("tsoil")),plotlyOutput(ns("vwc")))
    }
  })

  observeEvent(input$ddec,{
    updateDateInput(session, inputId = "dateInput", value = (input$dateInput-1))
  })
  observeEvent(input$dinc,{
    updateDateInput(session, inputId = "dateInput", value = (input$dateInput+1))
  })

  ## plotProfile <- function(outputNames,dataenv=readRDS("output/outputs.RDS"),selectedDate,profileName = "vwc-profile"){
  if(numProfile != 0){
    grepvwc <- grep("vwc",profPlots[,1])
    greptsoil <- grep("tsoil",profPlots[,1])
    if(length(grepvwc)!=0){
    output$vwc <-  renderPlotly({plotProfile(outputNames(), dataenv = dataenv, selectedDate = input$dateInput, profileName = "vwc-profile")})
    }
    if(length(greptsoil)!=0){
     output$tsoil <- renderPlotly({plotProfile(outputNames(), dataenv = dataenv, selectedDate = input$dateInput, profileName = "tsoil-profile")})
    }

  }
}
