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

variableMap <- new.env()
apply(data.frame(
  'longNames'= c("leaf dry matter","leaf dry matter in litter","fine root dry matter","yield","soft stem dry matter","projected lai","cum. evapotranspiration","cum_trans","rooting_depth","daily_gpp","daily_tr","daily_nee","tsoil_0","tsoil_1","tsoil_2","tsoil_3","tsoil_4","tsoil_5","tsoil_6","vwc_0","vwc_1","vwc_2","vwc_3","vwc_4","vwc_5","vwc_6"),
  'shortNames'= c("leaf_DM", "leaflitr_DM", "froot_DM", "fruit_DM", "softstem_DM", "proj_lai", "cum_evap", "cum_trans", "rooting_depth", "daily_gpp", "daily_tr", "daily_nee", "tsoil_0", "tsoil_1", "tsoil_2", "tsoil_3", "tsoil_4", "tsoil_5", "tsoil_6", "vwc_0", "vwc_1", "vwc_2", "vwc_3", "vwc_4", "vwc_5", "vwc_6")
),1,
      function(row){
        variableMap[[row[1]]] <- row[2] 
      })

conversionMap <- new.env()

apply(data.frame(
  longNames = c("leaf dry matter","leaf dry matter in litter","fine root dry matter","yield","soft stem dry matter","projected lai","cum. evapotranspiration","cum_trans","rooting_depth","daily_gpp","daily_tr","daily_nee","tsoil_0","tsoil_1","tsoil_2","tsoil_3","tsoil_4","tsoil_5","tsoil_6","vwc_0","vwc_1","vwc_2","vwc_3","vwc_4","vwc_5","vwc_6"),
  convFactors = c(rep(1,3),10000,rep(1,22)),
  stringsAsFactors = FALSE
),1,
function(row){
  conversionMap[[row[1]]] <- row[2] 
})

multiPlot <- function(input, output, session, dataenv, measurement, outputNames, outputTable, experimentID, treatmentID,repetAvg = TRUE){
  ns <- session$ns
  simplePlots <- outputTable()[grep("prof",outputTable()$variable,invert = TRUE),]
  if(dim(simplePlots)[1]!=0){
    ## browser()
    simplePlots <- cbind.data.frame(simplePlots,sapply(simplePlots[,"variable"],function(variable){
      convFactors <- as.numeric(tryCatch(conversionMap[[variable]],error = function(e){
        stop(sprintf("%s not found in variable list",variable))
      })
      )  }))

    simplePlots[,"variable"] <- sapply(simplePlots[,"variable"],function(variable){
      convFactors <- tryCatch(variableMap[[variable]],error = function(e){
        stop(sprintf("%s not found in variable list",variable))
      })
    })
  }

  ## browser()
  ## simplePlots <- simplePlots[simplePlots$select==TRUE,]
  profPlots <- outputTable()[grep("prof",outputTable()$variable),]
  numProfile<- nrow(profPlots)
  numSimplePlots <- nrow(simplePlots)
  
print(sprintf("Number of simple plots: %s",numSimplePlots))
  if(numSimplePlots != 0){
    output$plots <- renderUI({
      print(simplePlots[,1])
      if(numSimplePlots!=0){
        plot_output_list <- lapply(simplePlots[,1],function(variab){
          plotlyOutput(ns(variab))
        })
        return(do.call(tagList,plot_output_list))}
      return(plotlyOutput(ns("csacsi")))
    })
  }
  ## browser()
#plotSingle(outputNames, dataenv, "fruit_DM","year","max",plotT="line",10000,repetationsAveraged=TRUE, measurement,experimentID,treatment)
  print(simplePlots)
  print(numSimplePlots)
  if(numSimplePlots != 0)(
      for(i in 1:numSimplePlots){
        local({
          my_i <- i
          ## browser()
          output[[simplePlots[my_i,1]]] <- renderPlotly({plotSingle(outputNames = outputNames(),
                                                                    dataenv = dataenv,
                                                                    varName = simplePlots[my_i,1],
                                                                    timeFrame = simplePlots[my_i,2],
                                                                    groupFun = simplePlots[my_i,3],
                                                                    plotT = simplePlots[my_i,4],
                                                                    conversionFactor= simplePlots[my_i,5],
                                                                    repetationsAveraged = repetAvg(),
                                                                    measurement = measurement(),
                                                                    experiment_id = experimentID(),
                                                                    treatment = treatmentID())})})
      }
    )
  if(numProfile > 0){
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
      if(numProfile!=0){
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
}
