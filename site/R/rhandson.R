
graphControlUI <- function(id){
  ns <- NS(id)
  tagList(
  rHandsontableOutput(ns("tablein")))
  
}

graphControl <- function(input, output, session, gomb){
  reactiveInput<- reactiveValues()
  VarNames <- c("leaf_DM","leaflitr_DM","froot_DM",
                "fruit_DM","softstem_DM","proj_lai",
                "cum_evap","cum_trans","rooting_depth",
                "daily_gpp","daily_tr", "daily_nee",
                "tsoil_0","tsoil_1", "tsoil_2", "tsoil_3",
                "tsoil_4","tsoil_5","tsoil_6","vwc_0",
                "vwc_1","vwc_2","vwc_3","vwc_4",
                "vwc_5","vwc_6","tsoil-profil","vwc-profil")
  timeFrame <- c(rep("day",26),"none","none")
  groupFun <- rep("identity",28)
  plotType <- rep("line",28)
  DF <- data.frame(VarNames=VarNames,select = rep(FALSE,28), timeFrame=timeFrame,groupFun=groupFun,
                   plotType=plotType,stringsAsFactors = FALSE)
  output$tablein <- renderRHandsontable({
    rhandsontable(DF, rowHeaders = FALSE, width = 550, height = 300)%>%
      hot_col(col = "timeFrame", type = "dropdown", source = c("day","mont","year"))%>%
      hot_col(col = "groupFun", type = "dropdown", source = c("identity","min","max","median","mean","var"))%>%
      hot_col(col = "plotType", type = "dropdown", source = c("line","bar","scatter")) %>%
      hot_col(col = "VarNames",readOnly = TRUE)
    
  })
  observeEvent(gomb(),{
    if(!is.null(input$tablein)){
     csami <- hot_to_r(input$tablein)
     if(!is.null(csami)){
       reactiveInput[["data"]] <- hot_to_r(input$tablein)
     }
    }
  })
  
  return(reactiveInput)

  

}
