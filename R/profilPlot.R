plotProfile <- function(outputNames, dataenv, selectedDate, profilTag){
     numberOfLayers <- length(outputNames)
     valuesToPlot <- with(getProfileVariables(profilTag),lapply(outputNames, function(x){
                                                    unlist(dataenv[[x]][dataenv[[x]][,"date"]==selectedDate,data]*convFactor)}))
     depths <- -c(1.5,6.5,25,45,75,105,135,175,300,700)
     p <- plot_ly()
     p <- add_trace(p, x= valuesToPlot[[1]][1:8], y= depths[1:8], mode = "lines", type = "scatter", name = outputNames[1]) %>%
         layout(yaxis = list(zeroline=FALSE), xaxis = list(side = "top"))
     if(numberOfLayers>=2){
         for(i in 2:numberOfLayers){
             p <- add_trace(p, x= valuesToPlot[[i]][1:8], y= depths[1:8], mode = "lines", type = "scatter",name = outputNames[i]) 
         }
     }
    #
    # if(profilePretag == "tsoil"){
    #     return(p %>% layout(xaxis = list(range = c(0,20), title = "Soil temperature profile [°C]"),yaxis = list(title = "depths[cm]")))
    # } else {
    #     return(p %>% layout(xaxis = list(range = c(0,50), title = "Soil water content [V%]"), yaxis = list(title = "depths[cm]")))
    # }
    # p %>% layout(title=getTitleFromCentralData(variable=profilTag))
   
     # Margins:
     m <- list(
         l = 30,
         r = 30,
         b = 60,
         t = 120,
         pad = 4
     )
     
     # Parameters of the titles:
     titlefont <- list(
         family = "Trebuchet MS",
         size = 26,
         color = "black"
     )
     
     # Parameters of the axis labels:
     tickfont <- list(
         family = "Trebuchet MS",
         size = 22,
         color = "black" # to set scientific format use the parameter: exponentformat = "E"
     )
     
      p %>% layout(title=getTitleFromCentralData(variable=profilTag),
                 xaxis = list(ticks = "outside",
                              range = c(0,45),
                              ticklen = 10,
                              tickwidth = 2,
                              tickcolor = toRGB("grey40"),
                              showticklabels = TRUE,
                              tickangle = 0,
                              tickfont = tickfont,
                              # zeroline = TRUE, # to highlight the line at x=0
                              # zerolinecolor = toRGB("red"),
                              # zerolinewidth = 2,
                              gridcolor = toRGB("grey80"),
                              gridwidth = 1,
                              mirror = "ticks", # to get lines around the plot
                              linecolor = toRGB("grey40"),
                              linewidth = 2),
                   
                 yaxis = list(title = "<b>Depth [cm]</b>", # bold title
                              titlefont = titlefont,
                              ticks = "outside",
                              ticklen = 10,
                              tickwidth = 2,
                              tickcolor = toRGB("grey40"),
                              showticklabels = TRUE,
                              tickangle = 0,
                              tickfont = tickfont,
                              gridcolor = toRGB("grey80"),
                              gridwidth = 1,
                              mirror = "ticks", # to get borders around the plot
                              linecolor = toRGB("grey40"),
                              linewidth = 2),
                 margin = m)
}

#' getProfileVariables
#'
#' Get profile variables of a given profiletag
#' @param tag The profile tag
#' @keywords internal
getProfileVariables <- function(tag,centralData=getOption("AgroMo_centralData")){
    list(data=centralData[unlist(lapply(centralData[,"TAG"],function(x) {grepl(sprintf(".*%s.*",tag),x)})),"VARIABLE"],
          convFactor=as.numeric(centralData[centralData[,"VARIABLE"]==tag,"CONV FACTOR"])        
    )
}

# #' getProfileTags
# #'
# #' Get profile variables of a given profiletag
# #' @param outputTable is an outputTable
# #' @keywords internal
# getProfileVariables <- function(outputTable,centralData=getOption("AgroMo_centralData")){
#     tryCatch(centralData[unlist(lapply(centralData[,"TAG"],function(x) {grepl(sprintf(".*%s.*",tag),x)})),"VARIABLE"],
#     error=browser())
# }

getTitleFromCentralData <- function(labelName=NULL,variable=NULL,centralData=getOption("AgroMo_centralData")){
    if(!is.null(labelName)){
        rowIndex <- which(centralData[,"LABEL NAME"]==labelName)
        return(sprintf("%s [%s]",labelName,centralData[rowIndex,"TO"]))
    } else {
        rowIndex <-  which(centralData[,"VARIABLE"]==variable)
        return(sprintf("%s [%s]",centralData[rowIndex,"LABEL NAME"],centralData[rowIndex,"TO"]))
    }
}

#  getProfileVariables("water-profil")
# getTitleFromCentralData(variable="water-profil",centralData=centralData)
