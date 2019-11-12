plotProfile <- function(outputNames,dataenv=readRDS("output/outputs.RDS"),selectedDate,profileName = "vwc-profile"){
  profilePretag <- gsub("-.*","",profileName)
  numberOfLayers <- length(outputNames)
  data <- as.data.frame(dataenv[[outputNames[1]]])
  prof <- data[,c(1,grep(sprintf("%s_[0-5]",profilePretag),colnames(data)))]
  valuesToPlot <- as.numeric(prof[prof[,1] == as.Date(selectedDate),][2:5])
  if(profilePretag == "vwc"){
      valuesToPlot <- valuesToPlot * 100 # temporary hardcode...
  }
  depths <- -c(1.5,6.5,20,45)
  p <- plot_ly()
  p <- add_trace(p, x= valuesToPlot, y= depths, mode = "lines", type = "scatter", name = outputNames[[1]]) %>%
    layout(yaxis = list(zeroline=FALSE), xaxis = list(side = "top"))
  if(numberOfLayers>=2){
    for(i in 2:numberOfLayers){
      data <- as.data.frame(dataenv[[outputNames[i]]])
      prof <- data[,c(1,grep(sprintf("%s_[0-5]",profilePretag),colnames(data)))]
      valuesToPlot <- as.numeric(prof[prof[,1] == as.Date(selectedDate),][2:5])
      if(profilePretag == "vwc"){
          valuesToPlot <- valuesToPlot * 100 # temporary hardcode...
      }
      p <- add_trace(p, x= valuesToPlot, y= depths, mode = "lines", type = "scatter",name = outputNames[[i]]) 
    }
  }

  if(profilePretag == "tsoil"){
      return(p %>% layout(xaxis = list(range = c(0,20), title = "Soil temperature profile [°C]"),yaxis = list(title = "depths[cm]")))
  } else {
      return(p %>% layout(xaxis = list(range = c(0,50), title = "Soil water content [V%]"), yaxis = list(title = "depths[cm]")))
  }
}
