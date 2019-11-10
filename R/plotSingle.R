#' plotSingle
#' 
#' plotSingle
#' @param outputName blucs
#' @importFrom data.table data.table set year month
#' @importFrom plotly plot_ly add_trace layout '%>%' toRGB


plotSingle <- function(outputNames = NULL, dataenv, varName, timeFrame, groupFun, plotT = "scatter", conversionFactor = 1, measurement, experiment_id, treatment, repetationsAveraged, yTitle){ 
# print(ls(dataenv))
  plotType <- plotT
  plotMode <- NULL

  if(plotType == "scatter"){
    plotMode <- "markers"
  }

  if(plotType == "line"){
    plotType<- "scatter"
    plotMode <- "line"
  }

  titlefont <- list(
    family = "Arial",
    size = 26,
    color = "black"
  )

# Parameters of the axis labels:
  tickfont <- list(
    family = "Arial",
    size = 22,
    color = "black" # to set scientific format use: exponentformat = "E"
  )
## browser()
  measurement[,date:=as.Date(datetime)]
  set(measurement, i=which(measurement[["parameter"]]=="yield"), j="parameter", value="fruit_DM")
  filtMeasured <- tryCatch(getFilteredData( measurement, treatment, experiment_id, repetationsAveraged), error = function (e) NULL)
  if(is.null(filtMeasured)){
    containMeasurement <- FALSE
  } else {
    if(nrow(filtMeasured)==0 || !is.element(varName,unlist(measurement[,"parameter", with = FALSE]))){
      containMeasurement <- FALSE
    } else {
     containMeasurement <- TRUE
    }
  }


  if(timeFrame=="day"){
    groupFun <- "identity"
    timeFrame <- "identity" # This gaves us back the current date, it will create numDays separate groups. The group function will also the identity
  }

  ## browser()
  dataenv[[outputNames[1]]] <- data.table(dataenv[[outputNames[1]]])
  dataenv[[outputNames[1]]][,year:=as.Date(date)]
  print(yTitle)
   # browser()
  timeFrameF <- match.fun(timeFrame)
  pd <- dataenv[[outputNames[1]]][,eval(quote(conversionFactor))*get(groupFun)(get(varName)),timeFrameF(date)]
  colnames(pd)<- c(timeFrame, paste0(varName,"_",groupFun))
  p <- plot_ly()
  p <- add_trace(p,x = fDate(unlist(pd[,timeFrame,with = FALSE]),timeFrame), y =  unlist(pd[,paste0(varName,"_",groupFun),with = FALSE]), type = plotType, mode = plotMode, name = outputNames[1],line = list( width = 2,
              xaxs = "i", yaxs = "i") ) %>%
     # title = "<b>x tengely</b>", # bold title, to get itali title use <i>
     #                    titlefont = titlefont,
    layout(autosize=TRUE,
           # height="500px",
           margin =  list(l=150, r=20, b=50, t=50),
           xaxis = list(ticks = "outside",
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

           yaxis = list(ticks = "outside",
                        title = yTitle, # bold title, to get itali title use <i>
                        titlefont = titlefont,
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
                        linewidth = 2))


  if(length(outputNames) >= 2){
    for(i in 2:length(outputNames)){
      dataenv[[outputNames[i]]] <- data.table(dataenv[[outputNames[i]]])
      pd <- dataenv[[outputNames[i]]][,eval(quote(conversionFactor))*get(groupFun)(get(varName)),timeFrameF(date)]
      colnames(pd)<- c(timeFrame, paste0(varName,"_",groupFun))
      # print(str(pd))
      p <- add_trace(p, x = fDate(unlist(pd[,timeFrame, with = FALSE]),timeFrame), y =  unlist(pd[,paste0(varName,"_",groupFun),with = FALSE]), name = outputNames[i],type = plotType, mode = plotMode, line = list( width = 2,
              xaxs = "i", yaxs = "i") )

    }
  }

  if(containMeasurement){
    p <- plotMeasuredLayers(p,filtMeasured,timeFrame, experiment_id,treatment)

  }
    # p %>% layout(yaxis=list(title=sprintf("%s|%s|%s", varName, timeFrame, groupFun))) # %>% toWebGL()
    p 
}

plotMeasuredLayers <- function(p,measurement,timeFrame,experiment_id, treatment){

  if(is.null(measurement$repetition)){
    p <- add_trace(p,x = unlist(get(timeFrame)(measurement$date)),y = unlist(measurement$value), name = sprintf("%s-%s (mean)",experiment_id,treatment),line = list( width = 2,
              xaxs = "i", yaxs = "i") )
  } else {
    repetitions<- unique(measurement$repetition)
    if(length(repetitions) >= 5)
      stop("You must average your repetitions in case of more than 5")
    for(i in 1:length(repetitions)){
      actRep <- repetitions[i]
      p<- add_trace(p,x = unlist(get(timeFrame)(measurement[repetition == eval(quote(actRep))]$date)),
                    y = as.numeric(unlist(measurement[repetition == eval(quote(actRep))]$value)), name = sprintf("%s-%s#%d",experiment_id,treatment,actRep),line = list( width = 2,
              xaxs = "i", yaxs = "i") )

    }
  }
return(p)
}




decade <- function(datetime){
  Y <-  year(datetime)
  Y - Y%%10
}

fDate <- function(dat, timeFrame){
  if(timeFrame == "identity"){
    # print(typeof(dat))
    return(as.Date(dat,origin = "1970-01-01"))
  } else {
    if(timeFrame == "month"){
      return(dat)
    } else {
      return(as.Date(sprintf("%s-12-31",as.character(dat))))
    }
  } 
}

getFilteredData <- function(measurement, treatment, experiment, repetationsAveraged){
    tre <- treatment
    exper <- experiment
    if(repetationsAveraged){
        measurement[(treatment == eval(quote(tre))) & (experiment == eval(quote(exper))),
              list(value=mean(as.numeric(value),na.rm = TRUE)),
              by = list(date,parameter)]
    } else {
        measurement[(treatment == eval(quote(tre))) & (experiment == eval(quote(exper)))]
    }
    
}

getDataFromDBTable <- function(conn,timeFrame,groupFun,tableName,variableName, conversionFactor){
   groupFunPairs <- list(min="MIN",max="MAX",mean="AVG",median="MEDIAN",var="STDEV",)
   timeFramePairs <- list(year="\"%Y\"", month="\"%Y%m\"", day="\"%Y%m%d\"")
   timeFrameS <- timeFramePairs[[timeFrame]]
   groupFunS <- groupFunPairs[[groupFun]]
   querySentence <- sprintf("SELECT STRFTIME(%s,Date(udate)) AS %s,%s(%s)*%f AS %s FROM %s GROUP BY STRFTIME(%s,DATE(udate))",
                            timeFrameS,
                            timeFrame,
                            groupFunS,
                            variableName,
                            conversionFactor,
                            variableName,
                            tableName,
                            timeFrameS)
   queryRes <- dbGetQuery(conn,querySentence)
   queryRes
}
