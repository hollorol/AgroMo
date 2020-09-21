#' plotSingle
#' 
#' plotSingle
#' @param outputName blucs
#' @importFrom data.table data.table set year month
#' @importFrom plotly plot_ly add_trace layout '%>%' toRGB


plotSingle <- function(outputNames = NULL, dataenv, varName, timeFrame, groupFun, plotT = "scatter", conversionFactor = 1, measurementConn, experiment_id, treatment, repetationsAveraged, yTitle,measAlias=""){ 
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
  # filtMeasured <- tryCatch(getFilteredData(measurementConn, treatment, experiment_id, repetationsAveraged,varName), error = function (e) NULL)
  #   # browser()
  # if(is.null(filtMeasured)){
  #   containMeasurement <- FALSE
  # } else {
  #   if(nrow(filtMeasured)==0){
  #     containMeasurement <- FALSE
  #   } else {
  #    containMeasurement <- TRUE
  #    filtMeasured$measurement_date <- as.Date(filtMeasured$measure_date)
  #   }
  # }


  if(timeFrame=="day"){
    groupFun <- "identity"
    timeFrame <- "identity" # This gaves us back the current date, it will create numDays separate groups. The group function will also the identity
  }

  measurements <- tryCatch(read.csv(file.path(measurementConn,experiment_id), sep=";",stringsAsFactors=FALSE), error=function(e){
                               NULL
  })
  if(!is.null(measurements)){
        measurements$date <- as.Date(measurements$date)
  }

  p <- plot_ly()

  if(!is.null(measurements)){
    p <- addMeasuredData(p, measurements, varName)
  }

  ## browser()
  dataenv[[outputNames[1]]] <- data.table(dataenv[[outputNames[1]]])
  dataenv[[outputNames[1]]][,year:=as.Date(date)]
  print(yTitle)
   # browser()
  timeFrameF <- match.fun(timeFrame)
  pd <- dataenv[[outputNames[1]]][,eval(quote(conversionFactor))*get(groupFun)(get(varName)),timeFrameF(date)]
  colnames(pd)<- c(timeFrame, paste0(varName,"_",groupFun))
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
                        title = yTitle, # bold title, to get italic title use <i>
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

  # if(containMeasurement){
  #   p <- plotMeasuredLayers(p,filtMeasured,timeFrame, experiment_id,treatment,measAlias=measAlias)
  #
  # }
  
    # p %>% layout(yaxis=list(title=sprintf("%s|%s|%s", varName, timeFrame, groupFun))) # %>% toWebGL()
  p
    
}

plotMeasuredLayers <- function(p,measurement,timeFrame,experiment_id, treatment, measAlias=""){
   if(measAlias==""){
        measAlias =sprintf("%s-%s (mean)",experiment_id,treatment)
   }
  # if(is.null(measurement$repetition)){
    p <- add_trace(p,x = unlist(get(timeFrame)(measurement$measurement_date)),y = unlist(measurement$measurement_value), name = measAlias, color="black")
  # } else {
# # browser()
#   #   repetitions<- unique(measurement$repetition)
#   # print(repetitions)
#   #   if(length(repetitions) >= 5)
#   #     stop("You must average your repetitions in case of more than 5")
#   #   for(i in 1:length(repetitions)){
#   #     actRep <- repetitions[i]
#   #   # browser()
#   #     p<- add_trace(p,x = unlist(get(timeFrame)(measurement[measurement[,"repetition"] == eval(quote(actRep)),]$measure_date)),
#   #                   y = as.numeric(unlist(measurement[measurement[,"repetition"] == eval(quote(actRep)),]$measurement_value)), name = sprintf("%s-%s#%d",experiment_id,treatment,actRep),line = list( width = 2,
#   #             xaxs = "i", yaxs = "i") )
#     #}
#
#
#
  # }

 addMeasuredData(p, measurements, varName)

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

getFilteredData <- function(dbConnection, treatment, experiment, repetationsAveraged, varName){
    # tre <- treatment
    # exper <- experiment
    # if(repetationsAveraged){
    #     measurement[(treatment == eval(quote(tre))) & (experiment == eval(quote(exper))),
    #           list(value=mean(as.numeric(value),na.rm = TRUE)),
    #           by = list(date,parameter)]
    # } else {
    #     measurement[(treatment == eval(quote(tre))) & (experiment == eval(quote(exper)))]
    # }
if(repetationsAveraged){
    dbGetQuery(dbConnection,sprintf("
                             SELECT strftime('%%Y-%%m-%%d',datetime) AS measure_date, AVG(value) AS measurement_value
                             FROM site
                             WHERE value!='NA' AND
                             datetime!='NA' AND
                             variable == '%s' AND
                             experiment='%s' AND
                             key LIKE '%%%s'
                             GROUP BY key
                             ",varName, experiment,treatment))
} else {
    dbGetQuery(dbConnection,sprintf("
                             SELECT strftime('%%Y-%%m-%%d',datetime) AS measure_date, value AS measurement_value, repetition
                             FROM site
                             WHERE value!='NA' AND
                             datetime!='NA' AND
                             variable == '%s' AND
                             experiment='%s' AND
                             key LIKE '%%%s'
                             ",varName, experiment,treatment))
}
    
}

addMeasuredData <- function(p, measurements, varName){

    givenDataLabels <- colnames(measurements)

    filtVar <- (measurements[["var_id"]]==varName)

    if(all(!filtVar)){
        return(p)
    }

    measurements <- measurements[filtVar,]

    if(all(c("min","max") %in% givenDataLabels)){
        p <- add_trace(p,
                       x= measurements$date,
                       y= measurements$min,
                       mode="l",
                       type="scatter",
                       line = list(color = "rgba(169,169,169,0.3)", width = 1),
                       name = "observed min")
        p <- add_trace(p,
                       x= measurements$date,
                       y= measurements$max, mode="l",
                       fill="tonexty", type="scatter",
                       line = list(color = "rgba(169,169,169,0.3)", width = 1),
                       fillcolor = "rgba(189,189,189,0.3)",
                       name = "observed max")
    }

    if(is.element("sd",givenDataLabels)){
        p <- add_trace(p,
                       x= measurements$date,
                       y= measurements$mean - measurements$sd,
                       mode="l",
                       type="scatter",
                       line = list(color = "rgba(108,108,108,0.3))", width = 1),
                       name = "observed mean - sd")
        p <- add_trace(p,
                       x= measurements$date,
                       y= measurements$mean + measurements$sd,
                       mode="l",
                       fill="tonexty",
                       type="scatter",
                       line = list(color = "rgba(108,108,108,0.3)", width = 1),
                       fillcolor = "rgba(128,128,128,0.3)",
                       name = "observed mean + sd")
    }

    add_trace(p,
              x= measurements$date,
              y= measurements$mean,
              mode="l",
              type="scatter",
              line = list(color = "rgb(0,0,0)", width = 1),
              name="observed mean")

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
