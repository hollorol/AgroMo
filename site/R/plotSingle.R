plotSingle <- function(outputNames = NULL, dataenv = readRDS("output/outputs.RDS"), varName, timeFrame, groupFun, plotT = "scatter", conversionFactor = 1, measurement, experiment_id, treatment, repetationsAveraged){ #, measurement, experiment_id, treatment, repetationsAveraged

  plotType <- plotT
  plotMode <- NULL

  if(plotType == "scatter"){
    plotMode <- "markers"
  }

  if(plotType == "line"){
    plotType<- "scatter"
    plotMode <- "line"
  }

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

  dataenv[[outputNames[1]]] <- data.table(dataenv[[outputNames[1]]])


  pd <- dataenv[[outputNames[1]]][,eval(quote(conversionFactor))*get(groupFun)(get(varName)),match.fun(eval(quote(timeFrame)))(date)]
  colnames(pd)<- c(timeFrame, paste0(varName,"_",groupFun))
  p <- plot_ly()
  p <- add_trace(p,x = fDate(unlist(pd[,timeFrame,with = FALSE]),timeFrame), y =  unlist(pd[,paste0(varName,"_",groupFun),with = FALSE]), type = plotType, mode = plotMode, name = outputNames[1])

  if(length(outputNames) >= 2){
    for(i in 2:length(outputNames)){
      dataenv[[outputNames[i]]] <- data.table(dataenv[[outputNames[i]]])
      pd <- dataenv[[outputNames[i]]][,eval(quote(conversionFactor))*get(groupFun)(get(varName)),match.fun(eval(quote(timeFrame)))(date)]
      colnames(pd)<- c(timeFrame, paste0(varName,"_",groupFun))
      print(str(pd))
      p <- add_trace(p, x = fDate(unlist(pd[,timeFrame, with = FALSE]),timeFrame), y =  unlist(pd[,paste0(varName,"_",groupFun),with = FALSE]), name = outputNames[i],type = plotType, mode = plotMode)

    }
  }

  if(containMeasurement){
    p <- plotMeasuredLayers(p,filtMeasured,timeFrame, experiment_id,treatment)

  }
  p %>% layout(yaxis=list(title=sprintf("%s|%s|%s", varName, timeFrame, groupFun)))
}

plotMeasuredLayers <- function(p,measurement,timeFrame,experiment_id, treatment){

  if(is.null(measurement$repetition)){
    p <- add_trace(p,x = unlist(get(timeFrame)(measurement$date)),y = unlist(measurement$value), name = sprintf("%s-%s (mean)",experiment_id,treatment))
  } else {
    repetitions<- unique(measurement$repetition)
    if(length(repetitions) >= 5)
      stop("You must average your repetitions in case of more than 5")
    for(i in 1:length(repetitions)){
      actRep <- repetitions[i]
      p<- add_trace(p,x = unlist(get(timeFrame)(measurement[repetition == eval(quote(actRep))]$date)),
                    y = as.numeric(unlist(measurement[repetition == eval(quote(actRep))]$value)),name = sprintf("%s-%s#%d",experiment_id,treatment,actRep))
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
    print(typeof(dat))
    return(as.Date(dat,origin = "1970-01-01"))
  } else {
    if(timeFrame == "month"){
      return(dat)
    } else {
      return(as.Date(sprintf("%s-12-31",as.character(dat))))
    }
  } 
}

 #plotSingle(outputNames, dataenv, "fruit_DM","year","max",plotT="line",10000,repetationsAveraged=TRUE, measurement,experimentID,treatment)
##  dataenv <- readRDS("output/outputs.RDS")
##  outputSites <- ls(dataenv)
##  outputNames <- sample(outputSites,3)
## ## head(dataenv[[outputSites[1]]])
##  varName <- "fruit_DM"
##  timeFrame <- "year"
##  groupFun <- "sum"
##  measurement <- fread("~/agromo/observations.csv")
##  measurement[,date:=as.Date(datetime)]
##  experimentID <- "MV|planting_date"
##  treatment <- "T01|F01|V01"
## ## measurement[]
## origMes<- getFilteredData(measurement,treatment,experimentID,TRUE)
## p <- plot_ly()
## p <- add_trace(p, x=year(origMes$date),y=origMes$value, name = "orig")
## p <- add_trace(p, x = 2005, y = 6000, name = "new")
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

## ser <- 1991:2018
## med <- sort(c(2000,1998))
## med
## ser[!(ser %in% med)] <- NA
## ser
## csumi <- getFilteredData(measurement,treatment,experimentID,TRUE)
## csumi[,date:=]
## readObserved <-
## readRDS("")

## dayoutput<- data.table(dayoutput)
## cdayout <- copy(dayoutput)
## cdayout[,daily_gpp:=daily_gpp*1.3]
## dayoutput[,id:=1]
## cdayout[,id:=2]
## dayout<- list(orig=dayoutput, changed =cdayout)
## dayoutput
## cdayout
## myBigTable <-rbindlist(dayout)
## bx <- myBigTable[,"daily_gpp",with =FALSE
## dayout[[2]]
## plot_ly(myBigTable,x = ~date,y= ~daily_gpp,color= ~factor(id),mode = "line")
## bp <- plot_ly(dayout[[1]],x = ~date, y = ~daily_gpp, type = "scatter", mode = "line", name = "csaba")

## for(dt in 2:length(dayout)){
##   bp <-add_lines(bp, x = ~date, y = ~daily_gpp,data = dayout[[i]], name = "csilla")

## }

## recApply <- function(n,fun,v){
  
##   if(n==1){
##     return(fun(v))
##   } else {
##     n <- n - 1
##   }
  
##   fun(recApply(n,fun,v))
## }
