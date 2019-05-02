library("data.table")
library("udunits2")
measurement <- fread("observations.csv")
measurement[,datetime:=as.POSIXct(datetime)]
measurement[,value:=as.numeric(value)]
measurement[paramater == "yield",sum(value,na.rm = TRUE),year(datetime)]
str(measurement)

getDecade <- function(datetime){
  Y <-  year(datetime)
  Y - Y%%10
}



measurement[parameter=="yield",list(summa = sum(value,na.rm = TRUE)),(year(datetime)-year(datetime)%%10)]
colnames(measurement)
measurement

getExperiments <- function(datas){
    unique(datas[,"experiment",with = FALSE])
}

getTreatments <- function(datas){
    unique(datas[,"treatment",with = FALSE])
}
getExperiments(measurement)
getTreatments(measurement)

getFilteredData <- function(datas, treatment, experiment, repetationsAveraged){
    tre <- treatment
    exper <- experiment
    if(repetationsAveraged){
        datas[(treatment == eval(quote(tre))) & (experiment == eval(quote(exper))),
              list(value=mean(as.numeric(value),na.rm = TRUE)),
              by = list(datetime,parameter)]
    } else {
        datas[(treatment == eval(quote(tre))) & (experiment == eval(quote(exper)))]
    }
    
}
getFilteredData(measurement, "T04|F05|V05", "MV|planting_date", TRUE) %>% ggplot(aes(x=as.Date(datetime),y = value)) + geom_point()
treatment
measurement
measurement[(treatment == list(treatment)) & (measurement == list(measurement))]
datas[(treatment == eval(quote(tre))) & (experiment == eval(quote(exper))),
      mean(as.numeric(value),na.rm = TRUE),
      by = list(datetime,treatment)]
colorPalett <-
    <- read.rds() 
createPlotOrTable <- function(observed, measurement, simResults, variable, timeFrame, aggFun, type){
    
}
