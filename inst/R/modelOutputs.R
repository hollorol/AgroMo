
writeDataToEnv <- function(settings, envir,binaryName, outFile = "output/outputs.RDS", outputName){
browser() 
  con <- file(binaryName,"rb")

  dayoutput <- matrix(readBin(con,"double",size=8,n=(settings$numData)),(settings$numYears*365),byrow=TRUE)
  close(con)
  dayoutput <- cbind.data.frame(musoDate(startYear = settings$startYear,
                                         numYears = settings$numYears,
                                         combined = FALSE, prettyOut = TRUE),
                                dayoutput)
  colnames(dayoutput) <- as.character(c("date","day","month","year",unlist(settings$variableNames)))
  envir[[outputName()]]<- dayoutput
  saveRDS(envir,file = outFile)
}

