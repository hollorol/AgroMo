#' writeDataToDB
#'
#' This function reads the model binary and put that into a database
#' @param settings The result of the setupGUI
#' @param dbConnection An SQLite connection
#' @param binaryName The name of the binary output file
#' @param outputName The name of the result table
#' @importFrom DBI dbWriteTable

writeDataToDB <- function(settings, dbConnection, binaryName, outputName){

  con <- file(binaryName,"rb")
# browser()
  dayoutput <- matrix(readBin(con,"double",size=8,n=(settings$numData)),(settings$numYears*365),byrow=TRUE)
  close(con)
  dayoutput <- cbind.data.frame(musoDate(startYear = settings$startYear,
                                         numYears = settings$numYears,
                                         combined = FALSE, prettyOut = TRUE),
                                dayoutput, outputName)
  # browser()
  colnames(dayoutput) <- as.character(c("udate","uday","umonth","uyear",unlist(settings$variableNames),"outputName"))
  conn <- dbConnection()
  dbWriteTable(conn, outputName, dayoutput,  overwrite = TRUE)
}
