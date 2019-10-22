musoDate <- function(startYear, endYears = NULL, numYears, combined = TRUE, leapYearHandling = FALSE, prettyOut = FALSE){

   if(is.null(endYears) & is.null(numYears)){
       stop("You should provide endYears or numYears")
   }

   if(is.null(endYears)){
       endYear <- startYear + numYears -1
   }

   dates <- seq(from = as.Date(paste0(startYear,"01","01"),format = "%Y%m%d"), to =  as.Date(paste0(endYear,"12","31"),format = "%Y%m%d"), by = "day")
   if(leapYearHandling){
       if(prettyOut){
           return(cbind(format(dates,"%d.%m.%Y"),
                        as.numeric(format(dates,"%d")),
                        as.numeric(format(dates,"%m")),
                        as.numeric(format(dates,"%Y")))   )
       }

       if(combined == FALSE){
           return(cbind(format(dates,"%d"),format(dates,"%m"),format(dates,"%Y")))
       } else {
           return(format(dates,"%d.%m.%Y"))
       }

   } else {
        dates <- dates[format(dates,"%m%d")!="0229"]
       if(prettyOut){
           return(data.frame(date = as.Date(format(dates,"%Y-%m-%d")),
                             day = as.numeric(format(dates,"%d")),
                             month = as.numeric(format(dates,"%m")),
                             year = as.numeric(format(dates,"%Y"))))
       }


       if(combined == FALSE){
           return(cbind(format(dates,"%d"),format(dates,"%m"),format(dates,"%Y")))
       } else {
           return(format(dates,"%d.%m.%Y"))
       }
   }

}

#' setupGUI
#'
#' This function reads reneral setting from the iniFile
#' @param iniName The name of the inifile
#' @param baseDir The base directory from within the model will be launced
#' @param centralData The central datastructure of the AgroMo


setupGUI <- function(iniName, baseDir, centralData){

  settings <- new.env()

 if(length(iniName)==0){
   return("")
 }
 searchBellow <- function(inFile, key, stringP = TRUE,  n=1, management = FALSE){
     
         if(stringP){
             unlist(strsplit(inFile[grep(key,inFile,perl=TRUE)+n],split = "\\s+"))[1]
         } else {
             as.numeric(unlist(strsplit(inFile[grep(key,inFile,perl=TRUE)+n],split = "\\s+"))[1])
         }
 }

  ini <- readLines(file.path(baseDir,"input","initialization","site",iniName))
  getFirstNumber <- function(rowNum) {
    as.numeric(gsub("([0-9]*).*","\\1", ini[rowNum]))
  } 
  startOutSection  <- grep("DAILY_OUTPUT", ini, perl = TRUE)
  numOutVars <- 0
  if(length(startOutSection) != 0){
    numOutVars <- getFirstNumber(startOutSection + 1)
    varCodes <- sapply(seq(from=(startOutSection + 2),length=numOutVars), getFirstNumber)
    centralData <- centralData[unlist(centralData["VARCODE"]) %in% varCodes,]
  } 
  # browser()
  settings[["epc"]] <- tryCatch(basename(searchBellow(ini,"EPC_FILE")), error = function(e){""})
  settings[["meteo"]] <- tryCatch(basename(searchBellow(ini,"MET_INPUT")),error = function(e){""})
  ## browser()
  settings[["soil"]] <- tryCatch(basename(searchBellow(ini,"SOIL_FILE")),error = function(e){""})
  settings[["mgm"]] <- tryCatch(basename(searchBellow(ini,"MANAGEMENT_FILE")),error = function(e){""})
  settings[["outputName"]]<- tryCatch(basename(searchBellow(ini,"OUTPUT_CONTROL")),error = function(e){""})
  settings[["numYears"]] <- searchBellow(ini,"TIME_DEFINE",FALSE,1)
  settings[["startYear"]] <- searchBellow(ini,"TIME_DEFINE",FALSE,2)
  settings[["numOutVars"]] <- numOutVars
  settings[["numLayers"]] <- 7
  settings[["variableNames"]] <- centralData[,"VARIABLE"]
  settings[["labelName"]] <- centralData[,"LABEL NAME"]
  settings[["outputTypeRow"]] <- searchBellow(ini,"OUTPUT_CONTROL",2) 
  settings[["numData"]] <- with(settings,numOutVars * 365 * numYears)
  # browser()
  return(settings)
}

runMuso <- function(iniFile){
 system2("./muso",sprintf("input/initialization/site/tmp/%s",iniFile))
 settings <- setupGUI(iniFile)
}

readAndChangeIni <- function(iniFile, weatherFile, soilFile, managementFile, lines = c(4,40,46)){
 print(iniFile)
 init <- tryCatch(readLines(sprintf("input/initialization/%s",iniFile)), error = function (e) "Cannot run the smodel")
 paths <- c(sprintf("input/weather/%s",weatherFile),sprintf("input/soil/site/%s",soilFile),sprintf("input/management/%s", managementFile))
 init[lines] <- paths
 dir.create("input/initialization/tmp/",showWarnings = FALSE)
 writeLines(init,sprintf("input/initialization/tmp/%s",iniFile))
}


