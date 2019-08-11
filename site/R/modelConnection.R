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

setupGUI <- function(iniName){

  settings <- new.env()

 if(length(iniName)==0){
   return("")
 }
 searchBellow <- function(inFile, key, stringP = TRUE,  n=1, management = FALSE){
     if(!management){
         if(stringP){
             unlist(strsplit(inFile[grep(key,inFile)+n],split = "\\s+"))[1]
         } else {
             as.numeric(unlist(strsplit(inFile[grep(key,inFile)+n],split = "\\s+"))[1])
         }
     } else {
         if(unlist(strsplit(inFile[grep(key,inFile)+n], split = "\\s+"))[1]=="no"){
             "no management"
         }
     }

   }

  ini <- readLines(paste0("input/","initialization/",iniName))
  settings[["epc"]] <- basename(searchBellow(ini,"EPC_FILE"))
  settings[["meteo"]] <- basename(searchBellow(ini,"MET_INPUT"))
  settings[["soil"]] <- basename(searchBellow(ini,"SOILPROP_FILE"))
  settings[["mgm"]] <- basename(searchBellow(ini,"MANAGEMENT_FILE",management = TRUE))
  settings[["outputName"]]<- basename(searchBellow(ini,"OUTPUT_CONTROL"))
  settings[["numYears"]] <- searchBellow(ini,"TIME_DEFINE",FALSE,2)
  settings[["startYear"]] <- searchBellow(ini,"TIME_DEFINE",FALSE,3)
  settings[["numOutVars"]] <- 26# It is faster than using a function for it. (length(variableNames))
  settings[["numLayers"]] <- 7
  settings[["variableNames"]] <- c("leaf_DM", "leaflitr_DM", "froot_DM", "fruit_DM", "softstem_DM", "proj_lai", "cum_evap", "cum_trans", "rooting_depth", "daily_gpp", "daily_tr", "daily_nee", "tsoil_0", "tsoil_1", "tsoil_2", "tsoil_3", "tsoil_4", "tsoil_5", "tsoil_6", "vwc_0", "vwc_1", "vwc_2", "vwc_3", "vwc_4", "vwc_5", "vwc_6")

  mgm <- with(settings,{
    if(mgm != "no management"){
      readLines(paste0("input/","initialization/",mgm))
    } else {
      NA
    }
  })
  ## manTypes <- c("planting","harvest","fertilization","irrigation","cultivation","grazing","mowing","thinning")

  ## if(is.na(mgm)){
  ##   Map(function(x,y){assign(x,y,envir=settings)},manTypes,rep(NA,8))
  ## } else {
    
  ##   Map(function(x,y){assign(x,y,envir=settings)},manTypes,manResults)
  ## }



  settings[["numData"]] <- with(settings,numOutVars * 365 * numYears)
  return(settings)
}


runMuso <- function(iniFile){
 system2("./muso",sprintf("input/initialization/tmp/%s",iniFile))
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


