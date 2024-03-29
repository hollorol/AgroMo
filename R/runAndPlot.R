#' runAndPlotUI
#' 
#' This function provides the UI for the main runAndPlot function
#' @param id Unique identifier
#' @param label Label for the button
#' @importFrom shiny NS actionButton


runAndPlotUI <- function(id, label){
  ns <- NS(id)
  actionButton(ns("runModel"),label = "START SIMULATION")
}

#' runAndPlot
#' 
#' Blabl
#' @importFrom shiny reactiveValues observe isolate modalDialog showModal removeUI reactive
#' @importFrom shinyjs hide show
#' @importFrom future future
#' @importFrom promises %...>%
runAndPlot <- function(input, output, session,baseDir,
                       iniFile, weatherFile, soilFile, managementFile, outputName,
                       planting, harvest, fertilization, irrigation, grazing, mowing,
                       thinning, planshift_date, planshift_density, harvshift_date,
                       fertshift_date, irrshift_date, fertshift_amount, irrshift_amount, connection, centralData, siteRun, plotid,
                       runModel
                       ){
  ##preparation
dat<-reactiveValues(dataenv = NULL)
###if output is already done
#   observe({
#     print(irrshift_amount())
# })
  observeEvent(input$runModel,{
                   if(siteRun()){

       ## browser()
       chosenIni <- file.path(isolate(baseDir()),"input", "initialization",
                               "site", iniFile())
       errorFiles <- checkFileSystemForNotif(chosenIni)

       unlink(file.path(isolate(baseDir()),"input", "initialization",
                               "site","tmp"))
       if(length(errorFiles) == 0){ 
           readAndChangeFiles(isolate(baseDir()), iniFile(), weatherFile(), soilFile(), managementFile(),
                              planting(), harvest(), fertilization(), irrigation(), grazing(),
                              mowing(), thinning(), planshift_date(), planshift_density(),
                              harvshift_date(), fertshift_date(),
                              irrshift_date(), fertshift_amount(), irrshift_amount())
           # browser()
           settings <- setupGUI(isolate(iniFile()),isolate(baseDir()),isolate(centralData()))
           file.remove(file.path(baseDir(), "output", sprintf("%s.dayout", settings$outputName)))
           runModel <- reactive({future({runMuso(isolate(iniFile()),isolate(baseDir()))})})
           showModal(shiny::tags$div(id = "runIndicator", modalDialog(
                                                                      shiny::tags$img(id = "runningGif", src= "www/img/iu.gif", width = "150px"),
                                                                  hide(tags$img(id = "finishedGif", src= "www/img/iu_check.gif",width = "150px"))
                                                                  ,renderText({
                                                                      # browser()
                                                                      runModel() %...>% {
                                                                          print(connection())
                                                                          print(baseDir())
                                                                          print(outputName())
                                                                          writeDataToDB(settings = settings, dbConnection = isolate(connection()),
                                                                                        binaryName = file.path(baseDir(), "output", sprintf("%s.dayout", settings$outputName)),
                                                                                        isolate(outputName()))          
                                                                          unlink(file.path("input/initialization/site/tmp"),recursive=TRUE)
                                                                          unlink(file.path("input/management/tmp"),recursive=TRUE)
                                                                          # removeUI(selector = "#runningGif")
                                                                          hide("runningGif")
                                                                          removeModal()
                                                                          # show("finishedGif")
                                                                          # "Finished"
                                                                      }

                                                                  }),
                                                     footer = NULL,
                                                     easyClose = TRUE
                                                     ))
       )

       } else {
        # TODO: Nasty convoluted fast solution.
        showNotification2(ui=paste(sapply(names(errorFiles), function(eFile){
                                    sprintf(" the file indicated in [%s] file is missing: %s", errorFiles[eFile][[1]]$parent,
                                            errorFiles[eFile][[1]]$children)
                                 } ),collapse="<br/>"), type="error", duration = NULL, id = session$ns("siteerror")) 

       }
                   }   else {
                           tableName <- isolate(iniFile())
                           showNotification(sprintf("Querying table: %s in grid.db...", tableName))
                           gridDB <- dbConnect(RSQLite::SQLite(),file.path(isolate(baseDir()),"output", "grid.db"))
                           siteDB <- dbConnect(RSQLite::SQLite(),file.path(isolate(baseDir()),"output", "site.db"))
                           res <- dbGetQuery(gridDB,sprintf("SELECT * FROM %s WHERE plotid = %s", tableName, isolate(plotid())))
                           if(is.element("year",colnames(res))){
                               norig <- ncol(res)
                               res[,"udate"] <- paste0(res[,"year"],"-12-31")
                               res[,"umonth"] <- 12
                               res[,"uday"] <- 31
                               res <- res[,c(seq(from=norig+1, by=1, length.out=3), 1:norig)]
                           }

                           dbWriteTable(siteDB, isolate(outputName()), res, overwrite=TRUE)
                           showNotification("Grid run is saved into site database")
                           dbDisconnect(gridDB)
                   }
        })

}

#' runMuso
#' 
#' runMuso
#' @param iniFile a
#' @param baseDir a

runMuso <- function(iniFile,baseDir){
  ## browser()
  a <- getwd()
  setwd(baseDir)
  system2("./muso",sprintf("input/initialization/site/tmp/%s",iniFile))
  setwd(a)
}

#' readAndChangeFiles
#' 
#' This function creates tmp file and change them
#' @param baseDir a

readAndChangeFiles <- function(baseDir, iniFile, weatherFile, soilFile, managementFile,
                               planting=NULL, harvest=NULL, fertilization=NULL, irrigation=NULL,
                               grazing=NULL, mowing=NULL, thinning=NULL,
                               planshift_date, planshift_density, harvshift_date, fertshift_date, irrshift_date, fertshift_amount, irrshift_amount
                               ){
  # print(file.path(baseDir,"input","initialization","site",iniFile))
  managementPath <- if(managementFile == "none"){managementFile} else {
                                   file.path("input/management/tmp", managementFile)}
  ## managementPath <- "none"
  soilPath <- file.path("input","soil","site",soilFile)
  weatherPath <- file.path("input","weather","site",weatherFile)
  init <- tryCatch(readLines(file.path(baseDir,"input","initialization","site",iniFile)), error = function (e) stop("Cannot run the smodel"))
  lines <- c(grep("MET_INPUT",init,perl=TRUE),  grep("SOIL_FILE", init, perl=TRUE), grep("MANAGEMENT_FILE",init, perl = TRUE)) + 1
  paths <- c(weatherPath,soilPath, managementPath)
  init[lines] <- paths
  init[(grep("OUTPUT_CONTROL",init, perl=TRUE) + 2)] <- 1
  dir.create(file.path(baseDir,"input/initialization/site/tmp/"), showWarnings = FALSE)
  dir.create(file.path(baseDir,"input/management/tmp/"), showWarnings = FALSE)
  managementTypes <- c("planting", "harvest", "fertilization", "irrigation", "cultivation", "grazing", "mowing", "thinning")
  sapply(managementTypes,function(man){
    dir.create(file.path(baseDir,"input/management/tmp/",man),showWarnings = FALSE)
  })
  changingMGM(managementFile,baseDir, planting, harvest, fertilization, irrigation, grazing, mowing, thinning,
              planshift_date, planshift_density, harvshift_date, fertshift_date, irrshift_date, fertshift_amount, irrshift_amount
              )
  writeLines(init,file.path(baseDir,"input/initialization/site/tmp/",iniFile))
}

#' shiftFile
#'
#' Makes small changes: shiftes in management files
#' @param manFile a
#' @importFrom data.table fread fwrite

shiftFile <- function(manFile, destFile, shiftDate, varCol=NULL, shiftVar=NULL){
  #browser()
  manDT <- fread(manFile)
  manDT[,DATE:=format.Date(as.Date(DATE,format="%Y-%m-%d")+as.numeric(shiftDate),format="%Y-%m-%d")]
  if(is.null(varCol)){
    fwrite(manDT,destFile,sep = " ")
  } else {
    varName <- colnames(manDT)[varCol]
    manDT[,eval(varName):=eval(parse(text = varName))+shiftVar]
    fwrite(manDT,destFile,sep = " ")
  }
}

managementTemplate <- c("MANAGEMENT_INFORMATION MuSo6", "-", "PLANTING", "1", "none", "-", "THINNING", "1", "none", "-", "MOWING", "1", "none", "-", "GRAZING", "1", "none", "-", "HARVESTING", "1", "none", "-", "PLOUGHING", "1", "none", "-", "FERTILIZING", "1", "none", "-", "IRRIGATING", "1", "none")
managementRows <- c("plt" = 5, "thn" =  9, "mow" = 13, "grz"= 17, "hrv"= 21, "plo" = 25, "frz" = 29, "irr" = 33)

#' changingMGM
#'
#' This function changes the management files
#' @param mgmFile asdf


changingMGM <- function(mgmFile, baseDir, planting=NULL, harvest=NULL, fertilization=NULL, irrigation=NULL, grazing=NULL, mowing=NULL, thinning=NULL,
                        planshift_date, planshift_density, harvshift_date, fertshift_date, irrshift_date, fertshift_amount, irrshift_amount
                        ){
  if(mgmFile == "none"){
    return(0)
  }
  manFileList <- c(planting, harvest, fertilization, irrigation, grazing, mowing, thinning)
  managementTypes <- c("planting", "harvest", "fertilization", "irrigation", "cultivation", "grazing", "mowing", "thinning")
  manFileList <- grep("\\.[a-z]{3}$",manFileList,value = TRUE)
  inpFiles <- sapply(manFileList,function(x) {
                         grep("\\/tmp\\/",
                              grep(paste0("/",x),
                                   list.files(file.path(baseDir,"input/management/site/"),
                                              recursive = TRUE),value=TRUE),invert = TRUE, value=TRUE)})
  # print(inpFiles)
  destFiles <- paste0("input/management/site/",gsub("(.*)(\\/.*\\..*)","\\1/tmp\\2",inpFiles))
  sapply(destFiles, function(m){
    dir.create(dirname(file.path(baseDir,m)),showWarnings = FALSE)
  })
 Map(function(x,y){file.copy(x,y,overwrite = TRUE)}, file.path(baseDir,"input/management/site/",inpFiles), file.path(baseDir,destFiles))
 choosenRows <- managementRows[gsub(".*\\.","",inpFiles)]
 delRows <- setdiff(managementRows, choosenRows)
 managementTemplate[delRows-1] <- 0
 managementTemplate[choosenRows] <- destFiles 
  # print(destFiles)
 toWrite <- managementTemplate #[-delRows]
  # print(toWrite)
  writeLines(toWrite,file.path(baseDir,"/input/management/tmp/",mgmFile))

  plantFile <- grep("plt$",destFiles,value = TRUE)
  if(length(plantFile)!=0){
    shiftFile(file.path(baseDir,plantFile), destFile = file.path(baseDir,plantFile), shiftDate = planshift_date, varCol = 3, as.numeric(planshift_density))
  }

  harvestFile <- grep("hrv$",destFiles,value = TRUE)
  if(length(harvestFile)!=0){
    shiftFile(file.path(baseDir,harvestFile), destFile = file.path(baseDir,harvestFile), shiftDate = harvshift_date)
  }

  fertilFile <- grep("frz$",destFiles,value = TRUE)
  if(length(fertilFile)!=0){
    shiftFile(file.path(baseDir,fertilFile), destFile = file.path(baseDir,fertilFile), shiftDate = fertshift_date, varCol = 3, as.numeric(fertshift_amount))
  }

  irrFile <- grep("irr$",destFiles,value = TRUE)
  if(length(irrFile)!=0){
    shiftFile(file.path(baseDir,irrFile), destFile = file.path(baseDir,irrFile), shiftDate = irrshift_date, varCol = 2, as.numeric(irrshift_amount))
  }
  ## browser()
}

#' wrap
#' 
#' Mini helper function which provides comparison between ...

wrap<- function(x){
if(x == ""){NULL} else x
}
