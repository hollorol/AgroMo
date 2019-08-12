runAndPlotUI <- function(id, label){
  ns <- NS(id)
  actionButton(ns("runModel"),label)
}


runAndPlot <- function(input, output, session,
                       iniFile, weatherFile, soilFile, managementFile, outputName,
                       planting, harvest, fertilization, irrigation, grazing, mowing,
                       thinning, planshift_date, planshift_density, harvshift_date,
                       fertshift_date, irrshift_date, fertshift_amount, irrshift_amount
                       ){
  ##preparation
dat<-reactiveValues(dataenv = NULL)
###if output is already done
  observe({
    print(irrshift_amount())
})
  observeEvent(input$runModel,{

    
    ## browser()
    readAndChangeFiles(iniFile(), weatherFile(), soilFile(), managementFile(),
                       planting(), harvest(), fertilization(), irrigation(), grazing(),
                       mowing(), thinning(), planshift_date(), planshift_density(),
                       harvshift_date(), fertshift_date(),
                       irrshift_date(), fertshift_amount(), irrshift_amount())

    settings <- setupGUI(iniFile())
    runModel <- reactive({future(runMuso(isolate(iniFile())))})
    showModal(tags$div(id = "runIndicator", modalDialog(
                                                tags$img(id = "runningGif", src= "img/iu.gif", width = "150px"),
                                                hide(tags$img(id = "finishedGif", src= "img/iu_check.gif",width = "150px"))
      ,renderText({
        runModel() %...>% {
          writeDataToEnv(settings = settings, envir = readRDS("output/outputs.RDS"),outFile = "output/outputs.RDS",outputName = outputName)
          dat$dataenv <- readRDS("output/outputs.RDS")
          removeUI(selector = "#runningGif")
          show("finishedGif")
          "Finished"
        }

      }),
      footer = NULL,
      easyClose = TRUE
    ))
    )
  })

}

runMuso <- function(iniFile){
  system2("./muso",sprintf("input/initialization/tmp/%s",iniFile))
  settings <- setupGUI(iniFile)
}

readAndChangeFiles <- function(iniFile, weatherFile, soilFile, managementFile,
                               planting=NULL, harvest=NULL, fertilization=NULL, irrigation=NULL,
                               grazing=NULL, mowing=NULL, thinning=NULL,
                               planshift_date, planshift_density, harvshift_date, fertshift_date, irrshift_date, fertshift_amount, irrshift_amount
                               ){
  print(iniFile)
  lines = c(4,40,46)
  managementPath <- if(managementFile == "no management"){managementFile} else {sprintf("input/management/tmp/%s", managementFile)}
  soilPath <- sprintf("input/soil/%s",soilFile)
  weatherPath <- sprintf("input/weather/%s",weatherFile)
  init <- tryCatch(readLines(sprintf("input/initialization/%s",iniFile)), error = function (e) "Cannot run the smodel")
  paths <- c(weatherPath,soilPath, managementPath)
  init[lines] <- paths
  dir.create("input/initialization/tmp/",showWarnings = FALSE)
  dir.create("input/management/tmp/",showWarnings = FALSE)
  managementTypes <- c("planting", "harvest", "fertilization", "irrigation", "cultivation", "grazing", "mowing", "thinning")
  sapply(managementTypes,function(man){
    dir.create(sprintf("input/management/%s/tmp/",man),showWarnings = FALSE)
  })
  changingMGM(managementFile, planting, harvest, fertilization, irrigation, grazing, mowing, thinning,
              planshift_date, planshift_density, harvshift_date, fertshift_date, irrshift_date, fertshift_amount, irrshift_amount
              )
  writeLines(init,sprintf("input/initialization/tmp/%s",iniFile))
}

shiftFile <- function(manFile, destFile, shiftDate, varCol=NULL, shiftVar=NULL){
## browser()
  manDT <- fread(manFile)
  manDT[,DATE:=format.Date(as.Date(DATE,format="%Y.%m.%d")+as.numeric(shiftDate),format="%Y.%m.%d")]
  if(is.null(varName)){
    fwrite(manDT,destFile,sep = " ")
  } else {
    varName <- colnames(manDT)[varCol]
    manDT[,eval(varName):=eval(parse(text = varName))+shiftVar]
    fwrite(manDT,destFile,sep = " ")
  }
}

managementTemplate <- c("MANAGEMENT_INFORMATION MuSo6", "-", "PLANTING", "1", "input/management/planting/maize_mono.plt", "-", "THINNING", "1", "blubancsek.thn", "-", "MOWING", "1", "input/mowing/mow/mov.mow", "-", "GRAZING", "1", "input/management/grazin/bla.grz", "-", "HARVESTING", "1", "input/management/harvest/maize_mono.hrv", "-", "PLOUGHING", "1", "bibcsek.plt", "-", "FERTILIZING", "1", "input/management/fertilization/maize_mono.frt", "-", "IRRIGATING", "1", "input/management/irrigation/maize.irr")
managementRows <- c("plt" = 5, "thn" =  9, "mow" = 13, "grz"= 17, "hrv"= 21, "plo" = 25, "frt" = 29, "irr" = 33)
changingMGM <- function(mgmFile, planting=NULL, harvest=NULL, fertilization=NULL, irrigation=NULL, grazing=NULL, mowing=NULL, thinning=NULL,
                        planshift_date, planshift_density, harvshift_date, fertshift_date, irrshift_date, fertshift_amount, irrshift_amount
                        ){
  if(mgmFile == "no management"){
    return(0)
  }
  manFileList <- c(planting, harvest, fertilization, irrigation, grazing, mowing, thinning)
  manFileList <- grep("\\.[a-z]{3}$",manFileList,value = TRUE)
  inpFiles <- sapply(manFileList,function(x) grep("\\/tmp\\/",grep(x,list.files(".", recursive = TRUE),value=TRUE),invert = TRUE, value=TRUE))
  destFiles <- gsub("(.*)(\\/.*\\..*)","\\1/tmp\\2",inpFiles)
  ## sapply(seq_along(inpFiles),function(i){
  ##   file.copy(inpFiles[i],destFiles[i],overwrite = TRUE)
  ## })
  ## browser()
 Map(function(x,y){file.copy(x,y,overwrite = TRUE)}, inpFiles, destFiles)
 choosenRows <- managementRows[gsub(".*\\.","",inpFiles)]
 delRows <- setdiff(managementRows, choosenRows)
 managementTemplate[delRows-1] <- 0
 managementTemplate[choosenRows] <- destFiles 
  print(destFiles)
 toWrite <- managementTemplate[-delRows]
  print(toWrite)
  writeLines(toWrite,sprintf("./input/management/tmp/%s",mgmFile))

  plantFile <- grep("plt$",destFiles,value = TRUE)
  if(length(plantFile)!=0){
    shiftFile(plantFile, destFile = plantFile, shiftDate = planshift_date, varCol = 3, as.numeric(planshift_density))
  }

  harvestFile <- grep("hrv$",destFiles,value = TRUE)
  if(length(harvestFile)!=0){
    shiftFile(harvestFile, destFile = harvestFile, shiftDate = harvshift_date)
  }

  fertilFile <- grep("frt$",destFiles,value = TRUE)
  if(length(fertilFile)!=0){
    shiftFile(fertilFile, destFile = fertilFile, shiftDate = fertshift_date, varCol = 3, as.numeric(fertshift_amount))
  }

  irrFile <- grep("irr$",destFiles,value = TRUE)
  if(length(irrFile)!=0){
    shiftFile(irrFile, destFile = irrFile, shiftDate = irrshift_date, varCol = 2, as.numeric(irrshift_amount))
  }
}

wrap<- function(x){
if(x == ""){NULL} else x
}
