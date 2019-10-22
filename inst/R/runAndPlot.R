runAndPlotUI <- function(id, label){
  ns <- NS(id)
  actionButton(ns("runModel"),label)
}


runAndPlot <- function(input, output, session,baseDir,
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
    readAndChangeFiles(isolate(baseDir()), iniFile(), weatherFile(), soilFile(), managementFile(),
                       planting(), harvest(), fertilization(), irrigation(), grazing(),
                       mowing(), thinning(), planshift_date(), planshift_density(),
                       harvshift_date(), fertshift_date(),
                       irrshift_date(), fertshift_amount(), irrshift_amount())

    settings <- setupGUI(isolate(iniFile()),isolate(baseDir()))
    runModel <- reactive({future({runMuso(isolate(iniFile()),isolate(baseDir()))})})
    showModal(tags$div(id = "runIndicator", modalDialog(
                                                tags$img(id = "runningGif", src= "img/iu.gif", width = "150px"),
                                                hide(tags$img(id = "finishedGif", src= "img/iu_check.gif",width = "150px"))
      ,renderText({
        ## browser()
        runModel() %...>% {
          writeDataToEnv(settings = settings, envir = readRDS(file.path(baseDir(),"output/outputs.RDS")),binaryName=file.path(baseDir(),"output","site",sprintf("%s.dayout",settings$outputName)),outFile = file.path(baseDir(),"output/outputs.RDS"),outputName = outputName)
          dat$dataenv <- readRDS(file.path(baseDir(),"output/outputs.RDS"))
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

runMuso <- function(iniFile,baseDir){
  ## browser()
  a <- getwd()
  setwd(baseDir)
  system2("./muso",sprintf("input/initialization/site/tmp/%s",iniFile))
  setwd(a)
  settings <- setupGUI(iniFile,baseDir)
}

readAndChangeFiles <- function(baseDir, iniFile, weatherFile, soilFile, managementFile,
                               planting=NULL, harvest=NULL, fertilization=NULL, irrigation=NULL,
                               grazing=NULL, mowing=NULL, thinning=NULL,
                               planshift_date, planshift_density, harvshift_date, fertshift_date, irrshift_date, fertshift_amount, irrshift_amount
                               ){
  print(file.path(baseDir,"input","initialization","site",iniFile))
  managementPath <- if(managementFile == "none"){managementFile} else {
                                   file.path("input/management/tmp", managementFile)}
  ## managementPath <- "none"
  soilPath <- file.path("input","soil","site",soilFile)
  weatherPath <- file.path("input","weather","site",weatherFile)
  init <- tryCatch(readLines(file.path(baseDir,"input","initialization","site",iniFile)), error = function (e) stop("Cannot run the smodel"))
  lines <- c(grep("MET_INPUT",init),  grep("SOIL_FILE",init),grep("MANAGEMENT_FILE",init)) + 1
  paths <- c(weatherPath,soilPath, managementPath)
  init[lines] <- paths
  dir.create(file.path(baseDir,"input/initialization/site/tmp/"),showWarnings = FALSE)
  dir.create(file.path(baseDir,"input/management/tmp/"),showWarnings = FALSE)
  managementTypes <- c("planting", "harvest", "fertilization", "irrigation", "cultivation", "grazing", "mowing", "thinning")
  sapply(managementTypes,function(man){
    dir.create(file.path(baseDir,"input/management/tmp/",man),showWarnings = FALSE)
  })
  changingMGM(managementFile,baseDir, planting, harvest, fertilization, irrigation, grazing, mowing, thinning,
              planshift_date, planshift_density, harvshift_date, fertshift_date, irrshift_date, fertshift_amount, irrshift_amount
              )
  writeLines(init,file.path(baseDir,"input/initialization/site/tmp/",iniFile))
}

shiftFile <- function(manFile, destFile, shiftDate, varCol=NULL, shiftVar=NULL){
## browser()
  manDT <- fread(manFile)
  manDT[,DATE:=format.Date(as.Date(DATE,format="%Y.%m.%d")+as.numeric(shiftDate),format="%Y.%m.%d")]
  if(is.null(varCol)){
    fwrite(manDT,destFile,sep = " ")
  } else {
    varName <- colnames(manDT)[varCol]
    manDT[,eval(varName):=eval(parse(text = varName))+shiftVar]
    fwrite(manDT,destFile,sep = " ")
  }
}

managementTemplate <- c("MANAGEMENT_INFORMATION MuSo6", "-", "PLANTING", "1", "input/management/planting/maize_mono.plt", "-", "THINNING", "1", "blubancsek.thn", "-", "MOWING", "1", "input/mowing/mow/mov.mow", "-", "GRAZING", "1", "input/management/grazin/bla.grz", "-", "HARVESTING", "1", "input/management/harvest/maize_mono.hrv", "-", "PLOUGHING", "1", "bibcsek.plt", "-", "FERTILIZING", "1", "input/management/fertilization/maize_mono.frt", "-", "IRRIGATING", "1", "input/management/irrigation/maize.irr")
managementRows <- c("plt" = 5, "thn" =  9, "mow" = 13, "grz"= 17, "hrv"= 21, "plo" = 25, "frz" = 29, "irr" = 33)
changingMGM <- function(mgmFile, baseDir, planting=NULL, harvest=NULL, fertilization=NULL, irrigation=NULL, grazing=NULL, mowing=NULL, thinning=NULL,
                        planshift_date, planshift_density, harvshift_date, fertshift_date, irrshift_date, fertshift_amount, irrshift_amount
                        ){
  if(mgmFile == "none"){
    return(0)
  }
  manFileList <- c(planting, harvest, fertilization, irrigation, grazing, mowing, thinning)
  managementTypes <- c("planting", "harvest", "fertilization", "irrigation", "cultivation", "grazing", "mowing", "thinning")
  manFileList <- grep("\\.[a-z]{3}$",manFileList,value = TRUE)
  ## browser()
  inpFiles <- sapply(manFileList,function(x) grep("\\/tmp\\/",grep(x,list.files(baseDir, recursive = TRUE),value=TRUE),invert = TRUE, value=TRUE))
  destFiles <- gsub("(.*)(\\/.*\\..*)","\\1/tmp\\2",inpFiles)
  sapply(destFiles, function(m){
    dir.create(dirname(file.path(baseDir,m)),showWarnings = FALSE)
  })
 Map(function(x,y){file.copy(x,y,overwrite = TRUE)}, file.path(baseDir,inpFiles), file.path(baseDir,destFiles))
 choosenRows <- managementRows[gsub(".*\\.","",inpFiles)]
 delRows <- setdiff(managementRows, choosenRows)
 managementTemplate[delRows-1] <- 0
 managementTemplate[choosenRows] <- destFiles 
  print(destFiles)
 toWrite <- managementTemplate #[-delRows]
  print(toWrite)
  writeLines(toWrite,file.path(baseDir,"/input/management/tmp/",mgmFile))

  plantFile <- grep("plt$",destFiles,value = TRUE)
  if(length(plantFile)!=0){
    shiftFile(file.path(baseDir,plantFile), destFile = file.path(baseDir,plantFile), shiftDate = planshift_date, varCol = 3, as.numeric(planshift_density))
  }

  harvestFile <- grep("hrv$",destFiles,value = TRUE)
  if(length(harvestFile)!=0){
    shiftFile(file.path(baseDir,harvestFile), destFile = file.path(baseDir,harvestFile), shiftDate = harvshift_date)
  }

  fertilFile <- grep("frt$",destFiles,value = TRUE)
  if(length(fertilFile)!=0){
    shiftFile(file.path(baseDir,fertilFile), destFile = file.path(baseDir,fertilFile), shiftDate = fertshift_date, varCol = 3, as.numeric(fertshift_amount))
  }

  irrFile <- grep("irr$",destFiles,value = TRUE)
  if(length(irrFile)!=0){
    shiftFile(file.path(baseDir,irrFile), destFile = file.path(baseDir,irrFile), shiftDate = irrshift_date, varCol = 2, as.numeric(irrshift_amount))
  }
  ## browser()
}

wrap<- function(x){
if(x == ""){NULL} else x
}
