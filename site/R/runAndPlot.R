runAndPlotUI <- function(id, label){
  ns <- NS(id)
  actionButton(ns("runModel"),label)
}


runAndPlot <- function(input, output, session, iniFile, weatherFile, soilFile, managementFile, outputName){
  ##preparation
dat<-reactiveValues(dataenv = NULL)
###if output is already done
  observeEvent(input$runModel,{
    readAndChangeIni(iniFile(), weatherFile(), soilFile(), managementFile())
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

readAndChangeIni <- function(iniFile, weatherFile, soilFile, managementFile, lines = c(4,40,46)){
  print(iniFile)
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

  writeLines(init,sprintf("input/initialization/tmp/%s",iniFile))
}

shiftFile <- function(manFile, destFile, shiftDate, varName, shiftVar){

  manDT <- fread(manFile) 
  manDT[,DATE:=format.Date(as.Date(DATE,format="%Y.%m.%d")+shiftDate,format="%Y.%m.%d")]
  if(is.null(varName)){
    fwrite(manDT,destFile,sep = " ")
  } else {
    manDT[,eval(varName):=eval(parse(text = varName))+shiftVar]
    fwrite(manDT,destFile,sep = " ")
  }
}
