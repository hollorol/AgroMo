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
    ## showModal(
    ##   modalDialog(
    ##     renderText("joska"),
    ##     renderText({"Fut a model fut"})
    ##   ,easyClose = TRUE)
    ## )
   return(dat)
  })

}

runMuso <- function(iniFile){
  system2("./muso",sprintf("input/initialization/tmp/%s",iniFile))
  settings <- setupGUI(iniFile)
}

readAndChangeIni <- function(iniFile, weatherFile, soilFile, managementFile, lines = c(4,40,46)){
  print(iniFile)
  managementPath <- if(managementFile == "no management"){managementFile} else {sprintf("input/management/%s", managementFile)}
  soilPath <- sprintf("input/soil/%s",soilFile)
  weatherPath <- sprintf("input/weather/%s",weatherFile)
  init <- tryCatch(readLines(sprintf("input/initialization/%s",iniFile)), error = function (e) "Cannot run the smodel")
  paths <- c(weatherPath,soilPath, managementPath)
  init[lines] <- paths
  dir.create("input/initialization/tmp/",showWarnings = FALSE)
  writeLines(init,sprintf("input/initialization/tmp/%s",iniFile))
}
