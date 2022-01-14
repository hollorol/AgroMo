#' agroMoParAnaUI
#'
#' Bla
#' @param id id
#' @importFrom shiny NS tags checkboxInput selectInput textInput actionButton plotOutput updateSelectInput observe imageOutput

agroMoParAnaUI <- function(id){
  
  
  
  ns <- NS(id)
  tags$div(id = ns(id),
           
           
           tags$div(
             id = paste0(ns("paranait"),"_container"), 
             textInput(ns("paranait"),"ITERATION CYCLES:","100")
           ),
           imageOutput(ns("paranaimage")),
           tags$div(
             tableOutput(ns("resultsTable"))
           ),  
           tags$div(
             id = paste0(ns("paranaini"),"_container"),
             selectInput(ns("paranaini"),"WORKING DIRECTORY:",choices=c(""))
           ),
           tags$div(
             id = paste0(ns("paranaexp"),"_container"),
             selectInput(ns("paranaexp"),"OBSERVATION DATA file:",choices=c(""))
           ),
           #tags$div(
           #   id = paste0(ns("ctlfile"),"_container"),
           #  selectInput(ns("ctlfile"),"CONTROL file:",choices=c(""))
           #),
           tags$div(
             id = paste0(ns("ctinfo"),"_container"),
             selectInput(ns("ctinfo"),"CONSTRAINT INFO file:",choices=c(""))
           ),
           tags$div(
             id = paste0(ns("charfunc"),"_container"),
             selectInput(ns("charfunc"),"PARAMETER SET file:",choices=c("mean"))
           ),
           tags$div(
             id = paste0(ns("metfunc"),"_container"),
             selectInput(ns("metfunc"),"metric function for calibration:",choices=c("minima of RMSE"))
           ),
           tags$div(id="paranatype","TYPE OF PARAMETER ANALYSIS:"),
           tags$div(
             id = paste0(ns("paranaradio"), "_container"), 
             radioButtons(ns("paranaradio"),"",choices=c(" ", " ", " "), inline = FALSE)
           ),
           tags$div(id="paranaradsweep","Parameter Sweep"),
           tags$div(id="paranaradsens","Sensitivity Analysis"),
           tags$div(id="paranaradcal","Calibration"),
           tags$div(id=ns("cT"), checkboxInput(ns("copyThread"), "(re)create file system", value = TRUE)),
           tags$div(id = ns("Buttons"),
                    actionButton(ns("paranado"),label = "PERFORM ANALYSIS"))
           
           
           
           
  )
}

#' agroMoParAna 
#' 
#' asdfasfd
#' @param input input
#' @importFrom shiny reactiveValues observe updateSelectInput observe renderPlot renderImage renderTable
#' @importFrom DBI dbConnect


agroMoParAna <- function(input, output, session, baseDir){
 
  
  ns <- session$ns
  output$paranatable = DT::renderDataTable({valami})

  observe({
      updateSelectInput(session,"paranaini",
                        choices = list.dirs(file.path(baseDir(),"calibration"), full.names=FALSE, recursive=FALSE))
  })

  observe({
      updateSelectInput(session,"paranaexp",
                        choices = list.files(file.path(baseDir(),"calibration",input$paranaini), pattern="\\.obs$"))
  })

  observe({

      updateSelectInput(session,"ctlfile",
                        choices = list.files(file.path(baseDir(),"calibration",input$paranaini), pattern="\\.cal$"))
  })
  
  

  observeEvent(input$paranado,{
                   tryCatch({
                      setwd(file.path(baseDir(),"calibration",input$paranaini))
                      execPath <- baseDir()
                      calFileName <- input$ctlfile
                      calFileConn <- file(calFileName,"r")
                      measureFile <-  readLines(calFileConn,n=1)
                      paramFile <- readLines(calFileConn,n=1)
                      sourceDir <- readLines(calFileConn,n=1)
                      parameters <- read.csv(paramFile, stringsAsFactors=FALSE, skip=1)
                      parameters <- parameters[order(parameters[,2]),]
                      variableName <- read.table(paramFile, nrows=1, sep = ",",stringsAsFactors = FALSE)
                      measurements <- read.csv2(measureFile, stringsAsFactors=FALSE)
                      position <- seek(calFileConn)
                      calTable <- read.csv2(calFileConn, stringsAsFactor=FALSE)
                      if(nrow(calTable) == 0){
                      close(calFileConn)
                          siteLine <- readLines(calFileName)[4]
                          siteLine <- strsplit(siteLine,split=";")[[1]]
                          # siteLine[1] <-gsub("\\s*(\\S.*\\S)\\s*","\\1",siteLine[1],perl=TRUE)
                          siteLine[2] <- as.numeric(siteLine[2])
                          calTable <- data.frame("site_id"=siteLine[1],"domain_id"=siteLine[2])
                      }
                      colnames(calTable) <- c("site_id","domain_id")
                      calTable$site_id <- paste0(file.path(execPath,"input/initialization",sourceDir,calTable$site_id),".ini")
                      if(file.exists("measurement.prep")){
                          source("measurement.prep", local=TRUE)
                      }
                     dataVar <- as.numeric(options("AgroMo_centralData")[[1]][options("AgroMo_centralData")[[1]][,"VARIABLE"]==unlist(variableName),"VARCODE"])
                     names(dataVar) <- variableName
                     likelihood <- list(agroLikelihood)
                     names(likelihood) <- variableName
                     #TODO: constraints and th definition
                     const <- jsonlite::read_json("constraints.json",simplifyVector=TRUE) 
                     # constraints<- read.csv("consts.csv",stringsAsFactors=FALSE)
                     constraints <- const$constraints
                     # th <- as.numeric(readLines("th.txt")[1])
                     th <- const$treshold

                   withProgress(min=0, max=as.numeric(isolate(input$paranait), value=0, message="Calibration state"),
                                message="Calibrating...",
                                detail="Preparing processes...",{
                        results <- RBBGCMuso::multiSiteCalib(measurements = measurements,
                                                  parameters = parameters,
                                                  calTable = calTable,
                                                  dataVar = dataVar,
                                                  iterations = as.numeric(isolate(input$paranait)),
                                                  pb=NULL,
                                                  pbUpdate = function(x){setProgress(value=x,detail=x)},
                                                  likelihood = likelihood,
                                                  execPath = execPath,
                                                  copyThread = input$copyThread,
                                                  constraints = constraints,
                                                  th = th
                        )

                            })
                        setwd(baseDir())
                   }, error=function(e){
                       showNotification(sprintf("Something went wrong: %s",e$message),
                                        type="error", duration=NULL) 
                       # showNotification(e, type="error", duration=NULL) 
                       setwd(baseDir())
                   })

                         output$paranaimage <- renderImage({
                           list(src = file.path(baseDir(), "calibration", input$paranaini ,"calibRes.png"),
                               alt ="result of the calibration")
                       }, deleteFile=FALSE)

                   calDir <- file.path(baseDir(), "calibration", input$paranaini)
                   resObj <- readRDS(file.path(calDir,"results.RDS"))
                   listToExcel(resListToExcelList(resObj), file.path(calDir,"results"))
                   output$resultsTable <- renderTable({

                       data.frame(original = c(resObj[["originalMAE"]], resObj[["originalRMSE"]],
                                                                        resObj[["originalR2"]] ),
                                  calibrated = c(resObj[["MAE"]], resObj[["RMSE"]],resObj[["R2"]]),
                                  row.names = c("MAE", "RMSE", "R<sup>2</sup>"))

                   },rownames=TRUE,sanitize.text.function = function(x) x  )
  })

#  observe({
#    output$paranaTable <- renderTable(dat$querySelector,colnames=FALSE,width="100%", sanitize.text.function = function(x) x )
#  })
  
}


prepareFromAgroMo <- function(fName){
    obs <- read.table(fName, stringsAsFactors=FALSE, sep = ";", header=T)
    obs <- reshape(obs, timevar="var_id", idvar = "date", direction = "wide")
    dateCols <- apply(do.call(rbind,(strsplit(obs$date, split = "-"))),2,as.numeric)
    colnames(dateCols) <- c("year", "month", "day")
    cbind.data.frame(dateCols, obs)
}

agroLikelihood <- function(modVector,measured){
    mu <- measured[,grep("mean", colnames(measured))]
    stdev <- measured[,grep("^sd", colnames(measured))]
    ndata <- nrow(measured)
    sum(sapply(1:ndata, function(x){
                  dnorm(modVector, mu[x], stdev[x], log = TRUE)
               }), na.rm=TRUE)
}

resListToExcelList <- function (resList) {
    toExcel <- list()
    toExcel[["optimal results"]] <- data.frame(index = resList$calibrationPar,
                                               value = unlist(resList$parameter)
    )
    toExcel[["comparison"]] <- resList$comparison[,c(3, 1, 2)]
    toExcel[["error metrics"]] <- data.frame(original = c(resList[["originalMAE"]], resList[["originalRMSE"]],
                                                          resList[["originalR2"]] ),
                               calibrated = c(resList[["MAE"]], resList[["RMSE"]],resList[["R2"]]),
                               row.names = c("MAE", "RMSE", "R^2"))
    toExcel
}
