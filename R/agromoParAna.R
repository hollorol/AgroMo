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
             textInput(ns("paranait"),"number of iterations:","100")
           ),
           imageOutput(ns("paranaimage")),
#           tags$div(
#             tableOutput(ns("paranaTable"))
#          ),  
           tags$div(
             id = paste0(ns("paranaini"),"_container"),
             selectInput(ns("paranaini"),"WORKING DIRECTORY:",choices=c(""))
           ),
           tags$div(
             id = paste0(ns("paranaexp"),"_container"),
             selectInput(ns("paranaexp"),"OBSERVATION DATA file:",choices=c(""))
           ),
           tags$div(
              id = paste0(ns("ctlfile"),"_container"),
              selectInput(ns("ctlfile"),"CONTROL file:",choices=c(""))
           ),
           tags$div(
             id = paste0(ns("charfunc"),"_container"),
             selectInput(ns("charfunc"),"characterization function for sensitivity:",choices=c("mean"))
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
           tags$div(id = ns("Buttons"),
                    actionButton(ns("paranado"),label = "PERFORM ANALYSIS"))
           
           
           
           
  )
}

#' agroMoParAna 
#' 
#' asdfasfd
#' @param input input
#' @importFrom shiny reactiveValues observe updateSelectInput observe renderPlot renderImage 
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
                      calTable <- read.csv2(calFileConn, stringsAsFactor=FALSE)
                      colnames(calTable) <- c("site_id","domain_id")
                      calTable$site_id <- paste0(file.path(execPath,"input/initialization",sourceDir,calTable$site_id),"_1.ini")
                      close(calFileConn)
                      if(file.exists("measurement.prep")){
                          source("measurement.prep", local=TRUE)
                      }
                     dataVar <- as.numeric(options("AgroMo_centralData")[[1]][options("AgroMo_centralData")[[1]][,"VARIABLE"]==unlist(variableName),"VARCODE"])
                     names(dataVar) <- variableName
                     likelihood <- list(agroLikelihood)
                     names(likelihood) <- variableName

                   withProgress(min=0, max=as.numeric(isolate(input$paranait), value=0, message="Calibration state"),
                                message="Calibrating...",
                                detail="This may take a while...",{
                        results <- RBBGCMuso::multiSiteCalib(measurements = measurements,
                                                  parameters = parameters,
                                                  calTable = calTable,
                                                  dataVar = dataVar,
                                                  iterations = as.numeric(isolate(input$paranait)),
                                                  pb=NULL,
                                                  pbUpdate = function(x){setProgress(value=x,detail=x)},
                                                  likelihood = likelihood,
                                                  execPath = execPath)

                            })
                        setwd(baseDir())
                   }, error=function(e){
                       # browser()
                       showNotification("Something went wrong",type="error", duration=NULL) 
                       setwd(baseDir())
                   })
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

