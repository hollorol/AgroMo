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
             textInput(ns("paranait"),"number of iterations:","")
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
             radioButtons(ns("paranaradio"),"",choices=c("Parameter Sweep","Sensitivity Analysis","Calibration"), inline = TRUE)
           ),
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
                   inputLoc <- file.path(isolate(baseDir()), "calibration",isolate(input$paranaini))
                   settings <- RBBGCMuso::setupMuso(inputLoc=inputLoc)
                   centralData <- getOption("AgroMo_centralData")
                   obs <- prepareFromAgroMo(file.path(inputLoc,isolate(input$paranaexp)))
                   obs[,5:8] <- obs[,5:8] /10000
                   parameterLocation <- file.path(inputLoc, isolate(input$ctlfile)) 
                   parameters <- read.csv(parameterLocation, stringsAsFactors=FALSE, skip=1)
                   agroVarName = readLines(parameterLocation, n=1)
                   musoCode = as.numeric(centralData$VARCODE[centralData$VARIABLE == agroVarName])
                   dataVar <- musoCode
                   names(dataVar) <- agroVarName
                   likelihood <- list()
                   likelihood[[agroVarName]] <- agroLikelihood
                   # browser()
                   png(file.path(inputLoc, "calibResult.png"))
                   withProgress(min=0, max=as.numeric(isolate(input$paranait), value=0, message="Calibration state"),
                                message="Calibrating...",
                                detail="This may take a while...",{
                       RBBGCMuso::calibrateMuso(measuredData = obs,
                                     settings=settings,
                                     dataVar = dataVar,
                                     parameters=parameters,
                                     likelihood = likelihood,
                                     outputLoc=inputLoc,
                                     iterations=as.numeric(isolate(input$paranait)),
                                     method="agromo", lg = TRUE, pb=NULL,
                                     pbUpdate=function(x)(setProgress(value=x,detail=x)))
                   })
                   dev.off()
                   output$paranaimage <- renderImage({
                      print(file.path(inputLoc, "calibResult.png"))
                       list(src =file.path(inputLoc, "calibResult.png"),
                            contentType="image/png+xml"
                            width=920,
                            height=340,
                            alt ="result of the calibration")
                   }, deleteFile=FALSE)

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

