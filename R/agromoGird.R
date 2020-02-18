  agroMoGridUI <- function(id){
    ns <- NS(id)
    tags$div(id = ns(id),
    tags$div(
    id = paste0(ns("ensclim"),"_container"),
    checkboxInput(ns("ensclim"), label = "Ensemble", value = FALSE)
    ),
    tags$div(
    id =paste0(ns("ensalg"),"_container"),
    checkboxInput(ns("ensalg"), label = "Ensemble", value = FALSE)
    ),         
    tags$div(
      id = paste0(ns("gridres"),"_container"),
      selectInput(ns("gridres"),"GRID RESOLUTION:",choices = c("10×10 km"))
    ),
    tags$div(
      id = paste0(ns("climproj"),"_container"),
      selectInput(ns("climproj"),"CLIMATE PROJECTION:",NA)
    ),
    tags$div(
      id = paste0(ns("algosel"),"_container"),
      selectInput(ns("algosel"),"ALGORYTHM SELECTION:",choices=c("PHOTOS: Farquhar | PET: Penman-Monteith | WSTRESS: WCBased",
                                                                "PHOTOS: Farquhar | PET: Priestly-Taylor | WSTRESS: WCBased",
                                                                "PHOTOS: Farquhar | PET: Penman-Monteith | WSTRESS: TransDemBased",
                                                                "PHOTOS: Farquhar | PET: Priestly-Taylor | WSTRESS: TransDemBased ",
                                                                "PHOTOS: DSSAT | PET: Penman-Monteith | WSTRESS: WCBased",
                                                                "PHOTOS: DSSAT | PET: Priestly-Taylor | WSTRESS: WCBased",
                                                                "PHOTOS: DSSAT | PET: Penman-Monteith | WSTRESS: TransDemBased",
                                                                "PHOTOS: DSSAT | PET: Priestly-Taylor | WSTRESS: TransDemBased"
                                                                ))
    ),    
    tags$div(
      id = paste0(ns("story"),"_container"),
      selectInput(ns("story"),"STORYLINE:",choices=c(""))
    ),     
    tags$div(
      id = paste0(ns("outsq"),"_container"),
      textInput(ns("outsq"),"OUTPUT SQLite TABLE:",NA)
    ),   
    tags$div(
      id = paste0(ns("alias"),"_container"),
      textOutput(ns("alias"))
    ), 
    tags$div(
      id = paste0(ns("queryalias"),"_container"),
      textInput(ns("queryalias"), "QUERY ALIAS:",NA)
    ),
    tags$div(
      id = paste0(ns("metadata"),"_container"),
      textInput(ns("metadata"), "DESCRIPTION:",NA)
    ),
    
    tags$div(id = ns("Buttons"),
    actionButton(ns("StartSim"),label = "START SIMULATION"),
    actionButton(ns("RunQuery"),label = "START QUERY"),
    actionButton(ns("Map"),label="MAP")),             
     
    tags$div(
      id = paste0(ns("time"),"_container"),
      selectInput(ns("time"),"TIME SLICE [start-end]:",choices=c(""))
    ),                
    tags$div(
      id = paste0(ns("until"),"_container"),
      selectInput(ns("until"),"-",choices=c(""))
    ),  
    do.call(tagList,(
                      lapply(1:9,function(x){
                                 tags$div(
                                          id= ns(sprintf("sqlfunc_%s_container",x)),
                                          selectInput(ns(sprintf("sqlfunc_%s",x)),sprintf("{%s}:",x),choices=c("NA"))
                                 )
                      })
    )),
  
tags$div(id="query","QUERIES:"),

tags$div(
        tableOutput(ns("queryTable"))
  )  
    ,
    tags$script(HTML( sprintf(
                                         "
                                         $('#%s').on('click','td', function(){
                                            $('#%s td').removeClass();
                                            $(this).toggleClass('griddiv-selected-vars');
                                            let selections = getIndexesForSelection('#%s','.griddiv-selected-vars');
                                            if(selections.length <= 5){
                                                Shiny.onInputChange('%s',selections);
                                            } else {
                                                $(this).toggleClass('griddiv-selected-vars');

                                            }
                                           //  console.log(getIndexesForSelection('.showdiv-selected-vars'))
                                         })

                                         ",ns("queryTable"),ns("queryTable"),ns("queryTable"),ns("queryList"))
                                          )),
    tags$script(HTML("
                                        Shiny.addCustomMessageHandler('refreshSelected',function(message){
                                           $('#griddiv-queryTable tr:nth-child('+Number(message.indexOfQuery)+')').html(message.querySentence);
                                        }
                                        )  
                             
                             "))
    )                      
  }

#' agromoGrid
#'
#' This is agromoGrid main function
#' @importFrom jsonlite read_json
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW 
#' @importFrom foreach foreach %dopar%

agroMoGrid <- function(input, output, session,baseDir){
    language <- "en"
    ns <- session$ns
    dat <- reactiveValues(storyVars=NULL,
                          storyCSV=NULL,
                          storyTimeRange=NULL,
                          jsonList=NULL,
                          storyFiles=list(),
                          queryNames=NULL,
                          jsonOptions=NULL,
                          jsonNumbers=NULL,
                          querySelector=NULL,
                          queries=NULL,
                          language="en")

    observe({
        dat$jsonList <- lapply((list.files(path=file.path(baseDir(),"templates/grid"),pattern="*.json", full.names=TRUE)),read_json)
        # browser()
        dat$queryNames <-  sapply(dat$jsonList,function(x) x$Names[[dat$language]])
        dat$queries <-  sapply(dat$jsonList,function(x) x$query)
        dat$replNumbers <- sapply(dat$queryNames,getReplacementNumbers)
        # browser()
        dat$firstOptions <- lapply(dat$jsonList,function(x) {unlist(lapply(x$optionAlias[[dat$language]],function(y){y[1]}))}) 
        
        dat$querySelector <- as.data.frame(colorReplacements(unlist(lapply(seq_along(dat$replNumbers),function(i){
                                                              # if(i==8) browser()
                                                              interpolateInto(dat$replNumbers[[i]],dat$firstOptions[[i]],dat$queryNames[i]) 
                                 }))),stringsAsFactors=FALSE)
        
        dat$options <- sapply(1:9,function(queryIndex)(sapply(dat$jsonList[[queryIndex]]$options,unlist)))

    }) 

    # observe({
    #     dat$querySelector <- as.data.frame(colorReplacements(dat$queryNames),stringsAsFactors=FALSE)
    # })



    #dat[["dataenv"]] <-readRDS("output/outputs.RDS")
    # queryNames <- ls(dat$dataenv)

    observe({
        dat$storyFiles <- grep(".*\\.story",list.files(file.path(baseDir(),"input","storyline"), full.names=TRUE), value=TRUE)
        dat$storyOptions <- tools::file_path_sans_ext(basename(dat$storyFiles))
        updateSelectInput(session,"story",choices=dat$storyOptions) 
    })

    observe({
        projections <- basename(list.dirs(file.path(baseDir(),"input/weather/grid/projection"))[-1])
        if(length(projections)!=0){
            updateSelectInput(session,"climproj",choices=projections)
        }
    })

    observeEvent(input$story,{
                     if(input$story!=""){
                         choosenStoryFile <- dat$storyFiles[match(input$story,dat$storyOptions)]
                         output$alias <- renderText({readLines(choosenStoryFile,n=1)})
                         dat$storyVars <- as.character(read.table(choosenStoryFile,skip=1, nrows=1, sep=";",stringsAsFactors=FALSE))
                         dat$storyCSV <- read.table(choosenStoryFile,skip=2, sep=";",stringsAsFactors=FALSE)
                         dat$storyTimeRange <- range(dat$storyCSV[,c(3,4)])
                         storyRow <- as.data.frame((function(x){
                                                     list(site=x[,1],
                                                          name=apply(x,1,function(y){paste(y[1:2],collapse="_")}),
                                                          numDays=365*(x[,4]-x[,3]+1))
                                                })(dat$storyCSV),stringsAsFactor=FALSE)
                         dat$story <-split(storyRow,storyRow$site)
                         # browser()
                        
                         # sites <- split(dat$storyCSV, dat$storyCSV[,1])
                         # dat$numYears <- as.numeric(lapply(sites,function(m){
                         #                        m[nrow(m),4] - m[1,3] + 1
                         # }))
                     }
    })

    observe({
        if(input$story != ""){
            updateSelectInput(session,"time",choices=dat$storyTimeRange[1]:dat$storyTimeRange[2], selected=dat$storyTimeRange[1])
            updateSelectInput(session,"until",choices=dat$storyTimeRange[1]:dat$storyTimeRange[2], selected=dat$storyTimeRange[2])
        }
    })

    observeEvent(input$time,{
        if(input$time!=""){
            updateSelectInput(session,"until",choices=input$time:dat$storyTimeRange[2], selected=dat$storyTimeRange[2])
        }
    })
    observe({
        if(!is.null(input$queryList)){
            a <- dat$queryNames
             sapply(1:9,function(x){
                        choices <- unlist(dat$jsonList[[input$queryList]]$optionAlias[[dat$language]][[as.character(x)]], use.names=TRUE)
                        if(is.null(choices)){
                            choices <- "NA"
                        }
                        updateSelectInput(session,sprintf("sqlfunc_%s",x),
                                          choices=choices)
                      })
        }
        # session$sendCustomMessage("refreshSelected",input$queryList)
    })

    observe({
        inputs <- sapply(1:9,function(x){input[[sprintf("sqlfunc_%s",x)]]})
        if(!is.null(isolate(input$queryList))){
            indexOfQuery <- isolate(input$queryList)
            # browser()
            newQuerySentence <- interpolateInto(seq_along(isolate(dat$replNumbers[[indexOfQuery]])),
                                        inputs[seq_along(isolate(dat$replNumbers[[indexOfQuery]]))]
                                                ,isolate(dat$queryNames)[indexOfQuery])
            if((input$time!="")&&(input$until!="")){
                newQuerySentence <- gsub("\\[T-T\\]",sprintf("[%s-%s]",input$time,input$until),newQuerySentence)
            }
            newQuerySentence <- sprintf("<td class=\"griddiv-selected-vars\">%s</td>",colorReplacements(newQuerySentence))
            # dat$querySelector[indexOfQuery,1] <- colorReplacements(newQuerySentence)
            session$sendCustomMessage("refreshSelected",list(indexOfQuery=indexOfQuery, querySentence=newQuerySentence))
            # dat$querySelector<- isolate(dat$querySelector)[-1,1]
        }
    
    })

    # output$queryTable <- DT::renderDataTable (tabe,options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = FALSE, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
    observe({
# data.frame(c("<span class=\"reddi\">{1:mean}</span> {2:annual} yield {3:max} in the [start-end] period", "{1:max} {2:annual} lai {3:max} in the [start-end] period", "{1:mean} {2:may} {3:0-3 cm} soiltemp {4:mean} in the <span class=\"timeSlice\">[start-end]</span> period"))
        output$queryTable <- renderTable(dat$querySelector,colnames=FALSE,width="100%", sanitize.text.function = function(x) x )
    }) 
    #    output$queryTable <- DT::renderDataTable({
    observeEvent(input$RunQuery,{
                     # browser()
                    queryIndex <- input$queryList
                    sqlSentence <- dat$queries[input$queryList]
                    optionList <- sapply(1:9,function(x){input[[sprintf("sqlfunc_%s",x)]]}) # These are just the optionAliaces
                    possibilities <- lapply(dat$jsonList[[queryIndex]]$optionAlias[[dat$language]],unlist)
                    optionList <- optionList[optionList!="NA"]
                    selectedNum <- (sapply(seq_along(optionList),function(i){match(optionList[i],possibilities[[i]])}))
                    textContent <- sapply(seq_along(selectedNum),function(i){
                                               dat$options[[i]][selectedNum[i]]
                                                })
                    sentenceToSQL<- interpolateInto(dat$replNumbers[[input$queryList]],textContent,sqlSentence,TRUE)
                    writeLines(c(sprintf("/*%s*/",input$metadata),"\n\n",sentenceToSQL),file.path(baseDir(),"output/queries",sprintf("%s.sql",input$queryalias)))
    })

    # DT::datatable(data.frame(outputName = queryNames), options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = 600, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
    #}) 

    observeEvent(input$StartSim,{

        showNotification("Starting simulation... Removing previous .dayout files")
        suppressWarnings(file.remove(list.files(file.path(baseDir(),"output/grid",input$story),full.names=TRUE)))

        showNotification("Setting climate projections and algorithms")
        # indexOfRows <- 
        # replacement(c("/"))
        # changeFilesWithRegex(list.files(file.path(baseDir(),"output/initialization",input$story),full.names=TRUE),
                             # indexOfRows,replacements,regex)

        browser()
                         # runChain(baseDir(),input$story,dat$story[[5]])
        runGrid(baseDir(),input$story,dat$story)
    })
}
  
  
changeFilesWithRegex <- function (iniFiles, indexOfRows, replacements, regex=NULL) {
    while(length(regex) < length(replacements)) {
        regex <- c(regex,"")
    }
    for(i in iniFiles){
        a <- readLines(i)
        if(is.null(regex)){
            Map(function(x,y){a[x] <<- y}, indexOfRows, replacements)
            writeLines(text=a, con = i)
        } else {
            Map(function(x,ind){
                    if(regex[ind]!=""){
                        a[x] <<- gsub(regex[ind],replacements[ind],a[x])
                    } else {
                    # browser()
                        a[x] <<-as.character(replacements[ind])
                    }
                },
                indexOfRows, seq_along(replacements))
            writeLines(text=a, con = i)
        }
    }
}


getReplacementNumbers <- function (baseString) {
    atomicRes <- suppressWarnings(as.numeric(gsub("(.*)\\{(\\d)\\}(.*)","\\2",baseString,perl=TRUE)))
    if(is.na(atomicRes)){
        return(numeric(0))
    } else {
        return(c(getReplacementNumbers(gsub("(.*)\\{(\\d)\\}.*","\\1",baseString,perl=TRUE)),atomicRes))
    }
}

interpolateInto <- function(places, strings, jsonstring, plain=FALSE){
    for(i in seq_along(places)){
        jsonstring <- gsub(sprintf("(\\{%s\\})",places[i]), ifelse(!plain,sprintf("{%s: %s}", i, strings[places[i]]),
                                                                   sprintf("%s",strings[places[i]])),jsonstring)
    }

    return(jsonstring)
}

interpolateArray <- function (jsonlist,x) {
    jsonIndex<- x$jsonIndex
    jsonOptions <- x$jsonOptions
    interpolateInto(getReplacementNumbers(jsonlist[[jsonIndex]]$query), jsonOptions, jsonlist[[jsonIndex]]$query)
}

colorReplacements <- function(stringVector){
    stringVector <- gsub("(\\{.*?\\})","<span class=\"reddi\">\\1</span>",stringVector)
    stringVector <- gsub("(\\[.*?\\-.*?\\])","<span class=\"timeSlice\">\\1</span>",stringVector)
    return(stringVector)
}

queryCreator <- function(fileN, description, index, optis, connectionBase, dat){
    browser()
   interpolateInto(dat$replNumbers[[index]],optis,dat$query)
}

runChain <- function (baseDir, storyName, chainMatrix) {
   setwd(baseDir) 
   returnVal <- apply(chainMatrix,1,function(x){
                      suppressWarnings(system2("./muso",
                                            file.path(baseDir,"input/initialization/grid",storyName,paste0(x[2],".ini")),
                                            stderr=NULL,stdout=NULL))
                }) 
   names(returnVal) <- chainMatrix[,2]
   returnVal
}

#' runGrid
#'
#' This is the parallel executer
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW 
#' @importFrom foreach foreach %dopar%

runGrid <- function(baseDir,storyName,chainMatrixFull){
    showNotification(sprintf("Starting cluster for parallel run with %s cores, please wait for the progress indicator",detectCores()-1))
    cl <- makeCluster(detectCores()-1)
    registerDoSNOW(cl)
    iterations <- length(chainMatrixFull)
    progress <- function(i) incProgress(1/iterations, detail = paste("Doing part", i))
    opts <- list(progress = progress)
    withProgress(message = "simulation", value = 0, {
                     result <- foreach(i = 1:length(chainMatrixFull), .export="runChain", .combine = c, 
                                       .options.snow = opts) %dopar% {
                        runChain(baseDir,storyName, chainMatrixFull[[i]])
                     }
    })
    stopCluster(cl)
    print(result)
}
