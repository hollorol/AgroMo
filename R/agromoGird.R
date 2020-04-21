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
      id =paste0(ns("enssoil"),"_container"),
      checkboxInput(ns("enssoil"), label = "Ensemble", value = FALSE)
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
      id = paste0(ns("soildb"),"_container"),
      selectInput(ns("soildb"),"SOIL DATABASE:",NA)
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
      textInput(ns("outsq"),"OUTPUT DATA TABLE:",NA)
    ),   
    tags$div(
      id = paste0(ns("alias"),"_container"),
      textInput(ns("alias"),"",NA)
    ), 
    tags$div(
      id = paste0(ns("queryalias"),"_container"),
      textInput(ns("queryalias"), "QUERY ALIAS:",NA)
    ),
    tags$div(
      id = paste0(ns("metadata"),"_container"),
      textInput(ns("metadata"), "DESCRIPTION:",NA)
    ),
    tags$div(
      id = paste0(ns("climprojquery"),"_container"),
      selectInput(ns("climprojquery"),"CLIMATE PROJECTION FOR QUERY:",NA)
    ),
    tags$div(
      id = paste0(ns("algselquery"),"_container"),
      selectInput(ns("algselquery"),"ALGORYTHM SELECTION FOR QUERY:",NA)
    ),
    
    tags$div(id = ns("Buttons"),
    actionButton(ns("StartSim"),label = "START SIMULATION"),
    actionButton(ns("RunQuery"),label = "START QUERY"),
    actionButton(ns("Map"),label="MAP")),             
     
    tags$div(
      id = paste0(ns("time"),"_container"),
      selectInput(ns("time"),"[start-end]:",choices=c(""))
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
#' @importFrom DBI dbExecute dbGetQuery dbConnect dbDisconnect

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
    vari <- reactiveValues()
    toreturn <- reactiveValues(showMap=NULL)
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
         # dat$options <-  sapply(1:9,function(queryIndex)(sapply(dat$jsonList[[queryIndex]]$options,unlist)))
        dat$options <- sapply(1:9,function(queryIndex) lapply(dat$jsonList[[queryIndex]]$options,unlist))

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
                         # browser()
                         choosenStoryFile <- dat$storyFiles[match(input$story,dat$storyOptions)]
                         suppressWarnings(dir.create(file.path(baseDir(),"output/grid/",input$story)))
                         suppressWarnings(dir.create(file.path(baseDir(),"endpoint/grid/",input$story)))
                         output$alias <- renderText({readLines(choosenStoryFile,n=1)})
                         dat$storyVars <- as.character(read.table(choosenStoryFile,skip=1, nrows=1, sep=";",stringsAsFactors=FALSE))
                         dat$storyCSV <- read.table(choosenStoryFile,skip=2, sep=";",stringsAsFactors=FALSE)
                         dat$storyTimeRange <- range(dat$storyCSV[,c(3,4)])
                         storyRow <- as.data.frame((function(x){
                                                     list(site=x[,1],
                                                          name=apply(x,1,function(y){paste(y[1:2],collapse="_")}),
                                                          startYear=x[,3],
                                                          endYear=x[,4],
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
         dbDir <- file.path(baseDir(),"output/DB/grid/")
         dir.create(dbDir, showWarnings=FALSE)
         if(dir.exists(dbDir)){
             sqlDB <- DBI::dbConnect(RSQLite::SQLite(),file.path(dbDir,"grid.sqlite3"))
             dat[["modelOutputs"]] <-grep("_error$",dbListTables(sqlDB),invert=TRUE,value=TRUE)
             dbDisconnect(sqlDB)
         }
    })


    observe({
        if(input$story != ""){
          if(!is.null(dat$storyTimeRange)){
            updateSelectInput(session,"time",choices=dat$storyTimeRange[1]:dat$storyTimeRange[2], selected=dat$storyTimeRange[1])
            updateSelectInput(session,"until",choices=dat$storyTimeRange[1]:dat$storyTimeRange[2], selected=dat$storyTimeRange[2])
          }
 
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
                        # choices <- unlist(dat$jsonList[[input$queryList]]$options[[dat$language]][[as.character(x)]], use.names=TRUE)
                        if(is.null(choices)){
                            choices <- "NA"
                        }

                        if(grepl("\\*.*\\*",choices[1])){
                            starVar <- gsub("\\*(.*)\\*","\\1",choices[1])
                            choices <- eval(parse(text=sprintf("%s_get(baseDir())",starVar))) # Evaluate starVar void function. These are defined at the bottom of this file.
                            vari[[starVar]] <- choices # Evaluate starVar void function. These are defined at the bottom of this file.
                        }

                        updateSelectInput(session,sprintf("sqlfunc_%s",x),
                                          choices=choices)
                      })
        }
    })

    observe({
        inputs <- sapply(1:9,function(x){input[[sprintf("sqlfunc_%s",x)]]})
        if(!is.null(isolate(input$queryList))){
            indexOfQuery <- isolate(input$queryList)
            newQuerySentence <- interpolateInto(seq_along(isolate(dat$replNumbers[[indexOfQuery]])),
                                        inputs[seq_along(isolate(dat$replNumbers[[indexOfQuery]]))]
                                                ,isolate(dat$queryNames)[indexOfQuery])
            if((input$time!="")&&(input$until!="")){
                newQuerySentence <- gsub("\\[T-T\\]",sprintf("[%s-%s]",input$time,input$until),newQuerySentence)
            }
            newQuerySentence <- sprintf("<td class=\"griddiv-selected-vars\">%s</td>",colorReplacements(newQuerySentence))
            session$sendCustomMessage("refreshSelected",list(indexOfQuery=indexOfQuery, querySentence=newQuerySentence))
        }
    
    })

    observe({
        output$queryTable <- renderTable(dat$querySelector,colnames=FALSE,width="100%", sanitize.text.function = function(x) x )
    }) 
    observeEvent(input$RunQuery,{
                    queryIndex <- input$queryList
                    sqlSentence <- dat$queries[input$queryList]
                    optionList <- sapply(1:9,function(x){input[[sprintf("sqlfunc_%s",x)]]}) # These are just the optionAliaces
                    possibilities <- lapply(dat$jsonList[[queryIndex]]$optionAlias[[dat$language]],unlist)
                    optionList <- optionList[optionList!="NA"]
                    selectedNum <- (sapply(seq_along(optionList),function(i){match(optionList[i],possibilities[[i]])}))
                    datoptions <- lapply(dat$jsonList[[queryIndex]]$options,unlist)
                    textContent <- sapply(seq_along(selectedNum),function(i){
                                              if(is.na(selectedNum[i])){
                                                 input[[sprintf("sqlfunc_%s",i)]] 
                                              } else {
                                                  datoptions[[i]][selectedNum[i]]
                                              }
                                        })
                    sentenceToSQL <- interpolateInto(dat$replNumbers[[input$queryList]],textContent,sqlSentence,TRUE)
                    sentenceToSQL <- gsub("\\[T1\\]",sprintf("%s",input$time),sentenceToSQL)
                    sentenceToSQL <- gsub("\\[T2\\]",sprintf("%s",input$until),sentenceToSQL)
                    writeLines(c(sprintf("/*%s*/",input$metadata),"\n\n",sentenceToSQL),file.path(baseDir(),"output/queries",sprintf("%s.sql",input$queryalias)))
         dbDir <- file.path(baseDir(),"output/DB/grid/")
         sqlDB <- DBI::dbConnect(RSQLite::SQLite(),file.path(dbDir,"grid.sqlite3"))
         # browser()
         showNotification("Attaching Soil database...")
         soilDBName <- file.path(normalizePath(dbDir),"SOIL.db")
         if(file.exists(soilDBName)){
            dbExecute(sqlDB,sprintf("ATTACH DATABASE '%s' AS soil",soilDBName ))
         } else {
            showNotification("Cannot find soil database, queries which contains soil data will not run",type="warning")
         }
         showNotification("Running the query, please wait, it can take for a while", id="query", duration=NULL)
         queryResults <- tryCatch(dbGetQuery(sqlDB,sentenceToSQL),error=function(e){NULL})
         if(is.null(queryResults)){
             showNotification("Something went wrong with the query...",type="error")
         } else {
             errorTables <- unlist(lapply(seq_along(dat$jsonList[[queryIndex]]$options),function(i){
                               if(length(dat$jsonList[[queryIndex]]$options[[i]])!=0){
                                   if(dat$jsonList[[queryIndex]]$options[[i]][1]=="*tables*"){
                                      paste0(textContent[i],"_error")
                                   }
                               }}))
             errorColumns <- lapply(errorTables,function(tableName){
                                       dbGetQuery(sqlDB,sprintf("SELECT * FROM %s",tableName))
                                        })
             # queryResults$plotid <- as.numeric(as.numeric(queryResults$plotid))
            #doing a left outer join, the reduce part ads the columns
             finalDF <- merge(Reduce(function(x,y){x$error+y$error},errorColumns),
                               queryResults,by.x="site",by.y="plotid",all.x=TRUE)
             colnames(finalDF) <- c("plotid","error","value")
             write.csv(finalDF,file.path(baseDir(),"output/map_data",sprintf("%s.csv",input$queryalias)),row.names=FALSE)
         }

         removeNotification("query")
         dbDisconnect(sqlDB)
    })

    # DT::datatable(data.frame(outputName = queryNames), options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = 600, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
    #}) 
   algorithms <- list("PHOTOS: Farquhar | PET: Penman-Monteith | WSTRESS: WCBased" = c(0,0,0),
                                                                "PHOTOS: Farquhar | PET: Priestly-Taylor | WSTRESS: WCBased" = c(0,1,0),
                                                                "PHOTOS: Farquhar | PET: Penman-Monteith | WSTRESS: TransDemBased" = c(0,0,1),
                                                                "PHOTOS: Farquhar | PET: Priestly-Taylor | WSTRESS: TransDemBased " = c(0,1,1),
                                                                "PHOTOS: DSSAT | PET: Penman-Monteith | WSTRESS: WCBased" = c(1,0,0),
                                                                "PHOTOS: DSSAT | PET: Priestly-Taylor | WSTRESS: WCBased" = c(1,1,0),
                                                                "PHOTOS: DSSAT | PET: Penman-Monteith | WSTRESS: TransDemBased" = c(1,0,1),
                                                                "PHOTOS: DSSAT | PET: Priestly-Taylor | WSTRESS: TransDemBased" = c(1,1,1)
                                                                )
    observeEvent(input$StartSim,{

        showNotification("Starting simulation... Removing previous .dayout files")
        suppressWarnings(file.remove(list.files(file.path(baseDir(),"output/grid",input$story),full.names=TRUE)))

        showNotification("Setting climate projections and algorithms")
        indexOfRows <- c(4,58,59,61)
        replacements <- c(sprintf("projection/%s/",input$climproj),algorithms[[input$algosel]])
        regex <- "projection/.*?/"
        changeFilesWithRegex(list.files(file.path(baseDir(),"input/initialization/grid",input$story),full.names=TRUE),
                              indexOfRows,replacements,regex)
        ## runChain(baseDir(),input$story,dat$story[[5]])
         dbDir <- file.path(baseDir(),"output/DB/grid/")
         sqlDB <- DBI::dbConnect(RSQLite::SQLite(),file.path(dbDir,"grid.sqlite3"))
         error <- runGrid(baseDir(),input$story,dat$story) # dat$story is a list containing all running groups
         errorDF <- tapply(error,as.numeric(gsub("_.*","",names(error))),sum)
         errorDF <- data.frame(site=names(errorDF),error=errorDF)
         dbWriteTable(sqlDB,sprintf("%s_error",input$outsq),errorDF,overwrite=TRUE)
         dbExecute(sqlDB,sprintf("DROP TABLE IF EXISTS %s",input$outsq))
         withProgress(message="Writing data to database, it can be slow...",value=0,{
                          for(i in seq_along(dat$story)){
                              if(errorDF[i,"error"] == 0){
                                  writeChainToDB(baseDir(),input$story, sqlDB, input$outsq, dat$story[[i]], dat$storyVars)
                              }

                              incProgress(1/length(dat$story),detail=sprintf("Writing site %s into grid database",names(dat$story)[i])) 
                          }
         })

         indexSQL<- c(
                      "site" = "CREATE INDEX site_%s ON %s(plotid)",
                      "year" = "CREATE INDEX year_%s ON %s(year)"
         )
         if(is.element(input$outsq,dbListTables(sqlDB))){
             withProgress(message="Creating Database Indexes",value=0,{
                              for(i in seq_along(indexSQL)){
                                  dbExecute(sqlDB,sprintf("DROP INDEX IF EXISTS %s_%s",names(indexSQL[i]),input$outsq))
                                  dbExecute(sqlDB,sprintf(indexSQL[i],input$outsq,input$outsq,input$outsq))
                                  incProgress(1/length(indexSQL), sprintf("Creating index on %s",names(indexSQL)[i]))
                              }
         })
         }


         dat$modelOutputs <-grep("_error$",dbListTables(sqlDB),invert=TRUE,value=TRUE)
         dbDisconnect(sqlDB)
    })

   observeEvent(input$Map,{
    toreturn$showMap <- input$Map
    })

   return(toreturn)
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

#' writeChainToDB
#'
#' This function reads the model binary and put that into a database
#' @param settings The result of the setupGUI
#' @param dbConnection An SQLite connection
#' @param binaryName The name of the binary output file
#' @param outputName The name of the result table
#' @importFrom DBI dbWriteTable
#' @importFrom lubridate year month yday

writeChainToDB <- function(baseDir,storyName, dbConnection, outputName, chainMatrix, variables,errorVector){
    outFiles <- file.path(baseDir,"output/grid",storyName,paste0(chainMatrix$name,".dayout"))
    binaryName <- paste0(file.path(baseDir,"output/grid/",storyName,chainMatrix[,2]),".dayout")
    toWrite <- do.call("rbind",lapply(seq_along(binaryName),function(i){
                                    con <- file(binaryName[i],"rb")
                                    dayoutput <- matrix(readBin(con,"double",size=8,n=(chainMatrix[i,5]*length(variables))),chainMatrix[i,5],byrow=TRUE)
                                    udates <- grep("[0-9]{4}-02-29",
                                                   as.character(seq(from=as.Date(sprintf("%s-01-01", chainMatrix[i,"startYear"])),
                                                                    to=(as.Date(sprintf("%s-01-01", (chainMatrix[i,"endYear"] + 1)))-1), by=1
                                                                    )),invert=TRUE, value=TRUE)
                                    year <- year(udates)
                                    month <- month(udates)
                                    yday <- yday(udates)
                                    dayoutput <- cbind.data.frame(udates,year,month,yday, dayoutput, site=as.character(chainMatrix[i,1]), stringsAsFactors=FALSE)
                                    colnames(dayoutput) <- as.character(c("udate","year","month","yday", variables, "plotid"))
                                    close(con)
                                    dayoutput
                                       }))
    dbWriteTable(dbConnection, outputName, toWrite, append = TRUE)
}


tables_get <- function(baseDir){
         dbDir <- file.path(baseDir,"output/DB/grid/")
         dir.create(dbDir, showWarnings=FALSE)
         sqlDB <- DBI::dbConnect(RSQLite::SQLite(),file.path(dbDir,"grid.sqlite3"))
         result <- grep("_error$",dbListTables(sqlDB),invert=TRUE,value=TRUE)
         dbDisconnect(sqlDB)
         result
}
