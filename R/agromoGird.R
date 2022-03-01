  agroMoGridUI <- function(id){
    ns <- NS(id)
    tags$div(id = ns(id),
    tags$div(
    id = paste0(ns("ensclim"),"_container"),
    checkboxInput(ns("ensclim"), label = "ensemble", value = FALSE)
    ),
    tags$div(
    id =paste0(ns("ensalg"),"_container"),
    checkboxInput(ns("ensalg"), label = "ensemble", value = FALSE)
    ),   
    tags$div(
      id =paste0(ns("dailyout"),"_container"),
      checkboxInput(ns("dailyout"), label = "daily outputs", value = TRUE)
    ), 
    tags$div(
      id =paste0(ns("enssoil"),"_container"),
      checkboxInput(ns("enssoil"), label = "ensemble", value = FALSE)
    ),
    tags$div(
      id =paste0(ns("repcheck"),"_container"),
      checkboxInput(ns("repcheck"), label = "show and save", value = TRUE)
    ),
    tags$div(
      id =paste0(ns("annual"),"_container"),
      checkboxInput(ns("annual"), label = "annual outputs", value = TRUE)
    ),
    tags$div(
      id = paste0(ns("gridres"),"_container"),
      selectInput(ns("gridres"),"GRID RESOLUTION:",choices = c("10×10 km"))
    ),
    tags$div(
      id = paste0(ns("climproj"),"_container"),
      selectInput(ns("climproj"),"CLIMATE DATABASE:",NA)
    ),
    tags$div(
      id = paste0(ns("soildb"),"_container"),
      selectInput(ns("soildb"),"SOIL DATABASE:",NA)
    ),
    tags$div(
      id = paste0(ns("algosel"),"_container"),
      selectInput(ns("algosel"),"ALGORYTHM SELECTION:",choices=c(
                                                                "PHOTOS: Farquhar | PET: Penman-Monteith | WSTRESS: TransDemBased",
                                                                 "PHOTOS: Farquhar | PET: Penman-Monteith | WSTRESS: WCBased",
                                                                "PHOTOS: Farquhar | PET: Priestley-Taylor | WSTRESS: WCBased",
                                                                "PHOTOS: Farquhar | PET: Priestley-Taylor | WSTRESS: TransDemBased ",
                                                                "PHOTOS: DSSAT | PET: Penman-Monteith | WSTRESS: WCBased",
                                                                "PHOTOS: DSSAT | PET: Priestley-Taylor | WSTRESS: WCBased",
                                                                "PHOTOS: DSSAT | PET: Penman-Monteith | WSTRESS: TransDemBased",
                                                                "PHOTOS: DSSAT | PET: Priestley-Taylor | WSTRESS: TransDemBased"
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
    actionButton(ns("RunQuery"),label = "QUERY"),
    actionButton(ns("Report"),label = "REPORT"),
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
#' @importFrom openxlsx write.xlsx

agroMoGrid <- function(input, output, session, baseDir, language){
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
                          language="en",
                          weatherOptions=NULL,
                          soilOptions=NULL)
    observe({
        if(!is.null(language())){
            dat$language <- language()
        }
    })
    vari <- reactiveValues()
    toreturn <- reactiveValues(showMap=NULL)
    observe({
        dat$jsonList <- lapply((list.files(path=file.path(baseDir(),"template/grid"),pattern="*.json", full.names=TRUE)),read_json)
        dat$queryNames <-  sapply(dat$jsonList,function(x) x$Names[[dat$language]])
        dat$queries <-  sapply(dat$jsonList,function(x) x$query)
        dat$replNumbers <- lapply(dat$queryNames,getReplacementNumbers)
        dat$firstOptions <- lapply(dat$jsonList,function(x) {unlist(lapply(x$optionAlias[[dat$language]],function(y){y[1]}))}) 
        dat$querySelector <- as.data.frame(colorReplacements(unlist(lapply(seq_along(dat$replNumbers),function(i){
                                                              # if(i==8) browser()
                                                              interpolateInto(dat$replNumbers[[i]],dat$firstOptions[[i]],dat$queryNames[i]) 
                                 }))),stringsAsFactors=FALSE)

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
        projections <- basename(list.dirs(file.path(baseDir(),"input/weather/grid"))[-1])
        if(length(projections)!=0){
            updateSelectInput(session,"climproj",choices=projections)
            dat$weatherOptions <- projections
        }
    })

    observe({
        soils <- basename(list.dirs(file.path(baseDir(),"input/soil/grid"))[-1])
        if(length(soils)!=0){
            updateSelectInput(session,"soildb",choices=soils)
            dat$soilOptions <- soils
        }
    })

    observeEvent(input$annual,{
                     if(input$story!=""){
                         choosenStoryFile <- dat$storyFiles[match(input$story,dat$storyOptions)]
                         skip <- ifelse(isolate(input$annual),2,1)
                         dat$storyVars <- as.character(read.table(choosenStoryFile,skip=1, nrows=1, sep=";",stringsAsFactors=FALSE))
                     }
    })

    observeEvent(input$story,{
                     if(input$story!=""){
                         # browser()
                         choosenStoryFile <- dat$storyFiles[match(input$story,dat$storyOptions)]
                         suppressWarnings(dir.create(file.path(baseDir(),"output/grid/",input$story)))
                         suppressWarnings(dir.create(file.path(baseDir(),"endpoint/grid/",input$story)))
                         output$alias <- renderText({readLines(choosenStoryFile,n=1)})
                         skip <- ifelse(isolate(input$annual),2,1)
                         dat$storyVars <- as.character(read.table(choosenStoryFile,skip=skip, nrows=1, sep=";",stringsAsFactors=FALSE))
                         dat$storyCSV <- read.table(choosenStoryFile,skip=3, sep=";",stringsAsFactors=FALSE)
                         dat$storyTimeRange <- range(dat$storyCSV[,c(3,4)])
                         storyRow <- as.data.frame((function(x){
                                                     list(site=x[,1],
                                                          name=apply(x,1,function(y){paste(y[1:2],collapse="_")}),
                                                          startYear=x[,3],
                                                          endYear=x[,4],
                                                          numDays=365*(x[,4]-x[,3]+1))
                                                })(dat$storyCSV),stringsAsFactor=FALSE)
                         inF <- readLines(file.path(baseDir(),
                                                    "input/initialization/grid",
                                                    input$story,
                                                    paste0(storyRow[1,"name"],".ini")))
                         weather <- basename(dirname(inF[4]))
                         soil <- basename(dirname(inF[39]))

                         if(is.element(weather,dat$weatherOptions)){
                             updateSelectizeInput(session,"climproj",choices=unique(dat$weatherOptions),selected=weather)
                         } else {
                                showNotification("Climate file directory (defined in storyLine) not found",type="error")
                         }

                         if(is.element(soil,dat$soilOptions)){
                             updateSelectizeInput(session,"soildb",choices=unique(dat$soilOptions),selected=soil)
                         } else {
                                showNotification("Soil file directory (defined in storyLine) not found",type="error")
                         }

                         dat$story <- split(storyRow,storyRow$site)
                         dat$storyRow <- storyRow
                     }
    })

    observe({
         dbDir <- file.path(baseDir(),"output")
         dir.create(dbDir, showWarnings=FALSE)
         if(dir.exists(dbDir)){
             sqlDB <- DBI::dbConnect(RSQLite::SQLite(),file.path(dbDir,"grid.db"))
             dat[["modelOutputs"]] <-grep("_error$",dbListTables(sqlDB),invert=TRUE,value=TRUE)
             dbDisconnect(sqlDB)
         }
    })


    observeEvent(input$time,{
        if(input$time!=""){
            maxYear <- dat$jsonList[[input$queryList]]$timeFrame$max
            updateSelectInput(session,"until",choices=input$time:maxYear, selected=maxYear)
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

                        if(grepl("\\*.*\\*",choices[1])){
                            starVar <- gsub("\\*(.*)\\*","\\1",choices[1])
                            choices <- eval(parse(text=sprintf("%s_get(baseDir())",starVar))) # Evaluate starVar void function. These are defined at the bottom of this file.
                            vari[[starVar]] <- choices # Evaluate starVar void function. These are defined at the bottom of this file.
                        }

                        updateSelectInput(session,sprintf("sqlfunc_%s",x),
                                          choices=choices)
                      })
            minYear <- dat$jsonList[[input$queryList]]$timeFrame$min
            maxYear <- dat$jsonList[[input$queryList]]$timeFrame$max
            updateSelectInput(session,"time",choices=minYear:maxYear, selected=minYear)
            updateSelectInput(session,"until",choices=minYear:maxYear, selected=maxYear)

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
        
                    if(!identical(queryIndex, NULL) && !identical(input$queryalias, "")){
                        sqlSentence <- dat$queries[input$queryList]
                        optionList <- sapply(1:9,function(x){input[[sprintf("sqlfunc_%s",x)]]}) # These are just the optionAliaces
                        
                        possibilities <- lapply(dat$jsonList[[queryIndex]]$optionAlias[[dat$language]],unlist)
                        optionList <- optionList[optionList!="NA"]
                        selectedNum <- (sapply(seq_along(optionList),function(i){match(optionList[i],possibilities[[i]])}))
                        datoptions <- lapply(dat$jsonList[[queryIndex]]$options,unlist)
                        #BUGGER!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
                        writeLines(c(sprintf("/*%s*/",input$metadata),"\n\n",sentenceToSQL),file.path(baseDir(),"output/query",sprintf("%s.sql",input$queryalias)))
                        outputDB <- file.path(baseDir(),"output")
                        dbDir <- file.path(baseDir(),"database")
                        sqlDB <- DBI::dbConnect(RSQLite::SQLite(),file.path(outputDB,"grid.db"))
                        # browser()
                        showNotification("Attaching Soil database...")
                        soilDBName <- file.path(normalizePath(dbDir),"soil.db")
                        observationDBName <- file.path(normalizePath(dbDir),"observation.db")
                        if(file.exists(soilDBName)){
                            dbExecute(sqlDB,sprintf("ATTACH DATABASE '%s' AS soil",soilDBName))
                        } else {
                            showNotification("Cannot find soil database, queries which contains soil data will not run",type="warning")
                        }

                        showNotification("Attaching climate database...")
                        weatherDBName <- file.path(normalizePath(dbDir),"climate.db")
                        if(file.exists(weatherDBName)){
                            dbExecute(sqlDB,sprintf("ATTACH DATABASE '%s' AS climate", weatherDBName))
                        } else {
                            showNotification("Cannot find weather database, queries which contains weather data will not run",type="warning")
                        }

                        if(file.exists(observationDBName)){
                            dbExecute(sqlDB,sprintf("ATTACH DATABASE '%s' AS observation",observationDBName))
                        } else {
                            showNotification("Cannot find observation database, queries which contains soil data will not run",type="warning")
                        }


                        showNotification("Attaching econo database...")
                        econoDBName <- file.path(normalizePath(dbDir),"economy.db")
                        if(file.exists(econoDBName)){
                            dbExecute(sqlDB,sprintf("ATTACH DATABASE '%s' AS economy", econoDBName))
                        } else {
                            showNotification("Cannot find economy database, queries which contains economy data will not run",type="warning")
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
                            #doing a left outer join, the reduce part ads the columns
                            finalDF <- tryCatch(merge((Reduce(function(x,y){x$error <- x$error+y$error; return(x)},errorColumns)),
                                                      queryResults,by.x="site",by.y="cell_id",all.x=TRUE),
                                                error=function(e){cbind.data.frame(queryResults[,1],0,queryResults[,2])})
                            colnames(finalDF) <- c("cell_id","error","value")
                            write.csv(finalDF,file.path(baseDir(),"output/map_data",sprintf("%s.csv",input$queryalias)),row.names=FALSE)
                        }

                        removeNotification("query")
                        dbDisconnect(sqlDB)
                    } else {
                            showNotification("You should choose one query and provide an alias", duration=NULL)
                        }

    })

    observeEvent(input$Report,{
                    queryIndex <- input$queryList
                    if(!is.null(queryIndex)){
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
                        outputDB <- file.path(baseDir(),"output")
                        dbDir <- file.path(baseDir(),"database")
                        sqlDB <- DBI::dbConnect(RSQLite::SQLite(),file.path(outputDB,"grid.db"))
                        # browser()
                        showNotification("Attaching Soil database...")
                        soilDBName <- file.path(normalizePath(dbDir),"soil.db")
                        observationDBName <- file.path(normalizePath(dbDir),"observation.db")
                        if(file.exists(soilDBName)){
                            dbExecute(sqlDB,sprintf("ATTACH DATABASE '%s' AS soil",soilDBName))
                        } else {
                            showNotification("Cannot find soil database, queries which contains soil data will not run",type="warning")
                        }
                        if(file.exists(observationDBName)){
                            dbExecute(sqlDB,sprintf("ATTACH DATABASE '%s' AS observation",observationDBName))
                        } else {
                            showNotification("Cannot find observation database, queries which contains soil data will not run",type="warning")
                        }

                        showNotification("Attaching weather database...")
                        weatherDBName <- file.path(normalizePath(dbDir),"climate.db")
                        if(file.exists(weatherDBName)){
                            dbExecute(sqlDB,sprintf("ATTACH DATABASE '%s' AS climate", weatherDBName))
                        } else {
                            showNotification("Cannot find weather database, queries which contains weather data will not run",type="warning")
                        }


                        showNotification("Attaching econo database...")
                        econoDBName <- file.path(normalizePath(dbDir),"economy.db")
                        if(file.exists(econoDBName)){
                            dbExecute(sqlDB,sprintf("ATTACH DATABASE '%s' AS economy", econoDBName))
                        } else {
                            showNotification("Cannot find economy database, queries which contains economy data will not run",type="warning")
                        }


                        showNotification("Running the query, please wait, it can take for a while", id="query", duration=NULL)
                        queryResults <- tryCatch(dbGetQuery(sqlDB,sentenceToSQL),error=function(e){NULL})
                        if(is.null(queryResults)){
                            showNotification("Something went wrong with the query...",type="error")
                        } else {
                            if(input$repcheck){
                                showModal(modalDialog(tableOutput(ns("pukli")),title="REPORT", size="l",easyClose=TRUE))
                                output$pukli <- renderTable({
                                    queryResults
                                })
                            }
                            suppressWarnings(dir.create(file.path(baseDir(),"output/report")))
                            write.csv(queryResults,file.path(baseDir(),"output/report",sprintf("%s.csv",input$queryalias)),row.names=FALSE)
                            # write.xlsx(queryResults,file.path(baseDir(),"output/report",sprintf("%s.xlsx",input$queryalias)))
                        }

                        removeNotification("query")
                        dbDisconnect(sqlDB)
                    } else {
                            showNotification("You should choose at least one query", duration = TRUE)
                    }
    })

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
        
                     if(!isolate(input$annual)){
                        gridType <- ".dayout"
                        outputTypeIni <- c(1,0)
                     } else {
                         gridType <- ".annout"
                        outputTypeIni <- c(0,2)
                     }

    showNotification("Checking file system for errors")
    firstIni <- sprintf("input/initialization/grid/%s/%s.ini",input$story,dat$story[[1]]$name)
    errorFiles <- checkFileSystem(firstIni)

    if(length(errorFiles) == 0) {

    climprojs <- input$climproj
    if(input$ensclim){
        climprojs <- sprintf("grid/%s/",list.files(file.path(baseDir(),"input/weather/grid")))
        climprojs <- climprojs[grep('^\\.',basename(climprojs),invert=TRUE)]
    }
    climdb <- DBI::dbConnect(RSQLite::SQLite(),file.path(baseDir(),"/database/climate.db"))
    metaTable <- DBI::dbReadTable(climdb,"meta_data")
    DBI::dbDisconnect(climdb)
    withProgress(message="Climate Ensemble",value=0,{
        for(ci in seq_along(climprojs)){
            clim <- climprojs[ci]
            source_name <- basename(toupper(clim))
            source_name <- gsub("^\\.","",source_name)
            climid <- metaTable[toupper(metaTable[,"source_name"]) == toupper(source_name),"source_id"]

            showNotification("Starting simulation... Removing previous .dayout files")
            suppressWarnings(file.remove(
                                list.files(file.path(baseDir(),
                                                              "output/grid",
                                                              input$story),full.names=TRUE)))

            showNotification("Setting climate projections and algorithms")
            indexOfRows <- c(4,39,58,59,61,107,110)
            replacements <- c(sprintf("grid/%s/",basename(clim)),
                              sprintf("grid/%s/",input$soildb),
                              algorithms[[input$algosel]],outputTypeIni[1],outputTypeIni[2])
            regex <- c("grid/.*?/","grid/.*?/")
            changeFilesWithRegex(list.files(file.path(baseDir(),"input/initialization/grid",input$story),full.names=TRUE),
                                 indexOfRows,replacements,regex)
            ## runChain(baseDir(),input$story,dat$story[[5]])
            dbDir <- file.path(baseDir(),"output")
            sqlDB <- DBI::dbConnect(RSQLite::SQLite(),file.path(dbDir,"grid.db"))
            error <- runGrid(baseDir(),input$story,dat$storyRow) # dat$story is a list containing all running groups
            if(is.list(error)){
                error <- do.call(c,error)
            }
            errorDF <- tapply(error,as.numeric(gsub("_.*","",names(error))),sum)
            errorDF <- data.frame(site=names(errorDF),error=errorDF)
            dbWriteTable(sqlDB,sprintf("%s_error",input$outsq),errorDF,overwrite=TRUE)
            if(ci == 1){
                dbExecute(sqlDB,sprintf("DROP TABLE IF EXISTS %s",input$outsq))
            }
            withProgress(message="Writing data to database, it can be slow...",value=0,{
                             for(i in seq_along(dat$story)){
                                 if(errorDF[i,"error"] == 0){
                                     writeChainToDB(baseDir(),input$story, sqlDB, input$outsq, dat$story[[i]], dat$storyVars,
                                                    type=gridType,climid=climid)
                                 }

                                 incProgress(1/length(dat$story),detail=sprintf("Writing site %s into grid database",names(dat$story)[i])) 
                             }
                                 })
            incProgress(1/length(climprojs), detail=sprintf("%s [%s/%s]",basename(climprojs[ci]), ci, length(climprojs)))
        }

    })
    
    


        indexSQL<- c(
                     "site" = "CREATE INDEX site_%s ON %s(cell_id)",
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
    } else {
        showNotification(tags$html(paste(sapply(names(errorFiles),function(eFile){
                                    sprintf(" the %s file (%s) is missing", eFile, errorFiles)
                                 } ),collapse="<br>")), type="error", duration = 10) 
    }

    })

   observeEvent(input$Map,{
    toreturn$showMap <- input$Map
    })

   observe({
        outN <- paste(input$story, match(input$climproj,dat$weatherOptions),match(input$soildb,dat$soilOptions), sep="_")
        updateTextInput(session,"outsq", value=outN)
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
                        a[x] <<- gsub(regex[ind],replacements[ind],a[x], perl = TRUE)
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
   interpolateInto(dat$replNumbers[[index]],optis,dat$query)
}

runChain <- function (baseDir, storyName, chain, chainMatrix) {
   chainMatrix <- chainMatrix[chainMatrix[,"site"] == chain,]
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

runGrid <- function(baseDir, storyName, chainMatrixFull){

    showNotification(sprintf("Starting cluster for parallel run with %s cores, please wait for the progress indicator",detectCores()-1))
    cl <- makeCluster(detectCores()-1)
    registerDoSNOW(cl)
    iterations <- length(chainMatrixFull)
    progress <- function(i) incProgress(1/iterations, detail = paste("Doing part", i))
    opts <- list(progress = progress)
    os  <- Sys.info()["sysname"]
    if(os == "Windows"){
        withProgress(message = "simulation", value = 0, {
                         result <- foreach(site = unique(chainMatrixFull$site), .export="runChain", .combine = c, 
                                           .options.snow = opts) %dopar% {
                                runChain(baseDir,storyName, site, chainMatrixFull)
                         }
        })
        stopCluster(cl)
        print(result)
   } else {
       withProgress(message = "simulation", value = 0, min = 0, max = nrow(chainMatrixFull), {
                paraProgress(unique(chainMatrixFull$site), function(chain){runChain(baseDir, storyName, chain, chainMatrixFull)},
                                  cores = 100, pb=NULL, pbUpdate = function(x){setProgress(value=x,detail=x)}
                )
        })
   }
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

writeChainToDB <- function(baseDir, storyName, dbConnection, outputName,
                           chainMatrix, variables, errorVector, type, climid){

    fName <- paste0(file.path(baseDir, "output/grid/",
                              storyName, chainMatrix[,2]), type)
    econofName <-  paste0(file.path(baseDir, "output/grid/",
                              storyName, chainMatrix[,2]), ".econout")

    toWrite <- do.call("rbind",
        lapply(fName, function(fn){readTable(fn,econofName,
                      variables,
                      type,
                      cell_id=as.character(chainMatrix[,1]),
                      numDays=as.integer(chainMatrix[,5]),
                      startYear=as.integer(chainMatrix[,"startYear"]),
                      endYear=as.integer(chainMatrix[,"endYear"]))}))
    toWrite <- cbind.data.frame(toWrite, climate_id =climid)

    dbWriteTable(dbConnection, outputName, toWrite, append = TRUE)
}


#' writeChainToDB
#'
#' This function reads the model binary and put that into a database
#' @param fName the name of the file
#' @param variables The variable names 
#' @param type .dayout or .annout
#' @importFrom lubridate year month yday

readTable <- function(fName, econofName, variables, type, cell_id, numDays, startYear, endYear){   

    if(type == ".dayout"){
        con <- file(fName,"rb")
        dayoutput <- matrix(readBin(con,"double",size=8,n=(numDays*length(variables))),
                                       numDays,byrow=TRUE)
        udates <- grep("[0-9]{4}-02-29", as.character(seq(from=as.Date(sprintf("%s-01-01",startYear)),
                                                          to=(as.Date(sprintf("%s-01-01", (endYear + 1)))-1),
                                                          by=1)),
                       invert=TRUE, value=TRUE)
        year <- year(udates)
        month <- month(udates)
        yday <- yday(udates)
        dayoutput <- cbind.data.frame(udates,year,month,yday, dayoutput,
                                      site=cell_id, stringsAsFactors=FALSE)
        colnames(dayoutput) <- as.character(c("udate","year","month","yday", variables, "cell_id"))
        close(con)
        return(dayoutput)
    } else {
        if(file.exists(econofName)){
            econonames <- c("year","crop_id","prim_prod","sec_prod","irr_amount","irr_type")
            econoOutput <- read.table(econofName, skip=1, header=FALSE)
            econoOutput[,1] <- as.integer(econoOutput[,1])
            econoOutput[,5] <- as.integer(econoOutput[,5])
            colnames(econoOutput) <- econonames
            annual <- read.table(fName, skip=1, header=FALSE)
            colnames(annual) <- c("year",variables)
            annuOutput <- cbind.data.frame(
                            merge(x = annual,
                                  y = econoOutput,all=TRUE,by="year",sort = FALSE),
                            cell_id)
            colnames(annuOutput) <- c("year", variables, econonames[-1],"cell_id")
            return(annuOutput)
        }

        annuOutput <- cbind.data.frame(read.table(fName, skip=1, header=FALSE),cell_id)
        colnames(annuOutput) <- c("year", variables, "cell_id")
        return(annuOutput)
    }
}


#' decor_fun
#'
#' This is a simple decorator for paralell progress bars
#' @param fun the name of the function
#' @param tmpdir the temporary file for IPC
#' @importFrom filelock lock unlock
#' @keywords internal 

decor_fun <- function(fun, tmpdir){
  function(X){
    ret <- fun(X)
    lck <- lock(paste0(tmpdir,'.lock'))
    str <- readLines(tmpdir)
    n <- as.numeric(str[length(str)]) + 1
    write(n, tmpdir)
    unlock(lck)
    return(ret)
  }
}


#' fut_lapply
#'
#' parallel lapply alternative. the difference between this and future_parallel is that the result of this function is a future, not a value
#' @param li starting list
#' @param FUN a decor_fun decorated function
#' @param cores number of cores
#' @importFrom future future
#' @keywords internal 

fut_lapply <- function(li, FUN, cores, ...){
  out <- vector(mode="list",length(li))
  
  for(i in seq_along(li)){
    print(i)
    out[[i]] <- future({FUN(li[i], ...)})
  }
  return(out)
}


#' paraWithProgress
#'
#' A simple function which creates the necessery files for the paraprogress bar
#' @param X starting list
#' @param fun an arbitary 1D function
#' @param cores number of cores
#' @importFrom future future
#' @importFrom parallel mclapply
#' @keywords internal 

paraWithProgress <- function(X, fun, cores, ...){
  tmpdir <-tempfile() 
  write(0,tmpdir)
  FUN <- decor_fun(fun, tmpdir)
  
  if(Sys.info()["sysname"] == "Windows"){
      ret <- list(
        wdir=tmpdir,
        fun=function(...){
          return(fut_lapply(li=X, FUN=FUN, ...))
        }
      )
      return(ret)
  }
  return(
    list(wdir=tmpdir,
         fun=function(...){
           future({mclapply(X=X, FUN=FUN,..., mc.cores=cores)}) 
         })
  )
}


#' paraProgress
#'
#' A simple function which creates the necessery files for the paraprogress bar
#' @param X starting list
#' @param FUN an arbitary 1D function
#' @param cores number of cores
#' @param pb Progress bar
#' @param pbUpdate Progress bar
#' @importFrom future value plan sequential multicore multisession
#' @importFrom parallel mclapply
#' @export

paraProgress <- function(X, FUN, cores,
                         pb = txtProgressBar(min=1, max=length(X), style=3),
                         pbUpdate = setTxtProgressBar, prepB = NULL, prepUpdate = NULL){
    os <-  Sys.info()["sysname"]
    if(Sys.getenv("RSTUDIO")=="1" && os != "Windows"){
        warning("Forking is not supported in Rstudio, fall back to regular process making")
        os <- "Windows"
    }

    if(os == "Windows"){
        plan(multisession, workers=cores)
    } else {
        plan(multicore)
    }

    parfunc <- paraWithProgress(X=X, fun=FUN, cores=cores)
    print(parfunc$wdir)
    fut <- parfunc$fun()
    # browser()
    progress <- 0
    size <- length(X)
    print(size)
    while(1){

        progress <- (as.numeric(readLines(parfunc$wdir)[1]))

        if(is.null(progress) || is.na(progress) || length(progress) == 0){
            next()
        }

        if(progress >= size){
            break()
        }

        if(is.null(pb)){
            pbUpdate(as.numeric(progress))
        } else {
            pbUpdate(pb,as.numeric(progress))
        }

        Sys.sleep(1)
    }

    if(is.null(pb)){
        pbUpdate(size)
    } else {
        pbUpdate(pb,size)
    }

    file.remove(parfunc$wdir)

    if(!is.null(pb)){
        close(pb)
    }

    if(os != "Windows"){
        ret <- value(fut)
        plan(sequential)
        return(ret)
    }

    ret <- lapply(fut, value)
    plan(sequential)
    return(ret)
}
