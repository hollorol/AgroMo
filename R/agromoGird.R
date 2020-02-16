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
      textInput(ns("metadata"), "METADATA:",NA)
    ),
    
    tags$div(id = ns("Buttons"),
    actionButton(ns("StartSim"),label = "START SIMULATION"),
    actionButton(ns("RunQuery"),label = "START QUERY"),
    actionButton(ns("Map"),label="MAP")),             
     
    tags$div(
      id = paste0(ns("time"),"_container"),
      selectInput(ns("time"),"TIME SLICE [start-end]:",choices=c("1971"))
    ),                
    tags$div(
      id = paste0(ns("until"),"_container"),
      selectInput(ns("until"),"-",choices=c("2100"))
    ),  
    do.call(tagList,(
                      lapply(1:9,function(x){
                                 tags$div(
                                          id= ns(sprintf("sqlfunc_%s_container",x)),
                                          selectInput(sprintf("sqlfunc_%s",x),sprintf("{%s}:",x),choices=c(""))
                                 )
                      })
    )),
  
tags$div(id="query","QUERIES:"),

tags$div(
        tableOutput(ns("queryTable"))
  )  
    )
                          
  }
  
  agroMoGrid <- function(input, output, session,baseDir){
    ns <- session$ns
    dat <- reactiveValues(jsonList=NULL, storyFiles=list())

    observe({
    jsonlist <- lapply((list.files(path=file.path(baseDir()),pattern="*.json", full.names=TRUE)),read_json)
    }) 
    jsonlist <- lapply((list.files(path=file.path(baseDir()),pattern="*.json", full.names=TRUE)),read_json)


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
        if(isolate(input$story)!=""){
            output$alias <- renderText({readLines(dat$storyFiles[match(input$story,dat$storyOptions)],n=1)})
        }
    })

    observeEvent(,{
    
    })
     
    # output$queryTable <- DT::renderDataTable (tabe,options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = FALSE, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
    observe({
    tabe=data.frame(c("<span class=\"reddi\">{1:mean}</span> {2:annual} yield {3:max} in the [start-end] period", "{1:max} {2:annual} lai {3:max} in the [start-end] period", "{1:mean} {2:may} {3:0-3 cm} soiltemp {4:mean} in the <span class=\"timeSlice\">[start-end]</span> period"))
        output$queryTable <- renderTable(tabe,colnames=FALSE,width="100%", sanitize.text.function = function(x) x )
    }) 
#    output$queryTable <- DT::renderDataTable({
      
    # DT::datatable(data.frame(outputName = queryNames), options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = 600, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
    #}) 
    
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

interpolateInto <- function(places, strings, jsonstring){
    for(i in seq_along(places)){
        jsonstring <- gsub(sprintf("(\\{%s\\})",places[i]),strings[places[i]],jsonstring)
    }
    return(jsonstring)
}

interpolateArray <- function (jsonlist,x) {
    jsonIndex<- x$jsonIndex
    jsonOptions <- x$jsonOptions
    interpolateInto(getReplacementNumbers(jsonlist[[jsonIndex]]$query), jsonOptions, jsonlist[[jsonIndex]]$query)
}
