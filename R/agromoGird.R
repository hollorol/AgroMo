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
      id = paste0(ns("inidir"),"_container"),
      selectInput(ns("inidir"),"INI FILE DIRECTORY:",NA)
    ),
    tags$div(
      id = paste0(ns("algosel"),"_container"),
      selectInput(ns("algosel"),"ALGORYTHM SELECTION:",choices=c("[FaPe] PHOTOS: Farquhar | PET: Penman"))
    ),    
    tags$div(
      id = paste0(ns("story"),"_container"),
      selectInput(ns("story"),"STORY-LINE FILE:",choices=c("none"))
    ),     
    tags$div(
      id = paste0(ns("outsq"),"_container"),
      textInput(ns("outsq"),"OUTPUT SQLite TABLE:",NA)
    ),     
    tags$div(
      id = paste0(ns("queryalias"),"_container"),
      textInput(ns("queryalias"), "QUERY ALIAS:",NA)
    ),
    tags$div(
      id = paste0(ns("metadata"),"_container"),
      textInput(ns("metadata"), "METADATA:",NA)
    ),
    

    
#itt a funkcionalitas kerdeses    
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
    tags$div(
      id = paste0(ns("tempfocus"),"_container"),
      selectInput(ns("tempfocus"),"{1}:",choices=c("mean", "maximum", "minimum"))
    ),
    tags$div(
      id = paste0(ns("spatfocus"),"_container"),
      selectInput(ns("spatfocus"),"{2}:",choices=c("0-3 cm layer"))
    ),
    tags$div(
      id = paste0(ns("varfunc"),"_container"),
      selectInput(ns("varfunc"),"{3}:",choices=c("max"))
    ),
    tags$div(
      id = paste0(ns("aggrfunc"),"_container"),
      selectInput(ns("aggrfunc"),"{4}:",choices=c("maximum"))
    ),
    tags$div(
      id = paste0(ns("spaggr"),"_container"),
      selectInput(ns("spaggr"),"{5}:",choices=c("10×10 km"))
    ),    

tags$div(id="query","QUERIES:"),

## Itt is a funkcionalitas erosen kerdeses
tagList(
         DT::dataTableOutput(ns("queryTable"))
  )  
    )
                          
  }
  
  agroMoGrid <- function(input, output, session){
    ns <- session$ns
    #dat <- reactiveValues()
    #dat[["dataenv"]] <-readRDS("output/outputs.RDS")
   # queryNames <- ls(dat$dataenv)
    
    tabe=data.frame(c("{1:mean} {2:annual} yield {3:max} in the [start-end] period", "{1:max} {2:annual} lai {3:max} in the [start-end] period", "{1:mean} {2:may} {3:0-3 cm} soiltemp {4:mean} in the [start-end] period"))
    output$queryTable <- DT::renderDataTable (tabe,options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = FALSE, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
    
#    output$queryTable <- DT::renderDataTable({
      
    # DT::datatable(data.frame(outputName = queryNames), options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = 600, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
    #}) 
    
  }
  
  
