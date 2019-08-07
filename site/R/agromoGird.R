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
      selectInput(ns("gridres"),"GRID RESOLUTION:",NA)
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
      selectInput(ns("algosel"),"ALGORYTHM SELECTION:",NA)
    ),    
    tags$div(
      id = paste0(ns("story"),"_container"),
      selectInput(ns("story"),"STORY-LINE FILE:",NA)
    ),     
   
    
    tags$div(
      id = paste0(ns("outsq"),"_container"),
      textInput(ns("outsq"),"OUTPUT SQLite TABLE:",NA)
    ),     
    tags$div(
      id = paste0(ns("queryalias"),"_container"),
      textInput(ns("queryalias"), "QUERY ALIAS:",NA)
    ),
    
#itt a funkcionalitas kerdeses    
    tags$div(id = ns("Buttons"),
    actionButton(ns("RunQuery"),label = "RUN AND QUERY"),
    actionButton(ns("Map"),label="MAP")),             
       
          
    tags$div(
      id = paste0(ns("time"),"_container"),
      selectInput(ns("time"),"TIME SLICE [T-T]:",NA)
    ),                
    tags$div(
      id = paste0(ns("until"),"_container"),
      selectInput(ns("until"),"-",NA)
    ),  
    tags$div(
      id = paste0(ns("tempfocus"),"_container"),
      selectInput(ns("tempfocus"),"TEMPORAL FOCUS >T<:",NA)
    ),
    tags$div(
      id = paste0(ns("spatfocus"),"_container"),
      selectInput(ns("spatfocus"),"SPATIAL FOCUS >S<:",NA)
    ),
    tags$div(
      id = paste0(ns("varfunc"),"_container"),
      selectInput(ns("varfunc"),"VARIABLE FUNCTION <Vf>:",NA)
    ),
    tags$div(
      id = paste0(ns("aggrfunc"),"_container"),
      selectInput(ns("aggrfunc"),"AGGREGATION FUNCTION ;<Af>:",NA)
    ),
    tags$div(
      id = paste0(ns("spaggr"),"_container"),
      selectInput(ns("spaggr"),"SPATIAL AGGREGATION |SA|:",NA)
    )    
             
    #    TagList(
    # DT::dataTableOutput(ns("queryTable"))
    # )             
             
             
             
    )           
                          
  }
  
  agroMoGrid <- function(input, output, session){
    ns <- session$ns
    dat <- reactiveValues()
    dat[["dataenv"]] <-readRDS("output/outputs.RDS")
#    modellOutputNames <- ls(dat$dataenv)
    
    #    output$queryTable <- DT::renderDataTable({
      
    # DT::datatable(data.frame(outputName = modellOutputNames), options = list(autowidth = FALSE, paginate = FALSE, scrollX = FALSE, scrollY = 600, searching = TRUE, info = FALSE, header=FALSE,rownames=FALSE))
    #}) 
    
  }
  
  