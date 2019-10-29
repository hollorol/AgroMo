#' agroMoSiteUI
#'
#' this function defines the user interface of the site window
#' @param id the id of the site window
#' @importFrom shiny NS textInput checkboxInput selectInput uiOutput
#' @keywords internal
agroMoSiteUI <- function(id){
  ns <- NS(id)
  baseDir <- "defaultDir"
  baseTable<- data.frame(selectorId <- c(ns("iniFile"), ns("weatherFile"), ns("soilFile"), ns("managementFile")),
                         label <- c("INI file:", "WEATHER file:", "SOIL file:", "MANAGEMENT file:"),
                         place <- file.path(baseDir,c("input/initialization/site", "input/weather/site", "input/soil/site", "input/management")),
                         pattern <- c("*.ini","*.wth","*.soi","*.mgm"))

  managementTypes <- c("planting", "harvest", "fertilization", "irrigation", "cultivation", "grazing", "mowing", "thinning")


  dropdownElements <- shiny::tags$div(id = "fileOutput", class="inFile",
                               apply(baseTable, 1,function (x){
                                 if(!grepl("management",x[1])){
                                   shiny::tags$div(id = paste0(x[1],"_container"), selectInput(x[1],x[2],basename(grep(list.files(x[3]), pattern = x[4], value = TRUE)),width = "100%"))
                                 } else {
                                   shiny::tags$div(id = paste0(x[1],"_container"), selectInput(x[1],x[2],c(basename(grep(list.files(x[3],recursive = TRUE), pattern = x[4], value = TRUE)),"none"),width = "100%"))
                                 }
                               })
                               )


  shiny::tags$div(id = ns(id),

           tagList(
             shiny::tags$img(id = ns("base_bb"),src="www/img/base_banner_button.svg"),
             shiny::tags$img(id = ns("map_bb"),src="www/img/map_banner_button.svg"),
             shiny::tags$img(id = ns("grid_bb"),src="www/img/grid_banner_button.svg"),
             shiny::tags$img(id = ns("show_bb"),src="www/img/show_banner_button.svg"),
             shiny::tags$img(id = ns("refresh"),src="www/img/refresh_button.svg", draggable = FALSE),
             dropdownElements,
             shiny::tags$div(
                    id = paste0(ns("stationp"),"_container"),
                    checkboxInput(ns("stationp"), label = "Observed data only", value = TRUE)
                  ),
             shiny::tags$div(
                    id =paste0(ns("sitep"),"_container"),
                    checkboxInput(ns("sitep"), label = "Observed data only", value = TRUE)
                  ),
             shiny::tags$div(id="manModuls","management options:"),
             shiny::tags$div(id="shiftIn","shift in ..."),
             lapply(managementTypes,function(man){
                        # browser()
               choices <- basename(grep(man,list.files("./",recursive=TRUE),value = TRUE))
               if(length(choices)==0){
                 choices <- NULL
               }
               shiny::tags$div(
                      id = paste0(ns(man),"_container"),
                      selectInput(ns(man),paste0(man,":"),c("none",choices))
                    )
             }),

             uiOutput(ns("outputFile")),
             shiny::tags$div(id = ns("Buttons"),
             runAndPlotUI(ns("popRun"),label = "RUN"),
             actionButton(ns("Show"),label="PLOT")),
             shiny::tags$div(
                    id = paste0(ns("planshift_date"),"_container"),
               textInput(ns("planshift_date"), "date (day):", 0)
             ),
             shiny::tags$div(
                    id = paste0(ns("planshift_density"),"_container"),
                    textInput(ns("planshift_density"), "density (p/m2):", 0)
             ),
             shiny::tags$div(
                    id = paste0(ns("harvshift_date"),"_container"),
                    textInput(ns("harvshift_date"), "date (day):", 0)
             ),
             shiny::tags$div(
                    id = paste0(ns("fertshift_date"),"_container"),
                    textInput(ns("fertshift_date"), "date (day):", 0)
             ),
             shiny::tags$div(
                    id = paste0(ns("irrshift_date"),"_container"),
                    textInput(ns("irrshift_date"), "date (day):", 0)
             ),
             shiny::tags$div(
                    id = paste0(ns("fertshift_amount"),"_container"),
                    textInput(ns("fertshift_amount"), "amount (kg/ha):", 0)
             ),
             shiny::tags$div(
                    id = paste0(ns("irrshift_amount"),"_container"),
                    textInput(ns("irrshift_amount"), "amount (mm):", 0),
               shiny::tags$hr(id=ns("littleblackline")),
               shiny::tags$hr(id=ns("littleblacklinetwo"))
             )
           )
  )
}

#' agroMoSite
#'
#' This function provides the server-logic for the SITE window
#' @param input environment which provides the results of the user input
#' @param output environment where the server output goes
#' @param session environment to get information about the current session
#' @param dataenv The central datastructure of the AgroMo
#' @param baseDir baseDir is the base directory for the modell inputs/outputs
#' @importFrom shiny reactive updateSelectInput observe textInput renderUI reactiveValues callModule observeEvent isolate 
#' @importFrom jsonlite read_json
#' @keywords internal


agroMoSite <- function(input, output, session, dataenv, baseDir, connection){
  managementExt <- c("planting" = "plt", "harvest" = "hrv",
                     "fertilization" = "frz",
                     "irrigation" = "irr",
                     "grazing" = "grz",
                     "mowing" = "mow",
                     "thinning" = "thn")
  centralData <- read_json(system.file("data/centralData.json",package="agromR"),simplifyVector = TRUE)
  manReactive <- reactiveValues(included=NULL)
  managementRows <- c("plt" = 5, "thn" =  9, "mow" = 13, "grz"= 17, "hrv"= 21, "plo" = 25, "frz" = 29, "irr" = 33)
  dat <- reactiveValues(dataenv = dataenv, trigger = 0, show = 0, baseDir = baseDir)
  ## browser()
  output$outputFile <- renderUI({
    ns <- session$ns
    odellOutputs <- c(dataenv(),input$iniFile)
    tagList(
      shiny::tags$div(id = "outputF", class = "inFile",

               ## selectizeInput(ns("outFile"),"OUTPUT id:",modellOutputs,selected = iniFile(),options = list(create = TRUE))
               textInput(ns("outFile"),"OUTPUT id:",strsplit(input$iniFile,split = "\\.")[[1]][1])
               )

    )
    })
  observe({
    updateSelectInput(session,"iniFile", choices = grep("*.ini",list.files(file.path(baseDir(),"input/initialization/site")),value = TRUE))
  })
  iniFile <- reactive({input$iniFile})
  mgmFile <- reactive({input$managementFile})
  observe({
     # browser()
      if(iniFile()!=""){
          settings <- setupGUI(iniFile(),isolate(baseDir()), centralData)
          # sapply(ls(settings),function(x){print(settings$x)})
          # browser()
          if(!is.null(settings) && settings$epc != ""){
              updateSelectInput(session,"soilFile", selected = settings$soil)
              updateSelectInput(session,"weatherFile", selected = settings$meteo)
              ## browser()
              updateSelectInput(session,"managementFile", selected = settings$mgm)
          }
      }

   })

  observe({
     #  print(baseDir())
     # browser()
      updateSelectInput(session,"soilFile",
                        choices = basename(grep("*.soi",
                                       list.files(file.path(baseDir(),"input","soil","site"),recursive = TRUE),value = TRUE)))

      updateSelectInput(session,"weatherFile",
                        choices = basename(grep("*.wth",
                                                list.files(file.path(
                                                  baseDir(),"input","weather","site")
                                                 ,recursive = TRUE),value = TRUE)))
      updateSelectInput(session,"managementFile",
                        choices = basename(grep("*.mgm",
                                                list.files(file.path(
                                                  baseDir(),"input","management")
                                                 ,recursive = TRUE),value = TRUE)))


  })
  updateSelectInput(session,"cultivation",selected = NA)
  observe({
    manReactive$included <- sapply(names(managementExt),function(manName){
      if(mgmFile()=="none"){
        mgmF <- ""
      } else  {
        ## browser()
        mgmF<- readLines(file.path(isolate(baseDir()),"input","management",mgmFile()))
      }
      included <- grep(sprintf("\\.%s$",managementExt[manName]), mgmF, value = TRUE)
      if(length(included)==0){
        return(NA)
      } else {
        return(basename(included))
      }
    })
  })
 manType <- reactive({manReactive$included})
  observe({
    updateSelectInput(session,"planting", selected = manType()[1])
    updateSelectInput(session,"harvest", selected = manType()[2])
    updateSelectInput(session,"fertilization", selected = manType()[3])
    updateSelectInput(session,"irrigation", selected = manType()[4])
    updateSelectInput(session,"grazing", selected = manType()[5])
    updateSelectInput(session,"mowing", selected = manType()[6])
    updateSelectInput(session,"thinning", selected = manType()[7])
  })

  observeEvent(input$Show,{
    dat$show <- dat$show + 1
  })

  onclick("refresh",{

    ## browser()
    updateSelectInput(session,"soilFile",
                      choices = basename(grep("*.soi",
                                              list.files(file.path(baseDir(),"input","soil","site"),recursive = TRUE),value = TRUE)))

    updateSelectInput(session,"weatherFile",
                      choices = basename(grep("*.wth",
                                              list.files(file.path(baseDir(),"input","weather","site"),recursive = TRUE),value = TRUE)))

    updateSelectInput(session,"managementFile",
                      choices = basename(grep("*.mgm",
                                              list.files(file.path(
                                                baseDir(),"input","management")
                                               ,recursive = TRUE),value = TRUE)))
  })
  observe({
    print(input$outFile)
  })
  callModule(runAndPlot,"popRun",baseDir, reactive({input$iniFile}),
             reactive({input$weatherFile}), reactive({input$soilFile}),
             reactive({input$managementFile}), reactive({input$outFile}),
             reactive({input$planting}), reactive({input$harvest}),
             reactive({input$fertilization}), reactive({input$irrigation}),
             reactive({input$grazing}), reactive({input$mowing}),
             reactive({input$thinning}),
             reactive({input$planshift_date}),
             reactive({input$planshift_density}),
             reactive({input$harvshift_date}),
             reactive({input$fertshift_date}),
             reactive({input$irrshift_date}),
             reactive({input$fertshift_amount}),
             reactive({input$irrshift_amount}),
  reactive({connection}),reactive({centralData}))

    return(dat)
 }
