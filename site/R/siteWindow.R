#debug(setupGUI)
createInputElements <- function(baseTable){
    apply(baseTable, 1,function (x){
        if(!grepl("management",x[1])){
            tags$div(id = paste0(x[1],"_container"), selectInput(x[1],x[2],basename(grep(list.files(x[3]), pattern = x[4], value = TRUE)),width = "100%"))
        } else {
            tags$div(id = paste0(x[1],"_container"), selectInput(x[1],x[2],c(basename(grep(list.files(x[3]), pattern = x[4], value = TRUE)),"no management"),width = "100%"))
        }
    })
}

managementTypes <- c("planting", "harvest", "fertilization", "irrigation", "cultivation", "grazing", "mowing", "thinning")

createInputs <- function(baseTable){
  tags$div(id = "fileOutput", class="inFile",createInputElements(baseTable = baseTable))
}

agroMoSiteUI <- function(id){
  ns <- NS(id)
  baseTable<- data.frame(selectorId <- c(ns("iniFile"), ns("weatherFile"), ns("soilFile"), ns("managementFile")),
                         label <- c("INI file:", "WEATHER file:", "SOIL file:", "MANAGEMENT file:"),
                         place <- c("input/initialization/", "input/weather", "input/soil", "input/management"),
                         pattern <- c("*.ini","*.wth","*.soi","*.mgm"))

  
  tags$div(id = ns(id),

           tagList(
             tags$img(id = ns("base_bb"),src="img/base_banner_button.svg"),
             tags$img(id = ns("map_bb"),src="img/map_banner_button.svg"),
             tags$img(id = ns("grid_bb"),src="img/grid_banner_button.svg"),
             tags$img(id = ns("show_bb"),src="img/show_banner_button.svg"),
             createInputs(baseTable),
             tags$div(
                    id = paste0(ns("stationp"),"_container"),
                    checkboxInput(ns("stationp"), label = "Observed data only", value = TRUE)
                  ),
             tags$div(
                    id =paste0(ns("sitep"),"_container"),
                    checkboxInput(ns("sitep"), label = "Observed data only", value = TRUE)
                  ),
             tags$div(id="manModuls","management options:"),
             tags$div(id="shiftIn","shift in ..."),

             lapply(managementTypes,function(man){
               choices <- basename(grep(man,list.files("./",recursive=TRUE),value = TRUE))
               if(length(choices)==0){
                 choices <- NULL
               }
               tags$div(
                      id = paste0(ns(man),"_container"),
                      selectInput(ns(man),paste0(man,":"),c("none",choices))
                    )
             }),

             uiOutput(ns("outputFile")),
             tags$div(id = ns("Buttons"),
             runAndPlotUI(ns("popRun"),label = "RUN"),
             actionButton(ns("Show"),label="PLOT")),
             tags$div(
                    id = paste0(ns("planshift_date"),"_container"),
               textInput(ns("planshift_date"), "date (day):", 0)
             ),
             tags$div(
                    id = paste0(ns("planshift_density"),"_container"),
                    textInput(ns("planshift_density"), "density (p/m2):", 0)
             ),
             tags$div(
                    id = paste0(ns("harvshift_date"),"_container"),
                    textInput(ns("harvshift_date"), "date (day):", 0)
             ),
             tags$div(
                    id = paste0(ns("fertshift_date"),"_container"),
                    textInput(ns("fertshift_date"), "date (day):", 0)
             ),
             tags$div(
                    id = paste0(ns("irrshift_date"),"_container"),
                    textInput(ns("irrshift_date"), "date (day):", 0)
             ),
             tags$div(
                    id = paste0(ns("fertshift_amount"),"_container"),
                    textInput(ns("fertshift_amount"), "amount (kg/ha):", 0)
             ),
             tags$div(
                    id = paste0(ns("irrshift_amount"),"_container"),
                    textInput(ns("irrshift_amount"), "amount (mm):", 0),
               tags$hr(id=ns("littleblackline")),
               tags$hr(id=ns("littleblacklinetwo"))
             )
           )
  )
  }





agroMoSite <- function(input, output, session, dataenv){
  managementExt <- c("planting" = "plt", "harvest" = "hrv",
                     "fertilization" = "frt",
                     "irrigation" = "irr",
                     "grazing" = "grz",
                     "mowing" = "mow",
                     "thinning" = "thn")

  manReactive <- reactiveValues(included=NULL)
  managementRows <- c("plt" = 5, "thn" =  9, "mow" = 13, "grz"= 17, "hrv"= 21, "plo" = 25, "frt" = 29, "irr" = 33)
  dat <- reactiveValues(dataenv = dataenv, trigger = 0, show = 0)
  output$outputFile <- renderUI({
    ns <- session$ns
    modellOutputs <- c(dataenv(),input$iniFile)
    tagList(
      tags$div(id = "outputF", class = "inFile", selectizeInput(ns("outFile"),"OUTPUT id:",modellOutputs,selected = iniFile(),options = list(create = TRUE)))
    )
    })

  iniFile <- reactive({input$iniFile})
  mgmFile <- reactive({input$managementFile})
  observe({
     settings <- setupGUI(iniFile())
     print(iniFile())
     if(settings$epc != ""){
       updateSelectInput(session,"soilFile", selected = settings$soil)
       updateSelectInput(session,"weatherFile", selected = settings$meteo)
       updateSelectInput(session,"managementFile", selected = settings$mgm)
     }

   })
  updateSelectInput(session,"cultivation",selected = NA)
  observe({
    manReactive$included <- sapply(names(managementExt),function(manName){
      if(mgmFile()=="no management"){
        mgmF <- ""
      } else  {
        mgmF<- readLines(sprintf("input/management/%s",mgmFile()))
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


  callModule(runAndPlot,"popRun", reactive({input$iniFile}),
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
             reactive({input$irrshift_amount}))
 
   return(dat)
}
