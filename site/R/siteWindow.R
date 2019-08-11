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
             tags$div(
                    id = paste0(ns("planting"),"_container"),
               selectInput(ns("planting"),"planting:",NA)
             ),
             tags$div(
                    id = paste0(ns("harvest"),"_container"),
               selectInput(ns("harvest"),"harvest:",NA)
             ),
             tags$div(
                    id = paste0(ns("fertilization"),"_container"),
               selectInput(ns("fertilization"),"fertilization:",NA)
             ),
             tags$div(
                    id = paste0(ns("irrigation"),"_container"),
               selectInput(ns("irrigation"),"irrigation:",NA)
             ),
             tags$div(
                    id = paste0(ns("cultivation"),"_container"),
               selectInput(ns("cultivation"),"cultivation:",NA)
             ),
             tags$div(
                    id = paste0(ns("grazing"),"_container"),
               selectInput(ns("grazing"),"grazing:",NA)
             ),
             tags$div(
                    id = paste0(ns("mowing"),"_container"),
               selectInput(ns("mowing"),"mowing:",NA)
             ),
             tags$div(
                    id = paste0(ns("thinning"),"_container"),
               selectInput(ns("thinning"),"thinning:",NA)
             ),
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
  dat <- reactiveValues(dataenv = dataenv,trigger = 0)
  output$outputFile <- renderUI({
    ns <- session$ns
    modellOutputs <- c(dataenv(),input$iniFile)
    tagList(
      tags$div(id = "outputF", class = "inFile", selectizeInput(ns("outFile"),"OUTPUT id:",modellOutputs,selected = iniFile(),options = list(create = TRUE)))
    )
    })

  iniFile <- reactive({input$iniFile})
  observe({
     settings <- setupGUI(iniFile())
     print(iniFile())
     if(settings[[1]] != ""){
       updateSelectInput(session,"soilFile", selected = settings$soil)
       updateSelectInput(session,"weatherFile", selected = settings$meteo)
       updateSelectInput(session,"managementFile", selected = settings$mgm)
     }

   })


    callModule(runAndPlot,"popRun", reactive({input$iniFile}), reactive({input$weatherFile}), reactive({input$soilFile}), reactive({input$managementFile}),reactive({input$outFile}))#,reactive(dat$dataenv))
  
   return(dat)
}
