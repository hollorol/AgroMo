#debug(setupGUI)
createInputElements <- function(baseTable){
    apply(baseTable, 1,function (x){
        if(!grepl("management",x[1])){
            selectInput(x[1],x[2],basename(grep(list.files(x[3]), pattern = x[4], value = TRUE)),width = "100%")
        } else {
            selectInput(x[1],x[2],c(basename(grep(list.files(x[3]), pattern = x[4], value = TRUE)),"no management"),width = "100%")
        }
    })
}

createInputs <- function(baseTable){
  tags$div(id = "fileOutput", class="inFile",createInputElements(baseTable = baseTable))
}

agroMoSiteUI <- function(id){
  ns <- NS(id)

  baseTable<- data.frame(selectorId <- c(ns("iniFile"), ns("weatherFile"), ns("soilFile"), ns("managementFile")),
                         label <- c("INI FILE:", "WEATHER FILE:", "SOIL FILE:", "MANAGEMENT FILE:"),
                         place <- c("input/initialization/", "input/weather", "input/soil", "input/management"),
                         pattern <- c("*.ini","*.wth","*.soi","*.mgm"))

  tags$div(id = ns(id),
           tagList(
             createInputs(baseTable),
             tags$div(
                    id = paste0(ns("stationp"),"_container"),
                    checkboxInput(ns("stationp"), label = "Station data only", value = TRUE)
                  ),
             tags$div(
                    id =paste0(ns("sitep"),"_container"),
                    checkboxInput(ns("sitep"), label = "Site data only", value = TRUE)
                  ),
             tags$div(id="manModuls","management modules"),
             tags$div(id="shiftIn","shift in ..."),
             tags$div(
                    id = paste0(ns("planting"),"_container"),
               selectInput(ns("planting"),"planting",NA)
             ),
             tags$div(
                    id = paste0(ns("harvest"),"_container"),
               selectInput(ns("harvest"),"harvest",NA)
             ),
             tags$div(
                    id = paste0(ns("fertilization"),"_container"),
               selectInput(ns("fertilization"),"fertilization",NA)
             ),
             tags$div(
                    id = paste0(ns("irrigation"),"_container"),
               selectInput(ns("irrigation"),"irrigation",NA)
             ),
             tags$div(
                    id = paste0(ns("cultivation"),"_container"),
               selectInput(ns("cultivation"),"cultivation",NA)
             ),
             tags$div(
                    id = paste0(ns("grazing"),"_container"),
               selectInput(ns("grazing"),"grazing",NA)
             ),
             tags$div(
                    id = paste0(ns("mowing"),"_container"),
               selectInput(ns("mowing"),"mowing",NA)
             ),
             tags$div(
                    id = paste0(ns("thinning"),"_container"),
               selectInput(ns("thinning"),"thinning",NA)
             ),
             uiOutput(ns("outputFile")),
             tags$div(id = ns("Buttons"),
             runAndPlotUI(ns("popRun"),label = "Run"),
             actionButton(ns("Show"),label="Show")),
             tags$div(
                    id = paste0(ns("planshift_date"),"_container"),
               numericInput(ns("planshift_date"), "date (day):", 0)
             ),
             tags$div(
                    id = paste0(ns("planshift_density"),"_container"),
               numericInput(ns("planshift_density"), "density (p/m2):", 0)
             ),
             tags$div(
                    id = paste0(ns("harvshift_date"),"_container"),
               numericInput(ns("harvshift_date"), "date (day):", 0)
             ),
             tags$div(
                    id = paste0(ns("fertshift_date"),"_container"),
               numericInput(ns("fertshift_date"), "date (day):", 0)
             ),
             tags$div(
                    id = paste0(ns("irrshift_date"),"_container"),
               numericInput(ns("irrshift_date"), "date (day):", 0)
             ),
             tags$div(
                    id = paste0(ns("fertshift_amount"),"_container"),
               numericInput(ns("fertshift_amount"), "amount (kg/ha):", 0)
             ),
             tags$div(
                    id = paste0(ns("irrshift_amount"),"_container"),
               numericInput(ns("irrshift_amount"), "amount (mm):", 0)
             )
           )
           )}




agroMoSite <- function(input, output, session, outFile){
  dat <- reactiveValues()
  output$outputFile <- renderUI({
    ns <- session$ns
    modellOutputs <- c(ls(readRDS("output/outputs.RDS")),input$iniFile)
    tagList(
      tags$div(id = "outputF", class = "inFile", selectizeInput(ns("outFile"),"OUTPUT:",modellOutputs,selected = iniFile(),options = list(create = TRUE)))
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

  output$plots <- renderUI({
    plot_output_list <- lapply(1:13, function(i) {##14 is hardcode for now
      plotname <- paste("plot", i, sep="")
      plotlyOutput(plotname)
    })

                                        # Convert the list to a tagList - this is necessary for the list of items
                                        # to display properly.
    do.call(tagList, plot_output_list)
  })

observeEvent(input$Run,{print(input$outFile)})

  dat <- callModule(runAndPlot,"popRun", reactive({input$iniFile}), reactive({input$weatherFile}), reactive({input$soilFile}), reactive({input$managementFile}),reactive({input$outFile}))
return(dat)
   ##   observeEvent(input$Run,{
   ##       settings <- setupGUI(iniFile())
   ##     if(is.element(input$outFile, ls(outputs))){

   ##         columNames <- sapply(settings$variableNames,function (x) {paste0("~",x)})

   ##          for (i in 1:13) {
   ##                                       # Need local so that each item gets its own number. Without it, the value
   ##                                       # of i in the renderPlot() will be the same across all instances, because
   ##                                       # of when the expression is evaluated.
   ##              local(
   ##              {my_i <- i
   ##                  plotname <- paste("plot", my_i, sep="")
   ##                  output[[plotname]] <- renderPlotly({
   ##                      plot_ly(outputs[[input$outFile]], x=~date,y = as.formula(columNames[my_i]),mode = "line")
   ##                  })}
   ##              )
        
   ##          }
           
   ##     }
   ##     ##else {
   ## ##         print("Running the model")
   ## ##         system(paste0("./muso ","input/initialization/",input$iniFile))
   ## ##         writeDataToEnv(settings, outputs, "output/tmp.dayout", input$outFile)
   ## ##         file.remove("output/tmp.dayout")
           
   ## ##         output$plots <- renderUI({
   ## ##             plot_output_list <- lapply(1:13, function(i) {##14 is hardcode for now
   ## ##                 plotname <- paste("plot", i, sep="")
   ## ##                 plotlyOutput(plotname)
   ## ##             })
               
   ## ##                                      # Convert the list to a tagList - this is necessary for the list of items
   ## ##                                      # to display properly.
   ## ##             do.call(tagList, plot_output_list)
   ## ##         })

   ## ##         columNames <- sapply(settings$variableNames,function (x) {paste0("~",x)})
   ## ##         data <- outputs[[input$outFile]]
   ## ##         print(colnames(data))
   ## ##         print(columNames)

   ## ##         for (i in 1:13) {
   ## ##                                      # Need local so that each item gets its own number. Without it, the value
   ## ##                                      # of i in the renderPlot() will be the same across all instances, because
   ## ##                                      # of when the expression is evaluated.
   ## ##             local(
   ## ##             {my_i <- i
   ## ##                 plotname <- paste("plot", my_i, sep="")
                   
   ## ##                  output[[plotname]] <- renderPlotly({
   ## ##                      plot_ly(outputs[[input$outFile]], x=~date,y = as.formula(columNames[my_i]),mode = "line")
   ## ##             })
   ## ##         }
   ## ##             )
               
   ## ##         }

           
   ## ##     }
   ## ##     hide("mainWindow")
   ## ##     show("plotWindow")
   ## ## })
   ## ## observeEvent(input$backMain,{
   ## ##     show("mainWindow")
   ## ##     hide("plotWindow")
   ## ## })


   ##   })
  return(dat)
     }
