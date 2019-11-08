#' agroUI
#'
#' This is the main UI function for the agromo modell
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs hidden
#' @importFrom shiny fluidPage getShinyOption
#' @keywords internal
agroUI <- function(){
    setwd(getShinyOption("AgroMoData")) # The user interface function runs at first, not the server... horrible
    fluidPage(
        baseHead(),
        useShinyjs(),
        createLayout(
            agromoBaseUI(id= "base"),
            hidden(agroMoSiteUI(id = "sitediv")),
            hidden(agroMoShowUI(id = "showdiv"))#,
           # hidden(agroMoGridUI(id = "griddiv"))
        )
    )
}

#' agroServer
#'
#' This is the server function for the agromo modell
#' @param input Environment of the GUI input elements
#' @param output Environment of the output code. This will we rendered
#' @param session environment which is a channel between R and the Web environment
#' @importFrom shinyjs hide show onclick
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom jsonlite read_json
#' @keywords internal
agroServer <- function(input, output, session) {

    observeEvent(input$exit,{
        browser()
        if(Sys.info()["sysname"] == "Windows"){
            system("taskkill /IM nw.exe /F")
        }
        stopApp() 
    })

    baseDir <- getShinyOption("AgroMoData")
    centralData <- read_json(system.file("centralData.json",package="AgroMo"),simplifyVector = TRUE)
    setwd(baseDir)
    database <- file.path(baseDir,"output/outputs.db")
    dir.create(dirname(database), showWarnings = FALSE)
    baseConnection <- dbConnect(SQLite(),database)
    datas <- reactiveValues(baseDir = baseDir, connection=baseConnection)
    {
        observeEvent(input$choose,{
            choosenDir <- tcltk::tk_choose.dir()
            output$mdd <- renderText({choosenDir})
            if(!checkDirStucture(choosenDir)) {
                output$mdd <- renderText({sprintf("ERROR: not valid directory structure; last working: %s", datas$baseDir)})
            } else {
                setwd(choosenDir)
                datas$baseDir <- choosenDir
                dbDisconnect(datas$connection)
                database <- file.path(choosenDir, "output/outputs.db")
                datas$connection <- dbConnect(SQLite(), database)
            }
            
        })
    }
    ## BASE "MODULE"
    onclick("Site-banner-div",{
        hide("sitediv-sitediv")
        show("base")
        show("base-tools")
        hide(selector = ".banner")
        show("Base-banner-div")
    })
    
    onclick("Show-banner-div",{
     shinyjs::hide("showdiv-showdiv")
      shinyjs::show("base")
      shinyjs::show("base-tools")
      shinyjs::hide(selector = ".banner")
      shinyjs::show("Base-banner-div")
    })

    ## onclick("Grid-banner-div",{
    ##   shinyjs::hide("griddiv-griddiv")
    ##   shinyjs::show("base")
    ##   shinyjs::show("base-tools")
    ##   shinyjs::hide(selector = ".banner")
    ##   shinyjs::show("Base-banner-div")
    ## })


    # SITE MODULE
{
    # browser()
    dat <- callModule(agroMoSite,"sitediv",
                      dataenv = reactive(datas$dataenv),
                      baseDir = reactive({datas$baseDir}),
                      reactive({datas$connection}),centralData=centralData)

    observeEvent(dat$show,{
      if(dat$show > 0){

        shinyjs::hide("sitediv-sitediv")
        shinyjs::show("showdiv-showdiv")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Show-banner-div")
        datas$dataenv <- dbListTables(datas$connection) 
      }
    })
    


    observeEvent(input$site,{
        shinyjs::hide("base")
        shinyjs::hide("base-tools")
        shinyjs::show("sitediv-sitediv")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Site-banner-div")
    })
}
    ## ##  SHOW MODUL
{
    callModule(agroMoShow,"showdiv",dataenv = reactive(datas$dataenv),baseDir = reactive(datas$baseDir), connection = reactive({datas$connection}),centralData=centralData)


      observeEvent(input$show,{
        shinyjs::hide("base")
        shinyjs::hide("base-tools")
        shinyjs::show("showdiv-showdiv")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Show-banner-div")
        datas$dataenv <- dbListTables(datas$connection) 
      })
     }
}

#' launchApp
#'
#' launchApp launch the shiny app
#' @param ... Other parameters for shinyApp function
#' @importFrom shiny shinyApp shinyOptions
#' @export
launchApp <- function(directory = NULL,...){ 

    dataDir <- new.env()      
    if(is.null(directory)){
        shinyOptions(AgroMoData = system.file("defaultDir", package = "AgroMo"))
    } else {
        shinyOptions(AgroMoData = directory)
    }
        shinyApp(ui = agroUI(), server = agroServer, options = list(...))
}
