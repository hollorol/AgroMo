#' agroUI
#'
#' This is the main UI function for the agromo modell
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shiny fluidPage getShinyOption actionButton
#' @keywords internal 
agroUI <- function(){
    setwd(getShinyOption("AgroMoData")) # The user interface function runs at first, not the server... horrible
    fluidPage(
        baseHead(),
        useShinyjs(),
        createLayout(
            agromoBaseUI(id= "base"),
            hidden(agroMoSiteUI(id = "sitediv")),
            hidden(agroMoShowUI(id = "showdiv")),
            hidden(agroMoGridUI(id = "griddiv")),
            hidden(agroMoMapUI(id = "mapdiv")),
            hidden(agroMoSiteGeneratorUI(id = "sitegendiv")),
            hidden(agroMoParAnaUI(id = "paranadiv")),
            hidden(BBGCUI(id="BBGCDB")),
            hidden(actionButton(inputId = "basearrow",label="",title="Navigate back to the BASE window", style="background: url('www/img/base_arrow.png');background-size: 75px 43px;", draggable = FALSE)),
            hidden(actionButton(inputId = "backsite",label="",title="Navigate back to the SITE window", style="background: url('www/img/back_site.png');background-size: 75px 43px;", draggable = FALSE)),
            hidden(actionButton(inputId = "basearrow_sg",label="",title="Navigate back to the BASE window", style="background: url('www/img/base_arrow_sg.png');background-size: 75px 43px;", draggable = FALSE)),
            hidden(actionButton(inputId = "backsite_sg",label="",title="Navigate back to the SITE window", style="background: url('www/img/back_site_sg.png');background-size: 75px 43px;", draggable = FALSE)),
            hidden(actionButton(inputId = "backgrid",label="",title="Navigate to the GRID window", style="background: url('www/img/back_grid.png');background-size: 75px 43px;", draggable = FALSE))
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
#' @importFrom shiny getShinyOption
#' @importFrom jsonlite read_json
#' @importFrom shinyFiles parseDirPath shinyDirChoose 
#' @keywords internal
agroServer <- function(input, output, session) {
    baseDir <- getShinyOption("AgroMoData")
    centralData <- getOption("AgroMo_centralData")
     
    print(baseDir)
    setwd(baseDir)
    database <- file.path(baseDir,"output/outputs.db")
    dir.create(dirname(database), showWarnings = FALSE)
    baseConnection <- dbConnect(SQLite(),database)
    datas <- reactiveValues(baseDir = baseDir, connection=baseConnection)
    
   session$onSessionEnded(stopApp)

    observeEvent(input$exit,{
        if(Sys.info()["sysname"] == "Windows"){
            system("taskkill /IM nw.exe /F")
        }
        try(dbDisconnect(baseConnection),silent=TRUE)
        setwd(getShinyOption("workdir"))
        stopApp() 
    })

   rootwd <-  c(Documents="~",C="C:/",D="D:/")
   shinyDirChoose(input, 'choose', root=rootwd)

    {
        observeEvent(input$choose,{
            # choosenDir <- tcltk::tk_choose.dir()
            choosenDir <- parseDirPath(roots=rootwd,selection=input$choose)
            if(length(choosenDir)!=0){
                output$mdd <- renderText({choosenDir})
                if(!checkDirStucture(choosenDir)) {
                    output$mdd <- renderText({sprintf("ERROR: not valid directory structure; last working: %s", datas$baseDir)})
                } else {
                    setwd(choosenDir)
                    #browser()
                    datas$baseDir <- choosenDir
                    dbDisconnect(datas$connection)
                    database <- file.path(choosenDir, "output/outputs.db")
                    datas$connection <- dbConnect(SQLite(), database)
                }
            }
            
        })
    }
    ## BASE "MODULE"
#    onclick("Site-banner-div",{
    #        hide("sitediv-sitediv")
    #    show("base")
    #    show("base-tools")
    #    hide(selector = ".banner")
    #    show("Base-banner-div")
    #})
    
   # onclick("Show-banner-div",{
    #  shinyjs::hide("showdiv-showdiv")
    #  shinyjs::show("base")
    #  shinyjs::show("base-tools")
    #  shinyjs::hide(selector = ".banner")
    #  shinyjs::show("Base-banner-div")
    #})

    #onclick("Grid-banner-div",{
    #  shinyjs::hide("griddiv-griddiv")
    #  shinyjs::show("base")
    #  shinyjs::show("base-tools")
    #  shinyjs::hide(selector = ".banner")
    #  shinyjs::show("Base-banner-div")
    #})
   
    #onclick("Map-banner-div",{
    # shinyjs::hide("mapdiv-mapdiv")
    # shinyjs::show("base")
    # shinyjs::show("base-tools")
    # shinyjs::hide(selector = ".banner")
    # shinyjs::show("Base-banner-div")
    #})

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
        shinyjs::hide("griddiv-griddiv")
        shinyjs::hide("backgrid")
        shinyjs::hide("basearrow_sg")
        shinyjs::hide("backsite_sg")
        shinyjs::show("backsite")
        shinyjs::hide("mapdiv-mapdiv")
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
        shinyjs::show("basearrow")
        shinyjs::show("backgrid")
        shinyjs::hide("backsite")
        shinyjs::hide("basearrow_sg")
        shinyjs::hide("backsite_sg")
    })
   }
   {
     observeEvent(input$basearrow,{
       shinyjs::show("base")
       shinyjs::show("base-tools")
       shinyjs::hide("sitediv-sitediv")
       shinyjs::hide("basearrow")
       shinyjs::hide("backgrid")
       shinyjs::hide("backsite")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Base-banner-div")
     })
   }
   {
     observeEvent(input$backgrid,{
       shinyjs::show("grid")
       shinyjs::show("griddiv-griddiv")
       shinyjs::hide("showdiv-showdiv")
       shinyjs::hide("sitediv-sitediv")
       shinyjs::hide("backgrid")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Grid-banner-div")
       shinyjs::show("basearrow")
       shinyjs::show("backsite")
     })
   }
   
   
  ## GRID
   {
     # browser()
     # dat <- callModule(agroMoGrid,"griddiv",
     #                   dataenv = reactive(datas$dataenv),
     #                   baseDir = reactive({datas$baseDir}),
     #                   reactive({datas$connection}),centralData=centralData)
     griddi <- callModule(agroMoGrid,"griddiv",baseDir=reactive({datas$baseDir}))
     
   observeEvent(griddi$showMap,{
        shinyjs::show("mapdiv-mapdiv")
        shinyjs::hide("griddiv-griddiv")
        shinyjs::hide("backgrid")
        shinyjs::hide("basearrow_sg")
        shinyjs::hide("backsite_sg")
        shinyjs::show("backsite")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Map-banner-div")
     })

     observeEvent(input$grid,{
       shinyjs::hide("base")
       shinyjs::hide("base-tools")
       shinyjs::show("griddiv-griddiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Grid-banner-div")
       shinyjs::show("basearrow")
       shinyjs::show("backsite")
       shinyjs::hide("backgrid")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
     })
   }
   {
     observeEvent(input$basearrow,{
       shinyjs::show("base")
       shinyjs::show("base-tools")
       shinyjs::hide("griddiv-griddiv")
       shinyjs::hide("basearrow")
       shinyjs::hide("backgrid")
       shinyjs::hide("backsite")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Base-banner-div")
     })
   }
   {
     observeEvent(input$backsite,{
       shinyjs::show("site")
       shinyjs::show("sitediv-sitediv")
       shinyjs::hide("griddiv-griddiv")
       shinyjs::hide("backsite")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Site-banner-div")
       shinyjs::show("basearrow")
       shinyjs::show("backgrid")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
       shinyjs::hide("BBGCDB")
     })
   }
 
    ## SITEGENERATOR
    {
      # browser()
      # dat <- callModule(agroMoGrid,"griddiv",
      #                   dataenv = reactive(datas$dataenv),
      #                   baseDir = reactive({datas$baseDir}),
      #                   reactive({datas$connection}),centralData=centralData)
      callModule(agroMoSiteGenerator,"sitegendiv")
      observeEvent(input$sitegen,{
        shinyjs::hide("base")
        shinyjs::hide("base-tools")
        shinyjs::show("sitegendiv-sitegendiv")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Sitegen-banner-div")
        shinyjs::show("basearrow_sg")
        shinyjs::show("backsite_sg")
        shinyjs::hide("backgrid")
        shinyjs::hide("backsite")
        shinyjs::hide("backarrow")
      })
    }
    {
      observeEvent(input$basearrow_sg,{
              shinyjs::show("base")
        shinyjs::show("base-tools")
       shinyjs::hide("sitegendiv-sitegendiv")
       shinyjs::hide("basearrow")
       shinyjs::hide("backgrid")
       shinyjs::hide("backsite")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Base-banner-div")
      })
      }
      {
      observeEvent(input$backsite_sg,{
       shinyjs::show("site")
       shinyjs::show("sitediv-sitediv")
       shinyjs::hide("sitegendiv-sitegendiv")
       shinyjs::hide("backsite")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Site-banner-div")
       shinyjs::show("basearrow")
       shinyjs::show("backgrid")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
      })
      }

    ## PARANA
    {
      
      callModule(agroMoParAna,"paranadiv")
      observeEvent(input$parana,{
        shinyjs::hide("base")
        shinyjs::hide("base-tools")
        shinyjs::show("paranadiv-paranadiv")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Parana-banner-div")
        shinyjs::show("basearrow_sg")
        shinyjs::show("backsite_sg")
        shinyjs::hide("backgrid")
        shinyjs::hide("backsite")
        shinyjs::hide("backarrow")
      })
    }
    {
      observeEvent(input$basearrow_sg,{
        shinyjs::show("base")
        shinyjs::show("base-tools")
        shinyjs::hide("paranadiv-paranadiv")
        shinyjs::hide("basearrow")
        shinyjs::hide("backgrid")
        shinyjs::hide("backsite")
        shinyjs::hide("basearrow_sg")
        shinyjs::hide("backsite_sg")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Base-banner-div")
      })
    }
    {
      observeEvent(input$backsite_sg,{
        shinyjs::show("site")
        shinyjs::show("sitediv-sitediv")
        shinyjs::hide("paranadiv-paranadiv")
        shinyjs::hide("backsite")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Site-banner-div")
        shinyjs::show("basearrow")
        shinyjs::show("backgrid")
        shinyjs::hide("basearrow_sg")
        shinyjs::hide("backsite_sg")
      })
    }
    
        
   ## MAP
   {
     callModule(agroMoMap,"mapdiv",baseDir=reactive({datas$baseDir}))
     observeEvent(input$map,{
       shinyjs::hide("base")
       shinyjs::hide("base-tools")
       shinyjs::show("mapdiv-mapdiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Map-banner-div")
       shinyjs::show("basearrow")
       shinyjs::show("backgrid")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
       shinyjs::hide("backsite")
     })
   }
   
   {
     observeEvent(input$basearrow,{
       shinyjs::show("base")
       shinyjs::show("base-tools")
       shinyjs::hide("mapdiv-mapdiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::hide("backgrid")
       shinyjs::hide("backsite")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
       shinyjs::show("Base-banner-div")
       shinyjs::hide("basearrow")
     })
   }
   {
     observeEvent(input$backgrid,{
       shinyjs::show("grid")
       shinyjs::show("griddiv-griddiv")
       shinyjs::hide("mapdiv-mapdiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Grid-banner-div")
       shinyjs::hide("backgrid")
       shinyjs::show("basearrow")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
       shinyjs::show("backsite")
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
        shinyjs::show("basearrow")
        shinyjs::show("backsite")
        shinyjs::hide("basearrow_sg")
        shinyjs::hide("backsite_sg")
        shinyjs::hide("backgrid")
        datas$dataenv <- dbListTables(datas$connection) 
      })
}
   {
     observeEvent(input$basearrow,{
       shinyjs::show("base")
       shinyjs::show("base-tools")
       shinyjs::hide("showdiv-showdiv")
       shinyjs::hide("basearrow")
       shinyjs::hide("backgrid")
       shinyjs::hide("backsite")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Base-banner-div")
       shinyjs::hide("BBGCDB")
     })
   }
   {
     observeEvent(input$backsite,{
       shinyjs::show("site")
       shinyjs::show("sitediv-sitediv")
       shinyjs::hide("showdiv-showdiv")
       shinyjs::hide("backsite")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Site-banner-div")
       shinyjs::show("basearrow")
       shinyjs::show("backgrid")
     })
   }

   ## ##  GRID MODUL
   ##{
   ##callModule(agroMoGrid,"griddiv",dataenv = reactive(datas$dataenv),baseDir = reactive(datas$baseDir), connection = reactive({datas$connection}),centralData=centralData)
   ## 
   ##
   ##observeEvent(input$grid,{
   ##  shinyjs::hide("base")
   ##  shinyjs::hide("base-tools")
   ##  shinyjs::show("griddiv-griddiv")
   ##  shinyjs::hide(selector = ".banner")
   ##  shinyjs::show("Grid-banner-div")
   ##  datas$dataenv <- dbListTables(datas$connection) 
   ##})
   ##}
   {
     observeEvent(input$calibration,{
       shinyjs::hide("base")
       shinyjs::hide("base-tools")
       shinyjs::hide("mapdiv-mapdiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("BDB-banner-div")
       shinyjs::show("basearrow")
       shinyjs::hide("backgrid")
       shinyjs::hide("basearrow_sg")
       shinyjs::hide("backsite_sg")
       shinyjs::hide("backsite")
       shinyjs::show("BBGCDB")
     })
   }

   observeEvent(input$storEditor,{
        tryCatch(system(sprintf("%s",file.path(system.file("tools",package="AgroMo"),"AgroMo_tools.exe"))), warning=function(e){
           showNotification("Currently Story editor is only usable from Windows", type="error")  
        }) 
     })
   
}


#' launchApp
#'
#' launchApp launch the shiny app
#' @param ... Other parameters for shinyApp function
#' @importFrom shiny shinyApp shinyOptions
#' @export
launchApp <- function(directory = NULL,...){ 
    shinyOptions(workdir = getwd())
    if(is.null(directory)){
        shinyOptions(AgroMoData = system.file("defaultDir", package = "AgroMo"))
    } else {
        shinyOptions(AgroMoData = normalizePath(directory))
    }
        shinyApp(ui = agroUI(), server = agroServer, options = list(...))
}
