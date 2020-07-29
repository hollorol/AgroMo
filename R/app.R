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
            hidden(agroMoDBManUI(id = "dbmandiv")),
            hidden(BBGCUI(id="BBGCDB")),
            hidden(actionButton(inputId = "base_bb_button",label="",title="Navigateto the BASE window", style="background: url('www/img/base.png');background-size: 75px 30px;", draggable = FALSE)),
            hidden(actionButton(inputId = "plot_bb_button",label="",title="Navigate to the PLOT window", style="background: url('www/img/plot.png');background-size: 75px 30px;", draggable = FALSE)),
            hidden(actionButton(inputId = "map_bb_button_two",label="",title="Navigate to the MAP window", style="background: url('www/img/map.png');background-size: 75px 30px;", draggable = FALSE)),
            hidden(actionButton(inputId = "site_bb_button_two",label="",title="Navigate to the SITE window", style="background: url('www/img/site.png');background-size: 75px 30px;", draggable = FALSE)),
            hidden(actionButton(inputId = "grid_bb_button_two",label="",title="Navigate to the GRID window", style="background: url('www/img/grid.png');background-size: 75px 30px;", draggable = FALSE))
            
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
    database <- file.path(baseDir,"output/site.db")
    dir.create(dirname(database), showWarnings = FALSE)
    baseConnection <- dbConnect(SQLite(),database)
    datas <- reactiveValues(baseDir = baseDir, connection=baseConnection, gridConnection=dbConnect(SQLite(), file.path(baseDir, "output/grid.db")))
    
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
                    database <- file.path(choosenDir, "database/observation.db")
                    datas$connection <- dbConnect(SQLite(), database)
                    datas$gridConnection <- dbConnect(SQLite(), file.path(choosenDir, "output/grid.db"))
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
        shinyjs::show("base_bb_button")
        shinyjs::hide("site_bb_button")
        shinyjs::hide("plot_bb_button")
        shinyjs::show("map_bb_button")
        shinyjs::show("grid_bb_button")
        shinyjs::show("site_bb_button_two")
        shinyjs::hide("plot_bb_button_two")
        shinyjs::hide("map_bb_button_two")
        shinyjs::hide("grid_bb_button_two")
        shinyjs::hide("mapdiv-mapdiv")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Show-banner-div")
        datas$dataenv <- dbListTables(datas$connection) 
      }
    })
    


    observeEvent(input$site,{
        shinyjs::hide("base")
        shinyjs::hide("base-tools")
        shinyjs::hide("mapdiv-mapdiv")
        shinyjs::hide("griddiv-griddiv")
        shinyjs::hide("showdiv-showdiv")
        shinyjs::show("sitediv-sitediv")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Site-banner-div")
        shinyjs::show("base_bb_button")
        shinyjs::hide("site_bb_button")
        shinyjs::hide("plot_bb_button")
        shinyjs::show("map_bb_button")
        shinyjs::show("grid_bb_button")
        shinyjs::hide("site_bb_button_two")
        shinyjs::show("plot_bb_button_two")
        shinyjs::hide("map_bb_button_two")
        shinyjs::hide("grid_bb_button_two")
    })
   }
   {
     observeEvent(input$base_bb_button,{
       shinyjs::show("base")
       shinyjs::show("base-tools")
       shinyjs::hide("sitediv-sitediv")
       shinyjs::hide("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Base-banner-div")
     })
   }
   {
     observeEvent(input$grid_bb_button,{
       shinyjs::show("griddiv-griddiv")
       shinyjs::hide("showdiv-showdiv")
       shinyjs::hide("sitediv-sitediv")
       shinyjs::show("base_bb_button")
       shinyjs::show("site_bb_button")
       shinyjs::show("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::show("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Grid-banner-div")
       })
   }
   {
     observeEvent(input$map_bb_button,{
       shinyjs::hide("base")
       shinyjs::hide("base-tools")
       shinyjs::hide("sitediv-sitediv")
       shinyjs::show("mapdiv-mapdiv")
       shinyjs::show("base_bb_button")
       shinyjs::show("site_bb_button")
       shinyjs::show("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::show("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Map-banner-div")
     })
   }
   {
     observeEvent(input$plot_bb_button_two,{
       shinyjs::show("showdiv-showdiv")
       shinyjs::hide("sitediv-sitediv")
       shinyjs::show("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::show("map_bb_button")
       shinyjs::show("grid_bb_button")
       shinyjs::show("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Show-banner-div")
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
        shinyjs::show("base_bb_button")
        shinyjs::show("site_bb_button")
        shinyjs::show("plot_bb_button")
        shinyjs::hide("map_bb_button")
        shinyjs::hide("grid_bb_button")
        shinyjs::hide("site_bb_button_two")
        shinyjs::hide("plot_bb_button_two")
        shinyjs::hide("map_bb_button_two")
        shinyjs::show("grid_bb_button_two")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Map-banner-div")
     })

     observeEvent(input$grid,{
       shinyjs::hide("base")
       shinyjs::hide("base-tools")
       shinyjs::show("griddiv-griddiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Grid-banner-div")
       shinyjs::show("base_bb_button")
       shinyjs::show("site_bb_button")
       shinyjs::show("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::show("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
     })
   }
   {
     observeEvent(input$base_bb_button,{
       shinyjs::show("base")
       shinyjs::show("base-tools")
       shinyjs::hide("griddiv-griddiv")
       shinyjs::hide("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Base-banner-div")
     })
   }
   {
     observeEvent(input$map_bb_button_two,{
       shinyjs::show("mapdiv-mapdiv")
       shinyjs::hide("griddiv-griddiv")
       shinyjs::hide("base")
       shinyjs::show("base_bb_button")
       shinyjs::show("site_bb_button")
       shinyjs::show("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::show("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Map-banner-div")
     })
   }
   {
     observeEvent(input$site_bb_button,{
       shinyjs::show("sitediv-sitediv")
       shinyjs::hide("griddiv-griddiv")
       shinyjs::show("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::show("map_bb_button")
       shinyjs::show("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::show("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Site-banner-div")
     })
   }
   {
     observeEvent(input$plot_bb_button,{
       shinyjs::show("showdiv-showdiv")
       shinyjs::hide("griddiv-griddiv")
       shinyjs::show("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::show("map_bb_button")
       shinyjs::show("grid_bb_button")
       shinyjs::show("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Show-banner-div")
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
        shinyjs::show("base_bb_button")
        shinyjs::show("site_bb_button")
        shinyjs::show("plot_bb_button")
        shinyjs::hide("map_bb_button")
        shinyjs::hide("grid_bb_button")
        shinyjs::hide("site_bb_button_two")
        shinyjs::hide("plot_bb_button_two")
        shinyjs::hide("map_bb_button_two")
        shinyjs::show("grid_bb_button_two")
      })
    }
    {
      observeEvent(input$base_bb_button,{
              shinyjs::show("base")
        shinyjs::show("base-tools")
       shinyjs::hide("sitegendiv-sitegendiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::hide("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::show("Base-banner-div")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
      })
      }
      {
      observeEvent(input$grid_bb_button_two,{
        shinyjs::show("grid")
        shinyjs::show("griddiv-griddiv")
       shinyjs::hide("sitegendiv-sitegendiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Grid-banner-div")
       shinyjs::show("base_bb_button")
       shinyjs::show("site_bb_button")
       shinyjs::show("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::show("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
      })
      }
   {
     observeEvent(input$site_bb_button,{
       shinyjs::show("sitediv-sitediv")
       shinyjs::hide("sitegendiv-sitegendiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Site-banner-div")
       shinyjs::show("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::show("map_bb_button")
       shinyjs::show("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::show("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
     })
   }
   {
     observeEvent(input$plot_bb_button,{
       shinyjs::show("showdiv-swowdiv")
       shinyjs::hide("sitegendiv-sitegendiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Show-banner-div")
       shinyjs::show("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::show("map_bb_button")
       shinyjs::show("grid_bb_button")
       shinyjs::show("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
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
        shinyjs::show("base_bb_button")
        shinyjs::show("site_bb_button")
        shinyjs::show("plot_bb_button")
        shinyjs::hide("map_bb_button")
        shinyjs::hide("grid_bb_button")
        shinyjs::hide("site_bb_button_two")
        shinyjs::hide("plot_bb_button_two")
        shinyjs::hide("map_bb_button_two")
        shinyjs::show("grid_bb_button_two")
      })
    }
    {
      observeEvent(input$grid_bb_button_two,{
        shinyjs::show("griddiv-griddv")
        shinyjs::hide("paranadiv-paranadiv")
        shinyjs::show("base_bb_button")
        shinyjs::show("site_bb_button")
        shinyjs::show("plot_bb_button")
        shinyjs::hide("map_bb_button")
        shinyjs::hide("grid_bb_button")
        shinyjs::hide("site_bb_button_two")
        shinyjs::hide("plot_bb_button_two")
        shinyjs::show("map_bb_button_two")
        shinyjs::hide("grid_bb_button_two")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Grid-banner-div")
      })
    }
    {
      observeEvent(input$site_bb_button,{
        shinyjs::show("sitediv-sitediv")
        shinyjs::hide("paranadiv-paranadiv")
        shinyjs::hide("backsite")
        shinyjs::hide(selector = ".banner")
        shinyjs::show("Site-banner-div")
        shinyjs::show("base_bb_button")
        shinyjs::hide("site_bb_button")
        shinyjs::hide("plot_bb_button")
        shinyjs::show("map_bb_button")
        shinyjs::show("grid_bb_button")
        shinyjs::hide("site_bb_button_two")
        shinyjs::show("plot_bb_button_two")
        shinyjs::hide("map_bb_button_two")
        shinyjs::hide("grid_bb_button_two")
      })
    }
   {
     observeEvent(input$plot_bb_button,{
       shinyjs::show("showdiv-showdiv")
       shinyjs::hide("paranadiv-paranadiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Show-banner-div")
       shinyjs::show("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::show("map_bb_button")
       shinyjs::show("grid_bb_button")
       shinyjs::show("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
     })
   }    
   {
     observeEvent(input$base_bb_button,{
       shinyjs::show("base")
       shinyjs::show("base-tools")
       shinyjs::hide("paranadiv-paranadiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Base-banner-div")
       shinyjs::hide("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
     })
   }   
        
   ## DBMAN
   {
     
     callModule(agroMoDBMan,"dbmandiv", datas$baseDir, datas$gridConnection, datas$connection)
     observeEvent(input$calibration,{
       shinyjs::hide("base")
       shinyjs::hide("base-tools")
       shinyjs::show("dbmandiv-dbmandiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("DBMan-banner-div")
       shinyjs::show("base_bb_button")
       shinyjs::show("site_bb_button")
       shinyjs::show("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::show("grid_bb_button_two")
     })
   }
   {
     observeEvent(input$grid_bb_button_two,{
       shinyjs::show("griddiv-griddv")
       shinyjs::hide("dbmandiv-dbmandiv")
       shinyjs::show("base_bb_button")
       shinyjs::show("site_bb_button")
       shinyjs::show("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::show("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Grid-banner-div")
     })
   }
   {
     observeEvent(input$site_bb_button,{
       shinyjs::show("sitediv-sitediv")
       shinyjs::hide("dbmandiv-dbmandiv")
       shinyjs::hide("backsite")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Site-banner-div")
       shinyjs::show("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::show("map_bb_button")
       shinyjs::show("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::show("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
     })
   }
   {
     observeEvent(input$plot_bb_button,{
       shinyjs::show("showdiv-showdiv")
       shinyjs::hide("dbmandiv-dbmandiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Show-banner-div")
       shinyjs::show("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::show("map_bb_button")
       shinyjs::show("grid_bb_button")
       shinyjs::show("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
     })
   }    
   {
     observeEvent(input$base_bb_button,{
       shinyjs::show("base")
       shinyjs::show("base-tools")
       shinyjs::hide("dbmandiv-dbmandiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Base-banner-div")
       shinyjs::hide("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
     })
   }   
   
   ## MAP
   {
     callModule(agroMoMap,"mapdiv",baseDir=reactive({datas$baseDir}),reactive({griddi$showMap}))
     observeEvent(input$map,{
       shinyjs::hide("base")
       shinyjs::hide("base-tools")
       shinyjs::show("mapdiv-mapdiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Map-banner-div")
       shinyjs::show("base_bb_button")
       shinyjs::show("site_bb_button")
       shinyjs::show("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::show("grid_bb_button_two")
     })
   }
   
   {
     observeEvent(input$base_bb_button,{
       shinyjs::show("base")
       shinyjs::show("base-tools")
       shinyjs::hide("mapdiv-mapdiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::hide("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::show("Base-banner-div")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
     })
   }
   {
     observeEvent(input$grid_bb_button_two,{
       shinyjs::show("grid")
       shinyjs::show("griddiv-griddiv")
       shinyjs::hide("mapdiv-mapdiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Grid-banner-div")
       shinyjs::show("base_bb_button")
       shinyjs::show("site_bb_button")
       shinyjs::show("plot_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::show("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
     })
   }
   {
     observeEvent(input$site_bb_button,{
       shinyjs::show("sitediv-sitediv")
       shinyjs::hide("mapdiv-mapdiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Site-banner-div")
       shinyjs::show("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::show("map_bb_button")
       shinyjs::show("grid_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::show("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
     })
   }
   {
     observeEvent(input$plot_bb_button,{
       shinyjs::show("showdiv-swowdiv")
       shinyjs::hide("mapdiv-mapdiv")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Show-banner-div")
       shinyjs::show("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::show("map_bb_button")
       shinyjs::show("grid_bb_button")
       shinyjs::show("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
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
        shinyjs::show("base_bb_button")
        shinyjs::hide("site_bb_button")
        shinyjs::hide("plot_bb_button")
        shinyjs::show("map_bb_button")
        shinyjs::show("grid_bb_button")
        shinyjs::show("site_bb_button_two")
        shinyjs::hide("plot_bb_button_two")
        shinyjs::hide("map_bb_button_two")
        shinyjs::hide("grid_bb_button_two")
        datas$dataenv <- dbListTables(datas$connection) 
      })
}
   {
     observeEvent(input$base_bb_button,{
       shinyjs::show("base")
       shinyjs::show("base-tools")
       shinyjs::hide("showdiv-showdiv")
       shinyjs::hide("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Base-banner-div")
       shinyjs::hide("BBGCDB")
     })
   }
   {
     observeEvent(input$site_bb_button_two,{
       shinyjs::show("sitediv-sitediv")
       shinyjs::hide("showdiv-showdiv")
       shinyjs::show("base_bb_button")
       shinyjs::hide("site_bb_button")
       shinyjs::show("grid_bb_button")
       shinyjs::show("map_bb_button")
       shinyjs::hide("plot_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::show("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Site-banner-div")
     })
   }
   {
     observeEvent(input$map_bb_button,{
       shinyjs::show("mapdiv-mapdiv")
       shinyjs::hide("showdiv-showdiv")
       shinyjs::show("base_bb_button")
       shinyjs::show("site_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::show("plot_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::hide("map_bb_button_two")
       shinyjs::show("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Map-banner-div")
     })
   }
   {
     observeEvent(input$grid_bb_button,{
       shinyjs::show("griddiv-griddiv")
       shinyjs::hide("showdiv-showdiv")
       shinyjs::show("base_bb_button")
       shinyjs::show("site_bb_button")
       shinyjs::hide("grid_bb_button")
       shinyjs::hide("map_bb_button")
       shinyjs::show("plot_bb_button")
       shinyjs::hide("site_bb_button_two")
       shinyjs::hide("plot_bb_button_two")
       shinyjs::show("map_bb_button_two")
       shinyjs::hide("grid_bb_button_two")
       shinyjs::hide(selector = ".banner")
       shinyjs::show("Grid-banner-div")
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
     observeEvent(input$storEditor,{
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
        tryCatch(system(sprintf("%s",file.path(system.file("tool",package="AgroMo"),"AgroMo_Apps.exe"))), warning=function(e){
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
