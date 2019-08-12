library(shiny)
library(shinyjs)
library(plotly)
library(promises)
library(future)
library(data.table)
library(DT)

plan(multisession)
sapply(paste0("R/",list.files("R","\\.R$")), source) #sourcing all R files inside the R directory.

if(!file.exists("output/outputs.RDS")){
  e <- new.env()
  saveRDS(e, "output/outputs.RDS")
}

## Define UI
ui <- fluidPage(
    baseHead(), #+FROM: head.R
  useShinyjs(),
    baseWindow(
    agromoBaseUI(id= "base"), #
    hidden(agroMoSiteUI(id = "sitediv")),
    hidden(agroMoShowUI(id = "showdiv")),
    hidden(agroMoGridUI(id = "griddiv"))
    )
)

server <- function(input, output, session) {

  ## INITIALIZATION OF THE ENVIRONMENT
  dataenv <- if(file.exists("output/outputs.RDS")) {
               ls(readRDS("output/outputs.RDS"))
             } else {
               dataenv <- new.env()
               writeRDS("output/outputs.RDS")
               character(0)
             }

  gc(verbose = FALSE)
  datas <- reactiveValues(dataenv = dataenv, baseDir = "./")
  renderBanner(output)

  {
    observeEvent(input$choose,{
      baseDir <- tcltk::tk_choose.dir()
      datas$baseDir <- baseDir
      output$mdd <- renderText({baseDir})
    })
  }

  renderBanner(output)

  ##BASE "MODULE"

  onclick("Site-banner-div",{
    shinyjs::hide("sitediv-sitediv")
    shinyjs::show("base")
    shinyjs::show("base-tools")
    shinyjs::hide(selector = ".banner")
    shinyjs::show("Base-banner-div")
  }) ## How to put these segments into a function?

  onclick("Show-banner-div",{
    shinyjs::hide("showdiv-showdiv")
    shinyjs::show("base")
    shinyjs::show("base-tools")
    shinyjs::hide(selector = ".banner")
    shinyjs::show("Base-banner-div")
  })

  onclick("Grid-banner-div",{
    shinyjs::hide("griddiv-griddiv")
    shinyjs::show("base")
    shinyjs::show("base-tools")
    shinyjs::hide(selector = ".banner")
    shinyjs::show("Base-banner-div")
  })


  ## SITE MODULE
  {
  dat <- callModule(agroMoSite,"sitediv",dataenv = reactive(datas$dataenv))
  observeEvent(dat$trigger,{
    ## browser()
    if(dat$trigger > 0){
      ## browser()
      datas$dataenv <- dat$dataenv()
    }
    print(datas$dataenv)
  })

  observeEvent(dat$show,{
    if(dat$show > 0){

      shinyjs::hide("sitediv-sitediv")
      shinyjs::show("showdiv-showdiv")
      shinyjs::hide(selector = ".banner")
      shinyjs::show("Show-banner-div")
      datas$dataenv <- ls(readRDS("output/outputs.RDS"))
      gc(verbose = FALSE)
    }
  })
  }


  observeEvent(input$site,{
    shinyjs::hide("base")
    shinyjs::hide("base-tools")
    shinyjs::show("sitediv-sitediv")
    shinyjs::hide(selector = ".banner")
    shinyjs::show("Site-banner-div")
  })

  ## SHOW MODUL
  callModule(agroMoShow,"showdiv",dataenv = reactive(datas$dataenv))


  observeEvent(input$show,{
    shinyjs::hide("base")
    shinyjs::hide("base-tools")
    shinyjs::show("showdiv-showdiv")
    shinyjs::hide(selector = ".banner")
    shinyjs::show("Show-banner-div")
    datas$dataenv <- ls(readRDS("output/outputs.RDS"))
    gc(verbose = FALSE)
  })

  ## observeEvent(input$site,mainMenu("site",relVals)) # there exist a modul called agroMoSiteUI, and  agroMoSite, with id sitediv
  ## observeEvent(input$show,mainMenu("show",relVals))
  ## observeEvent(input$grid,mainMenu("grid",relVals))
}

## Run the application
shinyApp(ui = ui, server = server)
