library(shiny)
library(shinyjs)
library(plotly)
library(promises)
library(future)
library(data.table)
library(rhandsontable)
plan(multisession)
library(gtools)
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
    hidden(agroMoShowUI(id = "showdiv"))
    )
)

server <- function(input, output, session) {
## Comming Soons
  ##   observeEvent(input$first,{commingSoon()})#+FROM: commingSoon.r
  ##REACTIVE VALUES

  dataenv <- if(file.exists("outputs.RDS")) {
               readRDS("output/outputs.RDS")
             } else {
               new.env()
             }

  datas <- reactiveValues(dataenv = dataenv)
  relVals <- reactiveValues(siteRun = 0, showRun = 0, dataenv = dataenv)
  renderBanner(output)
  observeEvent(input$site,mainMenu("site",relVals)) # there exist a modul called agroMoSiteUI, and  agroMoSite, with id sitediv
  observeEvent(input$show,mainMenu("show",relVals)) 
  onclick("Site-banner-div",{
    shinyjs::hide("sitediv-sitediv")
    shinyjs::show("base")
    shinyjs::show("base-tools")
    shinyjs::hide(selector = ".banner")
    shinyjs::show("Base-banner-div")
  })

  onclick("Show-banner-div",{
    shinyjs::hide("showdiv-showdiv")
    shinyjs::show("base")
    shinyjs::show("base-tools")
    shinyjs::hide(selector = ".banner")
    shinyjs::show("Base-banner-div")
  })


}

## Run the application
shinyApp(ui = ui, server = server)
