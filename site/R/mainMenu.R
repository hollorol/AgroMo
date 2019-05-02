mainMenu <- function(name,relVals){
  shinyjs::hide("base")
  shinyjs::show(sprintf("%sdiv-%sdiv",name,name))
  shinyjs::hide(selector = ".banner")
  shinyjs::show(sprintf("%s-banner-div",gsub("(^[[:alpha:]])", "\\U\\1", name, perl=TRUE)))
  if(relVals[[sprintf("%sRun",name)]] == 0){
    modulName <- sprintf("agroMo%s",gsub("(^[[:alpha:]])", "\\U\\1", name, perl=TRUE))
    if(modulName == "agroMoSite"){
      callModule(match.fun(modulName),sprintf("%sdiv",name),isolate(input$iniFile))

    } else {
      callModule(match.fun(modulName),sprintf("%sdiv",name))
    }
    relVals[[sprintf("%sRun",name)]] <- 1
  }
}


## sinyjs::hide("base")
## shinyjs::show("sitediv-sitediv")
## shinyjs::hide(selector = ".banner")
## shinyjs::show("Site-banner-div")
## if(relVals$siteRun == 0){
##   callModule(agroMoSite,"sitediv",isolate(input$iniFile))
##   relVals$siteRun <- 1
## }

