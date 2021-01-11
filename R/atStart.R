.onLoad <- function(libname, pkgname) {
  ## future::plan(multisession)
  print("This is AgroMo 1.0")
  options(AgroMo_centralData=suppressWarnings(jsonlite::read_json(system.file("centralData.json", package = "AgroMo"), simplifyVector=TRUE))) 
  options(AgroMo_depTree=suppressWarnings(readRDS(system.file("depTree.RDS", package= "AgroMo"))))
  shiny::addResourcePath('defaultDir',
                         system.file('defaultDir',
                                      package = 'AgroMo'))
 
   # Changing the font on maps from Arial to Fira Sans:
  showtext_auto()
  font_add("fira", file.path(system.file("www", package = "AgroMo"), "font/FiraSans-Regular.ttf"))
  font_add("fira_title", file.path(system.file("www", package = "AgroMo"), "font/FiraSans-Bold.ttf"))
  
# print(system.file('www',
#                                      package = 'AgroMo'))
  shiny::addResourcePath('www',
                         system.file('www',
                                     package = 'AgroMo'))
}
