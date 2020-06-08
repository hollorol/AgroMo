.onLoad <- function(libname, pkgname) {
  ## future::plan(multisession)
  print("This is AgroMo")
  options(AgroMo_centralData=suppressWarnings(jsonlite::read_json(system.file("centralData.json", package = "AgroMo"), simplifyVector=TRUE))) 
  shiny::addResourcePath('defaultDir',
                         system.file('defaultDir',
                                      package = 'AgroMo'))
 
   # Changing the font on maps from Arial to Fira Sans:
  showtext_auto()
  font_add("fira", file.path(system.file("www", package = "AgroMo"), "font/FiraSans-Light.ttf"))
# print(system.file('www',
#                                      package = 'AgroMo'))
  shiny::addResourcePath('www',
                         system.file('www',
                                     package = 'AgroMo'))
}
