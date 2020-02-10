.onLoad <- function(libname, pkgname) {
  ## future::plan(multisession)
  print("This is AgroMo")
  options(AgroMo_centralData=suppressWarnings(jsonlite::read_json(system.file("centralData.json", package = "AgroMo"), simplifyVector=TRUE))) 
  shiny::addResourcePath('defaultDir',
                         system.file('defaultDir',
                                      package = 'AgroMo'))
# print(system.file('www',
#                                      package = 'AgroMo'))
  shiny::addResourcePath('www',
                         system.file('www',
                                     package = 'AgroMo'))
}
