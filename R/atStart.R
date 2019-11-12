.onLoad <- function(libname, pkgname) {
  ## future::plan(multisession)
  print("This is AgroMo")
  shiny::addResourcePath('defaultDir',
                         system.file('defaultDir',
                                      package = 'AgroMo'))
# print(system.file('www',
#                                      package = 'AgroMo'))
  shiny::addResourcePath('www',
                         system.file('www',
                                     package = 'AgroMo'))
}
