.onAttach <- function(libname, pkgname) {
  ## future::plan(multisession)
  print("This is AgroMo")
  shiny::addResourcePath('defaultDir',
                         system.file('defaultDir',
                                      package = 'agromR'))
print(system.file('www',
                                     package = 'agromR'))
  shiny::addResourcePath('www',
                         system.file('www',
                                     package = 'agromR'))
}
