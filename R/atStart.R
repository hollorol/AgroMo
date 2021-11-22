centralDataToVarTable <- function(){
    loadNamespace("RBBGCMuso")
    varTable <- options()$AgroMo_centralData[,c("VARCODE","VARIABLE","FROM","LABEL NAME")]
    varTable <- varTable[varTable$VARCODE != "",]
    varTable[,"VARCODE"] <- as.numeric(varTable[,"VARCODE"])
    colnames(varTable) <- c("codes","names","units","descriptions")
    options(RMuso_varTable=list("6"=varTable))
}

.onLoad <- function(libname, pkgname) {
  ## future::plan(multisession)
  print("This is AgroMo 1.0")
  options(AgroMo_centralData=suppressWarnings(jsonlite::read_json(system.file("centralData.json", package = "AgroMo"), simplifyVector=TRUE))) 
  options(AgroMo_depTree=suppressWarnings(readRDS(system.file("depTree.RDS", package= "AgroMo"))))
  centralDataToVarTable()
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
