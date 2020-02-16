#' checkDirStructure
#'
#' This function checks if the directory structure is correct for agromo
#' @param chosenPath Filepath to validate for AgroMo main directory structure
#' @export

checkDirStucture <- function(chosenPath){
    links <- c(
 "",
 "endpoint",
 "endpoint/grid",
 "endpoint/site",
 "input",
 "input/auxiliary",
 "input/initialization",
 "input/initialization/site",
 "input/management",
 "input/management/cultivation",
 "input/management/fertilization",
 "input/management/grazing",
 "input/management/harvest",
 "input/management/irrigation",
 "input/management/mowing",
 "input/management/planting",
 "input/management/thinning",
 "input/plant",
 "input/soil",
 "input/soil/grid",
 "input/soil/site",
 "input/storyline",
 "input/weather",
 "input/weather/grid",
 "input/weather/grid/observed",
 "input/weather/grid/projection",
 "input/weather/site",
 "observation",
 "output",
 "output/grid")

    dirs <- grep(".*tmp.*", list.dirs(chosenPath,full.names = FALSE), invert = TRUE, value = TRUE)
    all(sapply(links, function(x){
           is.element(x, dirs)
    }))

}




## checkDirStuctureProced <- function(chosenPath){
##     links <- c(
##  "",
##  "endpoint",
##  "endpoint/grid",
##  "endpoint/site",
##  "input",
##  "input/auxiliary",
##  "input/initialization",
##  "input/initialization/grid",
##  "input/initialization/grid/10km",
##  "input/initialization/site",
##  "input/management",
##  "input/management/cultivation",
##  "input/management/fertilization",
##  "input/management/grazing",
##  "input/management/harvest",
##  "input/management/irrigation",
##  "input/management/mowing",
##  "input/management/planting",
##  "input/management/thinning",
##  "input/plant",
##  "input/soil",
##  "input/soil/grid",
##  "input/soil/grid/100m",
##  "input/soil/grid/10km",
##  "input/soil/grid/1km",
##  "input/soil/site",
##  "input/storyline",
##  "input/weather",
##  "input/weather/grid",
##  "input/weather/grid/observed",
##  "input/weather/grid/projection",
##  "input/weather/site",
##  "observation",
##  "output",
##  "output/grid")

##     dirs <- grep(".*tmp.*", list.dirs(chosenPath,full.names = FALSE), invert = TRUE, value = TRUE)
##     for(i in links){
##         if(!is.element(i,dirs)){
##             return(FALSE)
##         }
##     }
##     retur(TRUE)

## }
