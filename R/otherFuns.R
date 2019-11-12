list.filesND <- function(path){
  setdiff(list.files(path),list.dirs(full.names = FALSE))
}
## getMetDataMeta <- function(mtcFile, numHeaders){

##     metData <- read.table(mtcFile,skip = numHeaders)
##    return(c(nrow(metData)/365,metData[1,1]))     
## }


## getMetDataMeta("/home/hollorol/agromo/site/input/weather/site/Bóly_(30).wth",4)
## setwd("/home/hollorol/agromo/site/input/weather/site/")
## list.files("/home/hollorol/agromo/site/input/weather/site/")
## lapply(list.files("/home/hollorol/agromo/site/input/weather/site/"),function(x){getMetDataMeta(x,4)})

## sliceMetDataMeta <- function(mtcFile, startYear, numYears, numHeaders){

##     header <- readLines(mtcFile,numHeaders)
##     fread("/home/hollorol/agromo/site/input/weather/site/Bóly_(30).wth",skip = 4)[V1 %between% c(2000,2010)]
    
## }


## siteIndexes <- c(30, 757, 825, 149, 643, 166, 896, 628, 911, 272, 408)

## siteIndexes <- c(30, 149, 166, 272, 336, 408, 628, 643, 757, 825, 896, 911)


## gridTable<- fread("/home/hollorol/agromo/site/R/AgroMo_DB_GRID_TABLE3.csv")[new_id %in% siteIndexes]

## (insert (shell-command-to-string "ls ../input/soil/site"))
## base <- c("Bóly_(30).soi", "Debrecen_(757).soi", "Győr_(825).soi", "Kaposvár_(149).soi", "Karcag_(643).soi", "Kiskunhalas_(166).soi", "Mezőkövesd_(896).soi", "Monor_(628).soi", "Nyírbátor_(911).soi", "Orosháza_(272).soi", "Sárbogárd_(408).soi", "Zalaegerszeg_(336).soi")
## soil <- base[order(as.numeric(gsub("\\D","",base)))]
## base <- c("Bóly_(30).wth", "Debrecen_(757).wth", "Győr_(825).wth", "Kaposvár_(149).wth", "Karcag_(643).wth", "Kiskunhalas_(166).wth", "Mezőkövesd_(896).wth", "Monor_(628).wth", "Nyírbátor_(911).wth", "Orosháza_(272).wth","Sárbogárd_(408).wth","Zalaegerszeg_(336).wth")
## weather <- base[order(as.numeric(gsub("\\D","",base)))]
## weather
## soil
## management <- sample(c("maize_mono.mgm","wheat_mono.mgm"),12,replace = TRUE)
## (insert (shell-command-to-string "ls ../input/weather/site"))

## length(management)
## length(gridTable)
## fread("AgroMo_DB_GRID_TABLE3.csv")

## gridTable <- cbind(gridTable,
##                    weather,
##                    soil,
##                    management,
##                    restart = sample(list.files("spinup"),12,replace = TRUE),
##                    CO2 = sample(list.files("auxiliary",pattern = "^CO"),12,replace = TRUE),
##                    Ndep = rep("Ndep_allRCP.rcp",12),
##                    plant = sample(list.files("plant"),12,replace = TRUE))

## gridTable <- cbind(gridTable,sample(list.files("spinup"),12,replace = TRUE))
## gridTable <- cbind(gridTable,sample(list.files("auxiliary",pattern = "^CO"),12,replace = TRUE))
## gridTable <- cbind(gridTable,rep("Ndep_allRCP.rcp",12))
## iniTemp <- readLines("../templates/template_normal.ini")
## apply(gridTable,1,function(x){
##     iniTemp[[4]] <- paste0("input/weather/site/",x[10])
##     iniTemp[[11]] <- paste0("input/spinup/",x[13])
##     iniTemp[[12]] <- paste0("input/spinup/",x[13])
##     iniTemp[[16]] <- iniTemp[[15]] <- getMetDataMeta(paste0("weather/site/",x[10]),4)[1]
##     iniTemp[[17]] <- getMetDataMeta(paste0("weather/site/",x[10]),4)[2]
##     iniTemp[[24]] <- paste0("input/auxiliary/",x[14])
##     iniTemp[[29]] <- paste0("input/auxiliary/",x[15])
##     iniTemp[[29]] <- paste0("input/auxiliary/",x[15])
##     iniTemp[[32]] <- unlist(x[5])
##     iniTemp[[33]]<- unlist(x[3])
##     iniTemp[[35]] <- unlist(x[8])
##     iniTemp[[40]] <- paste0("input/soil/site/",x[11])
##     iniTemp[[43]] <- paste0("input/plant/",x[16])
##     iniTemp[[46]] <- paste0("input/management/site/",x[12])
##     writeLines(iniTemp,paste0("initialization/",gsub("\\.wth",".ini",x[10])))    
## })
## cd iniTemp[[4]] <- paste0("input/weather/site/",gridTable[row,10])
## iniTemp[[11]] <- paste0("input/spinup/",gridTable[row,13])
## iniTemp[[12]] <- paste0("input/spinup/",gridTable[row,13])
## iniTemp[[16]] <- iniTemp[[15]] <- getMetDataMeta(paste0("weather/site/",gridTable[row,10]),4)[1]
## iniTemp[[17]] <- getMetDataMeta(paste0("weather/site/",gridTable[row,10]),4)[2]
## iniTemp[[24]] <- paste0("input/auxiliary/",gridTable[row,14])
## iniTemp[[29]] <- paste0("input/auxiliary/",gridTable[row,15])
## iniTemp[[29]] <- paste0("input/auxiliary/",gridTable[row,15])
## iniTemp[[32]] <- unlist(gridTable[row,5])
## iniTemp[[33]]<- unlist(gridTable[row,3])
## iniTemp[[35]] <- unlist(gridTable[row,8])
## iniTemp[[40]] <- paste0("input/soil/site/",gridTable[row,11])
## iniTemp[[43]] <- paste0("input/plant/",gridTable[row,16])
## iniTemp[[46]] <- paste0("input/management/site/",gridTable[row,12])
## writeLines(iniTemp,paste0("initialization/",gsub("\\.wth",".ini",gridTable[row,10])))
