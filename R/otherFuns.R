list.filesND <- function(path){
  setdiff(list.files(path),list.dirs(full.names = FALSE))
}

# FROM https://stackoverflow.com/questions/46290869/how-to-break-line-in-long-shownotification-for-rshiny
# AUTHOR r2evans
showNotification2 <- function (ui, action = NULL, duration = 5, closeButton = TRUE, 
    id = NULL, type = c("default", "message", "warning", "error"), 
    session = shiny:::getDefaultReactiveDomain()) {
    if (is.null(id)) 
        id <- shiny:::createUniqueId(8)
    res <- shiny:::processDeps(HTML(ui), session)
    actionRes <- shiny:::processDeps(action, session)
    session$sendNotification("show", list(html = res$html, action = actionRes$html, 
        deps = c(res$deps, actionRes$deps), duration = if (!is.null(duration)) duration * 
            1000, closeButton = closeButton, id = id, type = match.arg(type)))
    id
}

changeLineSiteGrid <- function(inFile, linum, dbName, baseDir, withDir = TRUE, forManagement=FALSE, manType){
    if(forManagement && file.exists(file.path(baseDir,inFile[linum])) && (inFile[linum] != "none")){
        newName <- gsub(sprintf("(.*)/grid/([a-zA-Z0-9_\\-]+)/%s/(.*)",manType),
                        sprintf("\\1/site/%s/\\2_%s_\\3", manType, dbName), inFile[linum])
        file.copy(from=file.path(baseDir,inFile[linum]),to=file.path(baseDir,newName))
        inFile[linum] <- newName
        return(inFile)
    }

    if(withDir){
        newName <- gsub("/grid/([a-zA-Z0-9_\\-]+)/",paste0("/site/\\1_",dbName,"_"), inFile[linum])
        file.copy(from=file.path(baseDir,inFile[linum]),to=file.path(baseDir,newName))
        inFile[linum] <- newName
        return(inFile)
    }
        newName <- gsub("/grid/",paste0("/site/",dbName,"_"), inFile[linum])
        file.copy(from=file.path(baseDir,inFile[linum]),to=file.path(baseDir,newName))
        inFile[linum] <- newName
        return(inFile)
}

managementSiteGrid <- function(inFile,linum, dbName, baseDir){
        newName <- gsub("/grid/([a-zA-Z0-9_\\-]+)/",paste0("/site/\\1_",dbName,"_"), inFile[linum])
        manFile <- readLines(file.path(baseDir,inFile[linum]))
        manFile <- changeLineSiteGrid(manFile, 5, dbName, baseDir, forManagement=TRUE, manType="planting")
        manFile <- changeLineSiteGrid(manFile, 9, dbName, baseDir, forManagement=TRUE, manType="thinning")
        manFile <- changeLineSiteGrid(manFile, 13, dbName, baseDir, forManagement=TRUE, manType="grazing")
        manFile <- changeLineSiteGrid(manFile, 21, dbName, baseDir, forManagement=TRUE, manType="harvest")
        manFile <- changeLineSiteGrid(manFile, 25, dbName, baseDir, forManagement=TRUE, manType="ploughing")
        manFile <- changeLineSiteGrid(manFile, 29, dbName, baseDir, forManagement=TRUE,
                                                                                   manType="fertilization")
        manFile <- changeLineSiteGrid(manFile, 33, dbName, baseDir, forManagement=TRUE, manType="irrigating")
        writeLines(manFile,file.path(baseDir,newName))
        inFile[linum] <- newName
        return(inFile)
}

createSiteFromGrid <- function(storyName, siteName, baseDir){
    iniFile <-  file.path(baseDir, "input/initialization/grid", storyName, paste0(siteName, "_1.ini"))
    iniContent <- readLines(iniFile)
    iniContent <- changeLineSiteGrid(iniContent, 4, storyName, baseDir) #weather
    iniContent <- changeLineSiteGrid(iniContent, 11, storyName, baseDir,withDir=FALSE) #endpoint
    iniContent <- changeLineSiteGrid(iniContent, 12, storyName, baseDir) #endoint
    iniContent <- changeLineSiteGrid(iniContent, 39, storyName, baseDir) #soi
    iniContent <- managementSiteGrid(iniContent, 45, storyName, baseDir) #mgm
    iniContent[106] <- "output/site"
    writeLines(iniContent,file.path(baseDir,"input/initialization/site",paste0(storyName,'_',siteName,".ini")))
}
