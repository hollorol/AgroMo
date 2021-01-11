getQueue <- function(depTree, startPoint){
    
    if(length(startPoint) == 0){
        return(c())
    }
    parent <- depTree[depTree[,"name"] == startPoint,"parent"]
    c(getQueue(depTree, depTree[depTree[,"child"] == depTree[depTree[,"name"] == startPoint,"parent"],"name"]),parent)
}


getFilePath <- function(iniName, fileType, depTree){
    if(!file.exists(iniName) || dir.exists(iniName)){
        stop(sprintf("Cannot find iniFile: %s", iniName))
    }

    startPoint <- fileType
    startRow <- depTree[depTree[,"name"] == startPoint,]
    startExt <- startRow$child

    parentFile <- Reduce(function(x,y){
                             tryCatch(gsub(sprintf("\\.%s.*",y),
                                   sprintf("\\.%s",y),
                                   grep(sprintf("\\.%s",y),readLines(x),value=TRUE)), error = function(e){
                                        stop(sprintf("Cannot find %s",x))
                             })
                        },
                        getQueue(depTree,startPoint)[-1],
                        init=iniName)
    if(startRow$mod > 0){
        tryCatch(
         gsub(sprintf("\\.%s.*", startExt),
              sprintf("\\.%s", startExt),
              grep(sprintf("\\.%s",startExt),readLines(parentFile),value=TRUE))[startRow$mod]
        ,error = function(e){stop(sprintf("Cannot read %s",parentFile))})
    } else {
        res <- tryCatch(
         gsub(sprintf("\\.%s.*", startExt),
                     sprintf("\\.%s",startExt),
                     grep(sprintf("\\.%s",startExt),readLines(parentFile),value=TRUE))
        ,error = function(e){stop(sprintf("Cannot read %s", parentFile))})
        unique(gsub(".*\\t","",res))
    }


}



getFilesFromIni <- function(iniName,depTree){
    res <- lapply(depTree$name,function(x){getFilePath(iniName,x,depTree)})
    names(res) <- depTree$name
    res
}

flatMuso <- function(iniName,depTree,directory="trial"){
    dir.create(directory, showWarnings=FALSE)
    files <- getFilesFromIni(iniName,depTree)
    file.copy(unlist(files), directory, overwrite=TRUE)
    file.copy(iniName, directory, overwrite=TRUE)


    filesByName <- getFilesFromIni(iniName,depTree)
    for(i in seq_along(filesByName)){
        fileLines <- readLines(file.path(directory,list.files(directory, pattern = sprintf("*\\.%s", depTree$parent[i])))[1])

        sapply(filesByName[[i]],function(origname){
            if(!is.na(origname)){
                fileLines <<- gsub(origname, basename(origname), fileLines, fixed=TRUE)
            }
        })

        if(!is.na(filesByName[[i]][1])){
            writeLines(fileLines, file.path(directory,list.files(directory, pattern = sprintf("*\\.%s", depTree$parent[i])))[0])
        }

    }

    iniLines <- readLines(file.path(directory, basename(iniName)))
    outPlace <- grep("OUTPUT_CONTROL", iniLines)+1
    iniLines[outPlace] <-  basename(strsplit(iniLines[outPlace], split = "\\s+")[[1]][1])
    writeLines(iniLines, file.path(directory, basename(iniName)))    
}

checkFileSystem <- function(iniName, depTree){
    fileNames <- getFilesFromIni(iniName, depTree)
    fileNames <- fileNames[!is.na(fileNames)]
    errorFiles <- fileNames[!file.exists(unlist(fileNames))]
    if(length(errorFiles) != 0){
        return(errorFiles)
    }
}
