tables_get <- function(baseDir){
         dbDir <- file.path(baseDir,"output")
         dir.create(dbDir, showWarnings=FALSE)
         sqlDB <- DBI::dbConnect(RSQLite::SQLite(),file.path(dbDir,"grid.db"))
         result <- grep("_error$",dbListTables(sqlDB),invert=TRUE,value=TRUE)
         dbDisconnect(sqlDB)
         result
}

climate_get <- function(baseDir){
         dbDir <- file.path(baseDir,"database")
         dir.create(dbDir, showWarnings=FALSE)
         sqlDB <- DBI::dbConnect(RSQLite::SQLite(),file.path(dbDir,"climate.db"))
         result <- unlist(dbGetQuery(sqlDB,"SELECT source_name FROM meta_data"))
         dbDisconnect(sqlDB)
         result
}

months_get <- function(baseDir){
         1:12
}
