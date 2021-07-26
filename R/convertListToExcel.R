#' listToExcel
#' 
#' This simple function exports a list to an excel document. The list elements will be worksheets in Excel
#' @param list_object The list to export
#' @importFrom openxlsx writeData createWorkbook addWorksheet saveWorkbook
#' @export

listToExcel <- function(list_object, fname) {
    w <- createWorkbook()

    invisible({
        lapply(names(list_object), function(n) {
           addWorksheet(w, n)
           writeData(w, n, list_object[[n]])
        })
    })

    saveWorkbook(w, paste0(fname, ".xlsx"), overwrite = TRUE)
}
