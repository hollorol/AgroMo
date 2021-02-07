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
