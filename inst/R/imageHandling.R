## renderBanner <- function(output){
##     tags$div(id = "banners",
##       tagList(
##         div(id = "Base-banner-div", class = "banner",
##             tags$img("",src = "img/banner/Base.svg")),
##         hidden(div(id = "Show-banner-div", class = "banner",
##                    tags$img("",src = "img/banner/Show.svg"))),
##         hidden(div(id = "Site-banner-div", class = "banner",
##                    tags$img("",src="img/banner/Site.svg")))
##       )
##     )

    ## for(img in imageToRender){
    ##     local({
    ##         myImg <- img
    ##         imgName <- paste0("img/banner/",grep(paste0("^",gsub("\\-.*","",myImg),"\\.(svg|png)$"),imageNames,value = TRUE))
    ##     output[[img]] <- renderImage({
    ##         list(src = imgName)
    ##     },deleteFile = FALSE)})

    ## }
## }
