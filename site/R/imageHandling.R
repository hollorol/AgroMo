renderBanner <- function(output){
    imageNames <- list.files("img/banner/",pattern = "*.(svg|png)")
    imageToRender <- gsub("\\..*","-banner",imageNames)
    output$banners<- renderUI({
        bannerList <- lapply(imageNames,function(bannerName){
            imageName <- gsub("\\..*","-banner",bannerName)
            bannerDiv <- paste0(imageName,"-div")
            if(imageName == "Base-banner"){
                div(id = bannerDiv, class = "banner",imageOutput(imageName,height = "100%"))                
            } else {
                hidden(div(id = bannerDiv, class = "banner",imageOutput(imageName,height = "100%")))
            }

            
        })
    })

    for(img in imageToRender){
        local({
            myImg <- img
            imgName <- paste0("img/banner/",grep(paste0("^",gsub("\\-.*","",myImg),"\\.(svg|png)$"),imageNames,value = TRUE))
        output[[img]] <- renderImage({
            list(src = imgName)
        },deleteFile = FALSE)})

    }
}
