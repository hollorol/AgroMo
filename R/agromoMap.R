  agroMoMapUI <- function(id){
    
    paletteAlias <- data.frame(src=c(
      "www/img/palette_samples/Greens.png",
      "www/img/palette_samples/Greys.png",
      "www/img/palette_samples/Reds.png",
      "www/img/palette_samples/Yellow-Green-Blue.png",
      "www/img/palette_samples/Yellow-Orange-Brown.png",
      "www/img/palette_samples/Blues.png",
      "www/img/palette_samples/Red-Blue.png",
      "www/img/palette_samples/Red-Yellow-Blue.png",
      "www/img/palette_samples/Red-Yellow-Green.png",
      "www/img/palette_samples/Spectral.png",
      "www/img/palette_samples/Yellow-Green.png"
    ),
    alias=c(
      "Greens", "Greys", "Reds", "Yellow-Green-Blue", 
      "Yellow-Orange-Brown", "Blues",  "Red-Blue", "Red-Yellow-Blue", 
      "Red-Yellow-Green", "Spectral", "Yellow-Green"
    ))
    
    paletteAliasMask <- data.frame(src=c(
      "www/img/palette_samples/LightGre_mask.png",
      "www/img/palette_samples/DarkGre_mask.png",
      "www/img/palette_samples/Black_mask.png",
      "www/img/palette_samples/White_mask.png"
      ),
    alias=c(
      "Light Grey", "Dark Grey", "Black", "White"
    ))
    
    
    ns <- NS(id)
    tags$div(id = ns(id),

    tags$div(
    id = paste0(ns("invert"),"_container"),
    checkboxInput(ns("invert"), label = "inverted", value = FALSE)
    ),
    tags$img(id = ns("greysc"),src="www/img/palette_samples/LightGre_mask.png", draggable = FALSE),
    tags$img(id = ns("greensc"),src="www/img/palette_samples/Greens.png", draggable = FALSE),
    tags$div(
      id = paste0(ns("countrycont"),"_container"),
      checkboxInput(ns("countrycont"), label = "add country contour to the background", value = TRUE)
    ),
      tags$div(
      id = paste0(ns("datasource"),"_container"),
      selectInput(ns("datasource"),"data source:","")
    ),
    tags$div(
      id = paste0(ns("palette"),"_container"),
      selectInput(ns("palette"),"palette:",choices= paletteAlias[,2])),
    tags$div(
      id = paste0(ns("colnumb"),"_container"),
      selectInput(ns("colnumb"),"number of colours:",choices=c(2,4,8,16,32))
    ),
    tags$div(
      id = paste0(ns("min"),"_container"),
      textInput(ns("min"),"minimum value:",NA)
    ),
    tags$div(
      id = paste0(ns("minprec"),"_container"),
      selectInput(ns("minprec"),"precision of rounding:",choices=c(0,1,2,3,4,5))
    ),
        tags$div(
      id = paste0(ns("max"),"_container"),
      textInput(ns("max"),"maximum value:",NA)
    ),
    tags$div(
      id = paste0(ns("maxprec"),"_container"),
      selectInput(ns("maxprec"),"precision of rounding:",choices=c(0,1,2,3,4,5))
    ),
    tags$div(
      id = paste0(ns("maskcol"),"_container"),
      selectInput(ns("maskcol"),"mask colour:",choices=paletteAliasMask[,2])
    ),
    
    tags$div(
      id = paste0(ns("maptitle"),"_container"),
      textInput(ns("maptitle"), "map title:")
    ),
    tags$div(id="maskpreview","mask preview:"),
    tags$div(id="palettepreview","palette preview:"),
    
#itt a funkcionalitas kerdeses    
    tags$div(id = ns("Buttons"),
    actionButton(ns("create"),label = "CREATE MAP"),
    actionButton(ns("save"),label="SAVE to FILE")),
tags$script(HTML("
           let palette = {
\"src\":[\"www/img/palette_samples/Greens.png\",
         \"www/img/palette_samples/Greys.png\",
        \"www/img/palette_samples/Reds.png\",
        \"www/img/palette_samples/YlGnBu.png\",
        \"www/img/palette_samples/YlOrBr.png\",
        \"www/img/palette_samples/Blues.png\",
        \"www/img/palette_samples/RdBu.png\",
        \"www/img/palette_samples/RdYlBu.png\",
        \"www/img/palette_samples/RdYlGr.png\",
        \"www/img/palette_samples/Spectral.png\",
        \"www/img/palette_samples/YlGn.png\",
        \"www/img/palette_samples/Greens_reverse.png\",
         \"www/img/palette_samples/Greys_reverse.png\",
        \"www/img/palette_samples/Reds_reverse.png\",
        \"www/img/palette_samples/YlGnBu_reverse.png\",
        \"www/img/palette_samples/YlOrBr_reverse.png\",
        \"www/img/palette_samples/Blues_reverse.png\",
        \"www/img/palette_samples/RdBu_reverse.png\",
        \"www/img/palette_samples/RdYlBu_reverse.png\",
        \"www/img/palette_samples/RdYlGr_reverse.png\",
        \"www/img/palette_samples/Spectral_reverse.png\",
        \"www/img/palette_samples/YlGn_reverse.png\"
       ],
 \"colorscheme\" : [      \"Greens\", \"Greys\", \"Reds\", \"Yellow-Green-Blue\", 
      \"Yellow-Orange-Brown\", \"Blues\",  \"Red-Blue\", \"Red-Yellow-Blue\", 
      \"Red-Yellow-Green\", \"Spectral\", \"Yellow-Green\",
      \"Greens_inverse\", \"Greys_inverse\", \"Reds_inverse\", \"Yellow-Green-Blue_inverse\", 
      \"Yellow-Orange-Brown_inverse\", \"Blues_inverse\",  \"Red-Blue_inverse\", \"Red-Yellow-Blue_inverse\", 
                 \"Red-Yellow-Green_inverse\", \"Spectral_inverse\", \"Yellow-Green_inverse\"
       ]

           }
                 
        Shiny.addCustomMessageHandler ('palletteChanger', function(message) {
console.log(\"Itt vagyok!\");
        $(\"#mapdiv-greensc\").attr(\"src\", palette.src[palette.colorscheme.indexOf(message)])
        }

        )         

                 
                 
                 
                 
                 
                 ")),

tags$script(HTML("
           let paletteMask = {
                 \"src\":[\"www/img/palette_samples/LightGre_mask.png\",
                 \"www/img/palette_samples/DarkGre_mask.png\",
                 \"www/img/palette_samples/Black_mask.png\",
                 \"www/img/palette_samples/White_mask.png\"
                 ],
                 \"colorscheme\" : [      \"Light Grey\", \"Dark Grey\", \"Black\", \"White\"
                 ]
                 
  }
                 
                 Shiny.addCustomMessageHandler ('palletteChangerMask', function(message) {
                 console.log(\"Itt vagyok!\");
                 $(\"#mapdiv-greysc\").attr(\"src\", paletteMask.src[paletteMask.colorscheme.indexOf(message)])
                 }
                 
                 )         
                 
                 
                 
                 
                 
                 ")),
    plotOutput(ns("map_left"), width="358px",height="230px"),
    plotOutput(ns("map_right"),width="358px",height="230px")




    )
  }

    
agroMoMap <- function(input, output, session, baseDir){
    datas <- reactiveValues(numPlots = 1)
    myColors <- data.frame(
    codes=c("Greens","Greys","Reds","YlGnBu","YlOrBr","Blues","RdBu","RdYlBu","RdYlGn","Spectral","YlGn"), 
    alias=c(
      "Greens", "Greys", "Reds", "Yellow-Green-Blue", 
      "Yellow-Orange-Brown", "Blues",  "Red-Blue", "Red-Yellow-Blue", 
      "Red-Yellow-Green", "Spectral", "Yellow-Green"
           )
    )
    ns <- session$ns
    observe({
        dir.create(sprintf("%s/output/queries", baseDir()), showWarnings = FALSE)
        dir.create(sprintf("%s/output/map_data", baseDir()), showWarnings = FALSE)
        updateSelectInput(session, "datasource", 
            choices = list.files(sprintf("%s/output/queries/",baseDir())),
            selected = head(list.files(sprintf("%s/output/queries/",baseDir())),n=1)
        )
        # print(list.files(sprintf("%s/output/queries/",baseDir())))
    })
    
    observe({
      if(input$invert==TRUE){# funny
          paletteList <- paste0(input$palette,"_inverse")
      } else {
          paletteList <- input$palette
      }
        session$sendCustomMessage(type="palletteChanger",paletteList)
    })
  
    observe({
        session$sendCustomMessage(type="palletteChangerMask",input$maskcol)
    })
    
    observeEvent(input$create,{
      palette <- myColors$codes[myColors$alias==input$palette]
      # browser()
      if(file.exists(sprintf("%s/output/DB/HU-10km.db",baseDir())) ||
          file.exists("~/AgroMoDB/HU-10km.db")){

          if(file.exists(sprintf("%s/output/DB/HU-10km.db",baseDir()))){
            dbName <- sprintf("%s/output/DB/HU-10km.db",baseDir())
          } else {
            dbName <- "~/AgroMoDB/HU-10km.db"
          }
          
              if(length(input$datasource)>0) {
                  sqlName <- sprintf("%s/output/queries/%s",baseDir(),input$datasource)
                  sqlString <- readChar(sqlName,file.info(sqlName)["size"])
              }

              mapData <- basename(tools::file_path_sans_ext(sqlName))
              mapData <- sprintf("%s/output/map_data/%s.csv",baseDir(),mapData)

              if(file.exists(mapData)){
                 plot.new()
                 dev.control("enable") 
                  agroMap(dbName, myData=mapData, nticks=6,
                    reverseColorScale=input$invert,colorSet=input$palette, plotTitle=input$maptitle) 
                 leftPlot <- recordPlot()
                 dev.off()
              } else {
                 plot.new()
                 dev.control("enable") 
                  agroMap(dbName, query=sqlString, nticks=6,
                    reverseColorScale=input$invert,colorSet=input$palette, plotTitle=input$maptitle,
                    outFile=mapData) 
                 leftPlot <- recordPlot()
                 dev.off()
              }

          output$map_left <- renderPlot({replayPlot(leftPlot)})
          output$map_right <- renderPlot({replayPlot(leftPlot)})
      }

       }) 
    
}
  
