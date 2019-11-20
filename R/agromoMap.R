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
      "www/img/palette_samples/Yellow-Green.png",
      "www/img/palette_samples/Greens_reverse.png",
      "www/img/palette_samples/Greys_reverse.png",
      "www/img/palette_samples/Reds_reverse.png",
      "www/img/palette_samples/Yellow-Green-Blue_reverse.png",
      "www/img/palette_samples/Yellow-Orange-Brown_reverse.png",
      "www/img/palette_samples/Blues_reverse.png",
      "www/img/palette_samples/Red-Blue_reverse.png",
      "www/img/palette_samples/Red-Yellow-Blue_reverse.png",
      "www/img/palette_samples/Red-Yellow-Green_reverse.png",
      "www/img/palette_samples/Spectral_reverse.png",
      "www/img/palette_samples/Yellow-Green_reverse.png"
    ),
    alias=c(
      "Greens", "Greys", "Reds", "Yellow-Green-Blue", 
      "Yellow-Orange-Brown", "Blues",  "Red-Blue", "Red-Yellow-Blue", 
      "Red-Yellow-Green", "Spectral", "Yellow-Green",
      "Greens_inverse", "Greys_inverse", "Reds_inverse", "Yellow-Green-Blue_inverse", 
      "Yellow-Orange-Brown_inverse", "Blues_inverse",  "Red-Blue_inverse", "Red-Yellow-Blue_inverse", 
      "Red-Yellow-Green_inverse", "Spectral_inverse", "Yellow-Green_inverse"
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
      selectInput(ns("datasource"),"data source:",NA)
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
                 
                 
                 
                 
                 
                 "))



    )
  }
  
  agroMoMap <- function(input, output, session){
    
    observe({
    session$sendCustomMessage(type="palletteChanger",input$palette)
    })
    observe({
      session$sendCustomMessage(type="palletteChangerMask",input$maskcol)
    })
    
      }
  
