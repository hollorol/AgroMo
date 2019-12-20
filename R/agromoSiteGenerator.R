#' agroMoSiteGeneratorUI
#'
#' Bla
#' @param id id
#' @importFrom shiny NS tags checkboxInput selectInput textInput actionButton plotOutput updateSelectInput observe imageOutput

  agroMoSiteGeneratorUI <- function(id){
  
    
    
    ns <- NS(id)
    tags$div(id = ns(id),

    tags$div(
    id = paste0(ns("soil"),"_container"),
    checkboxInput(ns("soil"), label = "Retrieve soil data from ISRIC database", value = TRUE)
    ),
    tags$div(
      id = paste0(ns("weather"),"_container"),
      checkboxInput(ns("weather"), label = "Retrieve weather data from ERA5 database", value = TRUE)
    ),
    tags$div(
      id = paste0(ns("ini"),"_container"),
      checkboxInput(ns("ini"), label = "Create ini file using the selected file as template:", value = FALSE)
    ),
      tags$div(
      id = paste0(ns("initemplate"),"_container"),
      selectInput(ns("initemplate"),"","")
    ),
    tags$div(
      id = paste0(ns("sitename"),"_container"), 
      textInput(ns("sitename"),"SITE NAME:","")
    ),
        tags$div(
      id = paste0(ns("erareg"),"_container"),
      textInput(ns("erareg"),"ERA5 REGISTRATION CODE:","")
    ),
    tags$div(
      id = paste0(ns("settlement"),"_container"),
      selectInput(ns("settlement"),"name of settlement:",choices=c(""))
    ),
    
    tags$div(
      id = paste0(ns("lat"),"_container"),
      textInput(ns("lat"), "latitude (deg):")
    ),
    tags$div(
      id = paste0(ns("lon"),"_container"),
      textInput(ns("lon"), "longitude (deg):")
    ),
    tags$div(
      id = paste0(ns("start"),"_container"),
      selectInput(ns("start"), "start year:",choices=c("1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))
    ),
    tags$div(
      id = paste0(ns("end"),"_container"),
      selectInput(ns("end"), "end year:",choices=c("1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))
    ),
    tags$div(id="geocord","GEOGRAPHICAL COORDINATES:"),
    tags$div(id="timeperiod","TIME PERIOD FOR WEATHER DATA:"),
#itt a funkcionalitas kerdeses    
    tags$div(id = ns("Buttons"),
    actionButton(ns("retdatcreate"),label = "RETRIEVE DATA AND CREATE FILE(S)"))




    )
  }

#' agroMoMap 
#' 
#' asdfasfd
#' @param input input
#' @importFrom shiny reactiveValues observe updateSelectInput observe renderPlot renderImage 
#' @importFrom DBI dbConnect

    
agroMoSiteGenerator <- function(input, output, session){
    
    
    ns <- session$ns
    
    
}
