#' agromoBaseUI
#'
#' The UI function for the BASE component of the GUI
#' @param id of the GUI element
#' @keywords internal 
#' @importFrom shiny tagList actionButton textOutput
#' @importFrom shinyFiles shinyDirButton
agromoBaseUI <- function(id){
    ns <- NS(id)
   tagList(shiny::tags$div(id = "base", shiny::tags$div(id="components","COMPONENTS"),
                    shiny::tags$div(actionButton("site","", class = "mainMenu", title="Site specific simulations",
                                style = ("background: url('www/img/site.svg')")),
                             actionButton("grid","", class = "mainMenu", title="Simulations using gridded climate and soil data",
                                style = ("background: url('www/img/grid.svg')")),
                             actionButton("show","", class = "mainMenu", title="Create plots using simulation results",
                                style = ("background: url('www/img/show.svg')")),
                             actionButton("map","",  class = "mainMenu", title="Create maps using gridded simulation results",
                                style = ("background: url('www/img/map.svg')")))
               ),

                 shiny::tags$div(
                 id = ns("tools"), #base-tools
                 class= "mainSideBar",
                        shiny::tags$div(id="mainSideBarDirectory", "MAIN DIRECTORY"),
                        shinyDirButton("choose", "CHOOSE","Please choose an AgroMo base directory!", class="mainSideBar"),
                        # actionButton("choose","CHOOSE", class = "mainSideBar"),
                        textOutput("mdd"),##mdd is not a good name
                        shiny::tags$hr(id="lineComp",""),
                        shiny::tags$hr(id="lineTool",""),
                        shiny::tags$hr(id="lineMain",""),
                        shiny::tags$hr(id="lineLang",""),
                        shiny::tags$div(id="mainSideBarTools", "TOOLS"),
                        shiny::tags$div(id="mainSideBarLang", "LANGUAGES"),
                        shiny::tags$img(id = ns("huFlag"),src="www/img/HUflag_s.png", title = "Magyar változat"),
                        shiny::tags$img(id = ns("ukFlag"),src="www/img/UKflag_s.png", title = "English version"),
                        shiny::tags$img(id = ns("gerFlag"),src="www/img/GERflag_s.png", title = "Deutsch Version"),
                        shiny::tags$img(id = ns("jpFlag"),src="www/img/JPflag_s.png"),
                        # shiny::tags$img(id = ns("exit_z"),src="www/img/exit.png", title = "EXIT"),
                 #actionButton("exit", "",onclick="function(){window.close()}", title="EXIT",
                              #style=("background: url('www/img/exit.png'); background-size: 260px 70px;")),
                 actionButton("exit", "",onclick="function(){window.close()}", title="Exit",
                              style=("background: url('www/img/exit_icon6_kicsi.png');")),
#                        actionButton("exit", src="www/img/exit.svg",
#                                     onclick="function(){window.close()}", title="EXIT"),
                        actionButton("parana","PARAMETER ANALYSIS",
                          class = "mainSideBar",title="Tool for calibration, parameter sweep and sensitivity analysis"),
                        actionButton("sitegen","SITE CREATOR",
                          class = "mainSideBar", title="Download site specific weather and soil data from ERA5 and ISRIC databases"),
                        actionButton("calibration","BBGCDB",
                          class = "mainSideBar"),
                        actionButton("storEditor","INPUT CREATOR",
                              class = "mainSideBar"),
                        actionButton("fileMan","",
                              class = "mainSideBar"))
           )
}
