#' agromoBaseUI
#'
#' The UI function for the BASE component of the GUI
#' @param id of the GUI element
#' @keywords internal 
#' @importFrom shiny tagList actionButton textOutput
agromoBaseUI <- function(id){
    ns <- NS(id)
   tagList(shiny::tags$div(id = "base", shiny::tags$div(id="components","COMPONENTS"),
                    shiny::tags$div(actionButton("site","", class = "mainMenu",
                                style = ("background: url('www/img/site.svg')")),
                             actionButton("grid","", class = "mainMenu",
                                style = ("background: url('www/img/grid.svg')")),
                             actionButton("show","", class = "mainMenu",
                                style = ("background: url('www/img/show.svg')")),
                             actionButton("map","",  class = "mainMenu",
                                style = ("background: url('www/img/map.svg')")))
               ),

                 shiny::tags$div(
                 id = ns("tools"), #base-tools
                 class= "mainSideBar",
                        shiny::tags$div(id="mainSideBarDirectory", "MAIN DIRECTORY"),
                        actionButton("choose","CHOOSE", class = "mainSideBar"),
                        textOutput("mdd"),##mdd is not a good name
                        shiny::tags$hr(id="lineComp",""),
                        shiny::tags$hr(id="lineTool",""),
                        shiny::tags$hr(id="lineMain",""),
                        shiny::tags$hr(id="lineLang",""),
                        shiny::tags$div(id="mainSideBarTools", "TOOLS"),
                        shiny::tags$div(id="mainSideBarLang", "LANGUAGES"),
                        shiny::tags$img(id = ns("huFlag"),src="www/img/HUflag_s.png", title = "HU-HU-HU-HU"),
                        shiny::tags$img(id = ns("ukFlag"),src="www/img/UKflag_s.png", title = "Switch back the default language"),
                        shiny::tags$img(id = ns("gerFlag"),src="www/img/GERflag_s.png", title = "Sprechen Sie Deutsch?"),
                        shiny::tags$img(id = ns("exit_z"),src="www/img/exit.png", title = "EXIT"),
                 actionButton("exit", "",onclick="function(){window.close()}", title="EXIT"),
#                        actionButton("exit", src="www/img/exit.svg",
#                                     onclick="function(){window.close()}", title="EXIT"),
                        actionButton("parSweep","PARAMETER SWEEP",
                          class = "mainSideBar",title="Do you feel the power?"),
                        actionButton("sensitivity","SENSITIVITY ANALYSIS",
                          class = "mainSideBar"),
                        actionButton("calibration","CALIBRATION",
                          class = "mainSideBar"),
                        actionButton("storEditor","STORYLINE EDITOR",
                              class = "mainSideBar"),
                        actionButton("fileMan","INPUT FILE MANAGER",
                              class = "mainSideBar"))
           )
}
