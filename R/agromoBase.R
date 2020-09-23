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
                                style = ("background: url('www/img/site_button.png')")),
                             actionButton("grid","", class = "mainMenu", title="Simulations using gridded climate and soil data",
                                style = ("background: url('www/img/grid_button.png')")),
                             actionButton("show","", class = "mainMenu", title="Create plots using simulation results",
                                style = ("background: url('www/img/show_button.png')")),
                             actionButton("map","",  class = "mainMenu", title="Create maps using gridded simulation results",
                                style = ("background: url('www/img/map_button.png')")))
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
                        shiny::tags$img(id = ns("huFlag"),src="www/img/HUflag_s.png", title = "Magyar változat", langID = "HUN", class = "languageButton"),
                        shiny::tags$img(id = ns("ukFlag"),src="www/img/UKflag_s.png", title = "English version", langID = "ENG", class = "languageButton"),
                        shiny::tags$img(id = ns("gerFlag"),src="www/img/GERflag_s.png", title = "Deutsch Version", langID = "GER", class = "languageButton"),
                        shiny::tags$img(id = ns("chiFlag"),src="www/img/CHIflag.png", langID = "CHN", class = "languageButton"),
                        shiny::tags$img(id = ns("rusFlag"),src="www/img/RUSflag.png", langID = "RUS", class = "languageButton"),
                        shiny::tags$img(id = ns("fraFlag"),src="www/img/FRAflag.png", title = "Version française", langID = "FRA", class = "languageButton"),
                        shiny::tags$img(id = ns("espFlag"),src="www/img/ESPflag.png", title = "Versión en español", langID = "ESP", class = "languageButton"),
                        shiny::tags$img(id = ns("porFlag"),src="www/img/PORflag.png", title = "Versão portugal", langID = "POR", class = "languageButton"),
                        shiny::tags$img(id = ns("jpFlag"),src="www/img/JPflag_s.png", langID = "JPN", class = "languageButton"),
                        shiny::tags$script(HTML("

                                            function dictChange(dictionary, lang){
                                                dictionary.map(function(elem){
                                              let elemID = document.querySelector(elem.ID);
                                                  let innerLang = `TEXT_${lang}`; 
                                                  //console.log(innerLang);
                                              if(elem[innerLang] === undefined){
                                              } else {
                                                elemID.innerHTML = elem[innerLang];
                                              }})
                                            }

                                            $(\".languageButton\").on(\"click\",function(){
                                              // console.log(this.getAttribute(\"langID\"));
                                              if(this.getAttribute(\"langID\")===\"HUN\"){
                                                  Shiny.setInputValue(\"languageState\",\"hu\");
                                              } else {
                                                  Shiny.setInputValue(\"languageState\",\"en\");
                                              }
                                              dictChange(dictionary,this.getAttribute(\"langID\"));
                                            })


                                             ")),
                        # shiny::tags$img(id = ns("exit_z"),src="www/img/exit.png", title = "EXIT"),
                 #actionButton("exit", "",onclick="function(){window.close()}", title="EXIT",
                              #style=("background: url('www/img/exit.png'); background-size: 260px 70px;")),
                 actionButton("exit", "",onclick="function(){window.close()}", title="Exit",
                              style=("background: url('www/img/exit_icon6_kicsi.png');")),
#                        actionButton("exit", src="www/img/exit.svg",
#                                     onclick="function(){window.close()}", title="EXIT"),
                        actionButton("parana","PARAMETER ANALYSIS",
                          class = "mainSideBar",title="Tool for calibration, parameter sweep and sensitivity analysis"),
                        actionButton("sitegen","INPUT IMPORT",
                          class = "mainSideBar", title="Download site specific weather and soil data from ERA5 and ISRIC databases"),
                        actionButton("calibration","DATABASE MANAGER",
                          class = "mainSideBar"),
                        actionButton("storEditor","INPUT CREATOR",
                              class = "mainSideBar"),
                        actionButton("fileMan","",
                              class = "mainSideBar"))
           )
}
