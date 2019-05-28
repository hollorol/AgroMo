agromoBaseUI <- function(id){
    ns <- NS(id)
   tagList(tags$div(id = "base", tags$div(id="components","COMPONENTS"),
               tags$div(actionButton("site","", class = "mainMenu", style = ("background: url('img/site.svg')")),
                        actionButton("grid","", class = "mainMenu", style = ("background: url('img/grid.svg')")),
                        actionButton("show","", class = "mainMenu", style = ("background: url('img/show.svg')")),
                        actionButton("map","",  class = "mainMenu", style = ("background: url('img/map.svg')")))
               
               ),
           
           
                 tags$div(
                 id = ns("tools"), #base-tools
                 class= "mainSideBar",
                        tags$div(id="mainSideBarDirectory", "MAIN DIRECTORY"),
                        actionButton("choose","CHOOSE", class = "mainSideBar"),
                        tags$div(id="mainSideBarTools", "TOOLS"),
                        actionButton("parSweep","PARAMETER SWEEP", class = "mainSideBar"),
                        actionButton("sensitivity","SENSITIVITY ANALYSIS", class = "mainSideBar"),
                        actionButton("calibration","CALIBRATION", class = "mainSideBar"),
                        actionButton("storEditor","STORYLINE EDITOR",  class = "mainSideBar"))
           )
   
   
}
