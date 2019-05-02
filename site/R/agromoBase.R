agromoBaseUI <- function(id){
    ns <- NS(id)
    tags$div(id = "base", fluidRow(
                            class = ns("baseInputs"),
        column(10,
               fluidRow(actionButton("site","", class = "mainMenu", style = ("background: url('img/site.svg')")),
                        actionButton("grid","", class = "mainMenu", style = ("background: url('img/grid.svg')")),
                        actionButton("show","", class = "mainMenu", style = ("background: url('img/show.svg')")),
                        actionButton("map","",  class = "mainMenu", style = ("background: url('img/map.svg')")))
               
               ),
        column(2,
               fluidRow(class= "mainSideBar",
                        tags$div(id="mainSideBarTools", "TOOLS"),
                        actionButton("site","PARAMETER SWEEP", class = "mainSideBar"),
                        actionButton("grid","SENSITIVITY ANALYSIS", class = "mainSideBar"),
                        actionButton("show","CALIBRATION", class = "mainSideBar"),
                        actionButton("map","STORYLINE EDITOR",  class = "mainSideBar"))
               )
        
    ))
}
