#' BBGCUI
#'
#' BBGCDB STARTER
#' @param id id of csao

BBGCUI <- function (id) {
# browser()
      tags$div(id=id,
           tags$script(HTML(
                                  sprintf("$('#helpme').on('click',function(){$('#%s').toggleClass('shinyjs-hide')})",id)
                                  )),
            tags$iframe(src="www/readme.html",style="width: 100vw;height:100vh;background-color:wheat; position: fixed; top:0;left:0;"),
           tags$button(id="helpback",'back', style="position: fixed; bottom: 0;left:0; width: 10vw; z-index: 1;"),
           tags$script(HTML(sprintf("$('#helpback').on('click', function(){$('#%s').toggleClass('shinyjs-hide')})",id))),

           style="position: fixed; z-index: 1; width: 100vw; height: 100vh;"
      )
}
