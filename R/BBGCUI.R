#' BBGCUI
#'
#' BBGCDB STARTER
#' @param id id of csao

BBGCUI <- function (id) {
# browser()
     tags$div(id=id,
              tags$script(HTML(
                                    sprintf("$('#bbgc').on('click',function(){$('#%s').toggleClass('shinyjs-hide')})",id)
                                    )),
              tags$iframe(src="https://bbgcdb.agrar.mta.hu/",style="width:100%;height:760px"))
}
