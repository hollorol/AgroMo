#' stringSanitizer
#'
#' This little function converts a given string to more sane format (no non-ASCII, no spacial, no whitespace)
#' @param string string to sanitize 
#' @keywords internal  

stringSanitizer <- function(string){

    unwantedChars <- "\'\"+^\\!°/%=()@&#\\[\\]{};×<>?,|"
    string <- iconv(string, "","ASCII//TRANSLIT")
    string <- gsub(sprintf("[%s]",unwantedChars),"",string,perl=TRUE)
    gsub("[\\s-:]","_",string,perl=TRUE)

}   
