#' cmn2prv
#' 
#' Given the ISTAT code for an Italian District, build the ISTAT code for the correspondent Province 
#'
#' @param x The District code
#' @param as_char Indicate if the Province code should be returned as a 3-char code (default) or as an integer
#'
#' @return A string or an integer depending on the value of the parameter `as_char`
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
cmn2prv <- function(x, as_char = TRUE){
    if(nchar(x) > 6 | nchar(x) < 4) stop('The code should be 4, 5 or 6 characters long.')
    x <- as.integer(substr(x, 1, nchar(x) - 3))
    if(as_char) return(str_add_char(x, 3))
    x
}
