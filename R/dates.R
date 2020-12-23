#' Create a list of ordered months, possibly by year(s)
#'
#' @param ys the start year, or the only year if \code{ye} is missing. If missing, the function returns only the months.
#' @param ye the final year
#'
#' @return a data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
mnths <- function(ys = NA, ye = NA){
    yL <- month.name
    if(is.na(ys)){
        yS <- month.abb
    } else if(is.na(ye)) {
        yS <- paste(substr(yL, 1, 3), ys)
        yL <- paste(yL, ys)
    } else {
        yS <- paste(substr(yL, 1, 3), rep(ys:ye, each = 12))
        yL <- paste(yM, rep(ys:ye, each = 12))
    }
    data.table::data.table(
        'long' = factor(yL, levels = yL, ordered = TRUE),
        'short' = factor(yS, levels = yS, ordered = TRUE)
    )
}
