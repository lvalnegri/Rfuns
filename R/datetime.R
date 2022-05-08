#' Create a dataset of ordered months, possibly by year(s)
#'
#' @param ys the start year, or the only year if \code{ye} is missing. If missing, the function returns only the months.
#' @param ye the final year
#'
#' @return a data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @export
#'
dd_months <- function(ys = NA, ye = NA){
    yL <- month.name
    if(is.na(ys)){
        yS <- month.abb
    } else if(is.na(ye)) {
        yS <- paste(substr(yL, 1, 3), ys)
        yL <- paste(yL, ys)
    } else {
        yS <- paste(substr(yL, 1, 3), rep(ys:ye, each = 12))
        yL <- paste(yL, rep(ys:ye, each = 12))
    }
    data.table(
        'long' = factor(yL, levels = yL, ordered = TRUE),
        'short' = factor(yS, levels = yS, ordered = TRUE)
    )
}


#' Transform and classify time values
#'
#' @param x a vector of time values in \code{POSIXct} format
#' @param prec the value in seconds to use as span for the bins
#' @param meth a valid \code{R} operation to be used to round values in classes
#'
#' @return a vector
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
dd_round_time = function(x, prec = 1, meth = floor) {
  if (!'POSIXct' %in% class(x)) stop('Input times must be in POSIXct format!')
  as.POSIXct( meth(as.numeric(x) / prec) * prec, tz = attributes(x)$tzone, origin = '1970-01-01')
}

