#' dd_ohe
#' 
#' One Hot Encoding
#'
#' @param x A vector of values for an attribute
#' @param add_is if `TRUE`, add `is_` in front of each value as the column name
#'
#' @return A data.table with as many columns as the unique values in the input.
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @export
#' 
dd_ohe <- function(x, add_is = TRUE){
    x <- data.table(ohe = x)
    xv <- sort(unique(x$ohe))
    xn <- xv; if(add_is) xn <- paste0('is_', xn)
    x[, c(xn) := lapply(xv, \(y) as.integer(ohe == y))][, ohe := NULL]
}
