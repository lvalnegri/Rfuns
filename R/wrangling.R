#' Convert a set of variables in the specified datatype
#'
#' @param dt a data.table
#' @param vset the variables to convert
#' @param vtype the datatype to convert to
#'
#' @return None (albeit the data.table in input is modified with the content of the specified column cleaned and converted)
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#'   convert_vars(dts, c('type', 'phase'), 'integer')
#' }
#'
#' @export
#'
convert_vars <- function(dt, vset, vtype){
    dt[, (vset) := lapply(.SD, match.fun(paste0('as.', vtype))), .SDcols = vset]
}
