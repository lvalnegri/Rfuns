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
#' @export
#'
convert_vars <- function(dt, vset, vtype){
    dt[, (vset) := lapply(.SD, match.fun(paste0('as.', vtype))), .SDcols = vset]
}


#' Update a column in a data.table (partially or totally) using the values of another column
#'
#' @param dt a data.table
#' @param lkp a mapping between two column of dt, the names must reflect the names in *dt*
#' @param to_factor if TRUE convert the updated column as factor
#'
#' @return a data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @export
#'
dt_update <- function(dt, lkp, to_factor = FALSE){
    nr <- nrow(lkp[, .N, LSOA][N > 1])
    if(nr > 0) stop('The mapping file is not correct! There are ', nr, ' duplicates.')
    nmd <- names(dt)
    nmk <- names(lkp)
    y1 <- dt[ !get(nmk[1]) %in% lkp[, get(nmk[1])] ]
    y2 <- dt[ get(nmk[1]) %in% lkp[, get(nmk[1])] ]
    y2[, nmk[2] := NULL]
    dt <- rbindlist(list( y1, lkp[y2, on = 'LSOA'] ), use.names = TRUE)
    setcolorder(dt, nmd)
    if(to_factor) dt[, get(nmk[1]) := factor(get(nmk[1]))]
    dt
}
