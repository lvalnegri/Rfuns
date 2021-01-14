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
    nmd <- names(dt)
    nmk <- names(lkp)
    nr <- nrow(lkp[, .N, get(nmk[1])][N > 1])
    if(nr > 0) stop('The mapping file is not correct! There are ', nr, ' duplicates.')
    y1 <- dt[ !get(nmk[1]) %in% lkp[, get(nmk[1])] ]
    y2 <- dt[ get(nmk[1]) %in% lkp[, get(nmk[1])] ]
    y2[, nmk[2] := NULL]
    dt <- rbindlist(list( y1, lkp[y2, on = nmk[1]] ), use.names = TRUE)
    setcolorder(dt, nmd)
    if(to_factor) dt[, nmk[1] := factor(get(nmk[1]))]
    dt
}


#' Reorder the columns in a data.table so that a column, possibly renamed, is listed after another column
#'
#' @param dt a data.table
#' @param colname the name of the column to be moved
#' @param after the name of the column after which \em{colname} the will be positioned
#' @param rename the name to substitute for the moved column
#'
#' @return a data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @export
#'
insert_column <- function(dt, colname, after, rename = NULL){
    yn <- setdiff(names(dt), colname)
    setcolorder(dt, c(  yn[1:which(yn == after)], colname, yn[(which(yn == after) + 1):length(yn)] ))
    if(!is.null(rename)) setnames(dt, colname, rename)
}


#' Create a new class variable, usually using a single year age variable
#'
#' @param dt a data.table
#' @param colname the name of the column to be scanned to create the class
#' @param newcol the name of the new categorical column
#' @param bin the width of each class, apart from the highest one whose width depends on the actual values of \em{colname}
#'
#' @return a data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @export
#'
create_age_class <- function(dt, colname = 'age', newcol = NA, bin = 5){
    if(is.na(newcol)) newcol <- paste0(colname, add_space(bin))
    if(newcol %in% names(dt)) dt[, (newcol) := NULL]
    setorderv(dt, c(colname))
    dt[, x := paste0( add_space(floor(get(colname) / bin) * bin), ' Ͱ ', add_space((floor(get(colname) / bin) + 1) * bin) )]
    dt[x == dt[, max(x)], x := paste0(min(get(colname)), '+  ') ]
    dt[, x := factor(x, levels = sort(unique(dt$x)), ordered = TRUE)]
    insert_column(dt, 'x', colname, newcol)
}

