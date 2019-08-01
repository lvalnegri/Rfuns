#' Write a dataset in fst format with an index over one of its columns
#'
#' @param tname 
#' @param cname 
#' @param dts 
#' @param out_path 
#' @param fname 
#' @param dname 
#'
#' @return None
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table DBI fst RMySQL
#'
#' @export
#'
write_fst_idx <- function(tname, cname, dts, out_path, fname = NA, dname = NA){
    if(!is.na(dbcall)) dts <- dbm_do(dname, 'r', tname)
    setorderv(dts, cname)
    yx <- dts[, .N, get(cname)]
    setnames(yx, c(cname, 'N'))
    yx[, n2 := cumsum(N)][, n1 := shift(n2, 1L, type = 'lag') + 1][is.na(n1), n1 := 1]
    setcolorder(yx, c(cname, 'N', 'n1', 'n2'))
    if(!is.na(fname)) tname <- fname
    write_fst(yx, file.path(out_path, paste0(tname, '.idx')))
    write_fst(dts, file.path(out_path, tname))
}

#' Read a (partial) dataset from an fst indexed file based on a set of values
#'
#' @param fname the name of the fst complete of its path
#' @param values the set of values pertaining to the column that makes up the index
#'
#' @return A data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table fst
#'
#' @export
#'
read_fst_idx <- function(fname, values){

}
