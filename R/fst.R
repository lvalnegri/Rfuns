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
write_fst_idx <- function(tname, cname, dts = NA, out_path = './', fname = NA, dname = NA){
    if(!is.na(dname)) dts <- dbm_do(dname, 'r', tname)
    setorderv(dts, cname)
    if(length(cname) == 1){
        yx <- dts[, .N, get(cname)]
    } else {
        yx <- dts[, .N, .(get(cname[1]), get(cname[2]))]
    }
    setnames(yx, c(cname, 'N'))
    yx[, n2 := cumsum(N)][, n1 := shift(n2, 1L, type = 'lag') + 1][is.na(n1), n1 := 1]
    setcolorder(yx, c(cname, 'N', 'n1', 'n2'))
    if(!is.na(fname)) tname <- fname
    write_fst(yx, file.path(out_path, paste0(tname, '.idx')))
    write_fst(dts, file.path(out_path, tname))
}

#' Read a (partial) dataset from an fst indexed file based on values pertainig to one or two columns
#'
#' @param fname the name of the fst complete of its path
#' @param values the value(s) pertaining to the column(s) that makes up the index
#' @param cols the columns to be returned (the NULL default means to return all columns)
#'
#' @return A data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table fst
#'
#' @export
#'
read_fst_idx <- function(fname, ref, cols = NULL){
    yx <- read_fst(paste0(fname, '.idx'), as.data.table = TRUE)
    if(length(ref) == 1){
        y <- yx[get(names(yx)[1]) == ref[1], .(n1 = min(n1), n2 = max(n2))]
    } else {
        if(is.na(ref[1])){
            y <- yx[get(names(yx)[2]) == ref[2], .(n1, n2)]
        } else {
            y <- yx[get(names(yx)[1]) == ref[1] & get(names(yx)[2]) == ref[2], .(n1, n2)]
        }
    }
    read_fst(fname, from = y$n1, to = y$n2, columns = cols, as.data.table = TRUE)
}
