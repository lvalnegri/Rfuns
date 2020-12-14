#' Write a dataset in fst format with an index over one of its columns
#'
#' @param x the dataset to be split
#' @param pct percentage of the dataset to include as training
#'
#' @return a list with two data.tables
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
split_dts <- function(x, pct = 0.7, grouped = NULL){
    if(is.null(grouped)){
        xs <- sample(nrow(x), pct * nrow(x))
        list('training' = x[xs], 'test' = x[-xs])
    } else {
        y <- x[ x[, .I[sample(.N, pct * .N)], get(grouped)][[2]]]
        list('training' = y, 'test' = fsetdiff(x, y, all = TRUE))
    }
}


#' Split a dataset for a $k$-fold Cross-Validation
#'
#' @param x the dataset to be split
#' @param n the number of folds to split
#'
#' @return a list with n items corresponding to the required $k$ folds
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
xval <- function(x, n){

}
