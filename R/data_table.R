#' Update one column of a data.table based on a second lookup data.table
#'
#' @param y  The data.table to be updated
#' @param yk The lookup data.table with \emph{ids} and \emph{values} to update in \code{y}
#' @param ny The name of the column in \code{y} to consider as \emph{ids}
#' @param vy The name of the column in \code{y} to consider as \emph{values}
#' @param nk The name of the column in \code{yk} to consider as \emph{ids}. If \code{NULL} it equates to \code{ny}
#' @param vk The name of the column in \code{yk} to consider as \emph{values}. If \code{NULL} it equates to \code{vy}
#'
#' @return A data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @export
#' 
dd_dt_update_col <- function(y, yk, ny, vy, nk = NULL, vk = NULL){
    if(is.null(nk)) nk <- ny
    if(is.null(vk)) vk <- vy
    eval(parse(text = paste0("y[yk, ", vy, " := i.", vk, ", on = c(", ny, " = '", nk, "')]")))
}

