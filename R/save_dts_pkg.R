#' save_dts_pkg
#' 
#' Save a dataset in different formats, in various directories
#'
#' @param y          a dataset
#' @param fn         a common name for all the file formats 
#' @param fst_path   the (optional) path where to save the file in `fst` format (usually inside the *public* repository)
#' @param fst_cols   When not `NULL`, save the dataset as an indexed `fst` file
#' @param as_rdb     if `TRUE`, save the dataset in the `dbn` database. 
#' @param dbn        the name of the database to be used when `as_rdb` is `TRUE`
#' @param tbn        The name for the database table where to store the dataset (by default equals `fn`).
#'                   The table should be conveniently created beforehand.
#'                   The current table will be truncated, so that existing data will be deleted
#' @param as_rda     if `TRUE`, and when inside a package project, save the dataset in the `data` ready to be exported
#' @param csv_in_pkg if `TRUE`, and when inside a package project, save the dataset as a csv file inside the `data-raw` directory 
#' @param csv2zip    if `TRUE` with also `csv_in_pkg` `TRUE`, and when inside a package project, zips the csv file inside the `data-raw` directory then deletes it
#'
#' @return none
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
save_dts_pkg <- function(y, 
                         fn, 
                         fst_path = NULL, 
                         fst_cols = NULL, 
                         as_rdb = TRUE, 
                         dbn = NULL, 
                         tbn = fn, 
                         as_rda = TRUE, 
                         csv_in_pkg = TRUE, 
                         csv2zip = FALSE
                ){
    if(!is.null(fst_path)){
        if(is.null(fst_cols)){
            write_fst(y, file.path(fst_path, fn))
        } else {
            write_fst_idx(fn, fst_cols, dts = y, out_path = fst_path)
        }
    }
    if(as_rdb) dd_dbm_do(dbn, 'w', tbn, y)
    if(csv_in_pkg){
        fwrite(y, paste0('./data-raw/csv/', fn, '.csv'))
        if(csv2zip){
            zip(paste0('./data-raw/csv/', fn, '.csv.zip'), paste0('./data-raw/', fn, '.csv'))
            file.remove(paste0('./data-raw/csv/', fn, '.csv'))
        }
    }
    if(as_rda){
        assign(fn, y)
        save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )
    }
}
