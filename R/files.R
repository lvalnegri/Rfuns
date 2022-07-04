#' save_dts_pkg
#' 
#' Save a dataset in different formats in various directory
#'
#' @param y          a dataset
#' @param fn         a common name for all the file formats 
#' @param fst_path   the path where to save the file in `fst` format (usually inside the *public* repository)
#' @param as_rdb     if `TRUE`, save the dataset in the `dbn` database. 
#' @param dbn        the name of the database to be used when `as_rdb` is `TRUE`
#' @param tbn        The name for the database table where to store the dataset (by default equals the filename).
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
save_dts_pkg <- function(y, fn, fst_path, as_rdb = TRUE, dbn = NULL, tbn = fn, as_rda = TRUE, csv_in_pkg = TRUE, csv2zip = FALSE){
    write_fst(y, file.path(fst_path, fn))
    if(as_rdb) dd_dbm_do(dbn, 'w', tbn, y)
    if(csv_in_pkg){
        fwrite(y, paste0('./data-raw/', fn, '.csv'))
        if(csv2zip){
            zip(paste0('./data-raw/', fn, '.csv.zip'), paste0('./data-raw/', fn, '.csv'))
            file.remove(paste0('./data-raw/', fn, '.csv'))
        }
    }
    if(as_rda){
        assign(fn, y)
        save( list = fn, file = file.path('data', paste0(fn, '.rda')), version = 3, compress = 'gzip' )
    }
}


#' down_shp
#' 
#' Download a zipped *shapefile* and returns it as an `sf` object. 
#' If the zip file contains multiple shapefiles, a specific file name can be passed, or the first will be returned.
#'
#' @param x   a url for a zipped file containing one or more shapefiles
#' @param fn  a specific name to extract and convert
#' @param lst only prints the names of the included files
#'
#' @return an `sf` object
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
down_shp <- function(x, fn = NA, lst = FALSE){
    tmpf <- tempfile()
    tmpd <- tempdir()
    download.file(x, tmpf)
    if(lst){
        y <- unzip(tmpf, list = TRUE)
    } else {
        if(is.na(fn)){
            unzip(tmpf, exdir = tmpd)
            y <- sf::st_read(file.path(tmpd, grep('\\.shp$', unzip(tmpf, list = TRUE)$Name, value = TRUE)[1]))
        } else {
            yn <- grep(fn, unzip(tmpf, list = TRUE)$Name, value = TRUE)
            unzip(tmpf, yn, exdir = tmpd, junkpaths = TRUE)
            y <- sf::st_read(file.path(tmpd, paste0(fn, '.shp')))
        }            
        unlink(tmpf)
        unlink(tmpd)
    }
    y
}
