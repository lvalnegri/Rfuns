#' save_bnd_folder
#' 
#' @param bname The name of the output file
#' @param bpath The *root* output folder where to store the file(s)
#' 
#' Build the output folder for the function `save_bnd`
#' 
save_bnd_folder <- function(x){
    NULL
}


#' dd_save_bnd
#' 
#' Save an `sf` object as one or more in: *shapefile*, *geojson*, *KML*, `qs`, `RDS`.
#'
#' @param bnd The `sf` object to be saved
#' @param bname The name of the output file
#' @param bpath The *root* output folder where to store the file(s)
#' @param fmt2path Add the extension of the output format (or an acronym) to the output folder `bpath`
#' @param fmt The output format(s) of the file
#'            One or more in: 
#'            - `s` for *shapefile* (folders uses `shp`)
#'            - `j` for *geojson* (`gjs`)
#'            - `k` for *KML*     (`kml`)
#'            - `q` for `qs` ($R$ serialization) (`qs`)
#'            - `r` for `RDS` (native $R$ format) (`rds`)
#' @param pct `NULL`` or a 2-char string identifying the percentage of simplification.
#'            One in: '00', '10', '20', '30', '40', '50' ('00' stand for *original size*)
#'            IF not `NULL`, the string will be be added to the output folder (after `fmt`)
#' @return none
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @importFrom sf st_write
#' @importFrom qs qwrite
#' @importFrom parallel detectCores
#'
#' @export
#'
dd_save_bnd <- function(bnd, bname, bpath, fmt = 'q', fmt2path = FALSE, pct = NULL){

    if(!is.null(pct)) pct <- ifelse(is.null(pct), '', paste0('s', pct))
    if(grepl('s', fmt)){
        message('Saving boundaries as shapefile...')
        st_write(bnd, file.path(bpath, 'shp', pct), bname, driver = 'ESRI Shapefile', append = FALSE)
    }
    if(grepl('k', fmt)){
        message('Saving boundaries as shapefile...')
        st_write(bnd, file.path(bpath, 'shp', pct), bname)
    }
    if(grepl('j', fmt)){
        message('Saving boundaries as shapefile...')
        st_write(bnd, file.path(bpath, 'shp', pct), bname, append = FALSE)
    }
    if(grepl('q', fmt)){
        message('Saving boundaries as `qs`...')
        qsave(bnd, file.path(bpath, 'rds', pct, bname), nthreads = parallel::detectCores())
    }
    if(grepl('r', fmt)){
        message('Saving boundaries as `RDS`...')
        saveRDS(bnd, file.path(bpath, 'rds', pct, bname))
    }

        # st_write(y, file.path(out_path, 'idro', paste0(pc, '.kml')), append = FALSE, quiet = TRUE)
        # if(file.exists(file.path(out_path, 'idro', paste0(pc, '.geojson')))) file.remove(file.path(out_path, 'idro', paste0(pc, '.geojson')))
        # st_write(y, file.path(out_path, 'idro', paste0(pc, '.geojson')), quiet = TRUE)
    
    
}


#' dd_simplify_bnd
#' 
#' Simplify the polygons Save an `sf` object as a *shapefile* and/or in `qs` format and/or in `RDS` format
#'
#' @param bnd the `sf` object to be saved
#' @param bname the name of the output file
#' @param bpath the *root* output folder where to store the file(s)
#' @param pct a 2-char string identifying the percentage of simplification: must be '05', '10', '20', '30', '40', '50'. '00' stand for "original size"
#' @param shp if true, save as shapefile
#' @param qs if true, save as shapefile
#' @param rds if true, save in RDS format
#'
#' @return none
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @importFrom sf st_write
#' @importFrom qs qwrite
#'
#' @export
#'
simplify_bnd <- function (y, fn, bpath, kc = 1:5, save_orig = TRUE, fmt = 'q') 
{
    if (save_orig) {
        message('Saving original object...')
        if (qs) {
            qs::qsave(y, file.path(bpath, "s00", fn), nthreads = 4)
        }
        else {
            saveRDS(y, file.path(bpath, "s00", fn))
        }
    }
    for (k in kc) {
        message("Riduzione ", k * 10, "%...")
        if (qs) {
            qsave(ms_simplify(y, k/10, keep_shapes = TRUE), file.path(bpath, 
                paste0("s", k, "0"), fn), nthreads = 4)
        }
        else {
            saveRDS(ms_simplify(y, k/10, keep_shapes = TRUE), 
                file.path(bpath, paste0("s", k, "0"), fn))
        }
    }
}
