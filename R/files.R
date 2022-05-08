#' Download a zipped shapefile, unzip it in a specified folder, then rename extracted files with a common name
#'
#' @param x    a url for a zipped file
#' @param d    the directory where the unzipped files should be saved
#' @param prfx the common name for the unzipped files
#'
#' @return none
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
down_unz_ren_shp <- function(x, d, prfx = NULL){
    tmpf <- tempfile()
    download.file(x, destfile = tmpf)
    unzip(tmpf, exdir = d)
    fnames <- unzip(tmpf, list = TRUE)$Name
    if(!is.null(prfx)) for(fn in fnames) file.rename(file.path(d, fn), file.path(d, paste0(prfx, substr(fn, nchar(fn)-3, nchar(fn)))))
    unlink(tmpf)
}
