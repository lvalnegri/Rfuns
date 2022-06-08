#' Download a zip file and extract a csv file therein.
#' If multiple csv files are included in the zip file, only the biggest one is returned, unless a name is given.
#'
#' @param url the url of the zip file to be downloaded
#' @param fname the name of the csv to be read. When NULL, the biggest , or the only, file included.
#' @param cols to restrict selection of columns
#' @param coln to change column names
#'
#' @return a data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @export
#'
get_csv_zip <- function(url, fname = NULL, cols = NULL, coln = NULL){
    tmpf <- tempfile()
    tmpd <- tempdir()
    message('Downloading zip file...')
    download.file(url, tmpf)
    if(is.null(fname)){
        fname <- unzip(tmpf, list = TRUE)
        fname <- fname[grepl('.csv$', fname$Name),]
        fname <- fname[order(fname$Length, decreasing = TRUE), 'Name'][1]
    }
    message('Extracting csv file...')
    unzip(tmpf, files = fname, exdir = tmpd, junkpaths = TRUE)
    fname <- basename(fname)
    message('Reading csv file...')
    if(is.null(coln)){
        fread(file.path(tmpd, fname), select = cols)
    } else {
        fread(file.path(tmpd, fname), select = cols, col.names = coln)
    }
}

#' Download zip files and save inner files depending on some content
#'
#' @param url the url of the zip file to be downloaded
#' @param fname
#' @param out_path
#' @param ext
#' @param shp if TRUE, the zip file contains shapefiles; if FALSE a file with the extension specified in <ext>
#' @param bndid if not NA, the name(s) of the column(s) to be carried over. If only one column, that column become the new id of the object
#' @param btransform if not NA, the polygons
#' @param crs the new Coordinate System for the polygons
#' @param bsimplify
#' @param spct
#'
#' @return None, a data.table, or a SpatialPolygons{DataFrame}
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#' @importFrom rgdal readOGR
#' @importFrom sp spTransform
#' @importFrom rmapshaper ms_simplify
#'
#' @export
#'
dunzip <- function(url,
                   fname = NA,
                   out_path = NA,
                   ext = 'csv',
                   shp = FALSE,
                   bndid = NA,
                   btransform = TRUE,
                   crs = crs.wgs,
                   bsimplify = FALSE,
                   spct = 0.05
    ){
        spath <- gsub(' ', '_', gsub(':', '-', Sys.time()))
        tpath <- file.path(pub_path, 'temp', 'downloads', spath)
        dir.create(tpath)
        if(!is.na(fname)){
            if(is.na(out_path)){
                out_path <- tpath
            } else {
                if(!dir.exists(out_path)) dir.create(out_path)
            }
        }
        zname <- tempfile()
        message('Downloading file...')
        download.file(url, zname)
        message('Unzipping file...')
        unzip(zname, exdir = tpath)
        fnames <- unzip(zname, list = TRUE)
        unlink(zname)

        if(shp){

            message('Reading shapefile...')
            y <- readOGR(tpath, unique(tools::file_path_sans_ext(fnames$Name)))
            if(!is.na(bndid)){
                message('Cleaning data slot...')
                y <- y[, bndid]
                if(length(bndid) == 1){
                    colnames(y@data) <- 'id'
                    y <- spChFIDs(y, y$id)
                }
            }
            if(btransform){
                message('Changing Coordinate System...')
                y <- spTransform(y, crs)
            }
            if(bsimplify){
                message('Simplifying polygons...')
                y <- ms_simplify(y, keep = spct)
            }
            if(!is.na(fname)) save_bnd(y, fname)

        } else {

            xnames <- fnames[grepl(paste0('\\.', ext, '$'), fnames)]
            tname <- xnames[order(xnames$Length, decreasing = TRUE), 'Name'][1]
            if(!is.na(fname)){
                file.rename(file.path(tpath, tname), file.path(out_path, fname))
            } else {
                y <- fread(file.path(tpath, tname))
            }

        }

        if(is.na(fname)){
            system(paste('rm -r', tpath))
            return(y)
        } else {
            if(out_path == tpath){
                file.remove(file.path(out_path, fnames$Name))
            } else {
                system(paste('rm -r', tpath))
            }
        }

}

#' Load packages
#'
#' @param uk  a logical to load the `UK` set of packages: 'RgeoUK', 'RbndUK', 'RdataUK'.
#' @param it  a logical to load the `IT` set of packages: 'RgeoIT', 'RbndIT', 'RdataIT'.
#' @param eu  a logical to load the `EU` set of packages: 'RgeoWD', 'RbndWD', 'RdataWD'.
#' @param wd  a logical to load the `WD` set of packages: 'RgeoWD', 'RbndWD', 'RdataWD'.
#' @param ... other packages to be loaded
#'
#' @return none
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
load_pkgs <- function(..., uk = FALSE, it = FALSE, eu = FALSE, wd = FALSE){
    invisible( lapply(c('Rfuns', ...), require, char = TRUE) )
    pn <- c('Rbnd', 'Rgeo', 'Rdata')
    if(uk) invisible( lapply(paste0(pn, 'UK'), require, char = TRUE) )
    if(it) invisible( lapply(paste0(pn, 'IT'), require, char = TRUE) )
    if(eu) invisible( lapply(paste0(pn, 'EU'), require, char = TRUE) )
    if(wd) invisible( lapply(paste0(pn, 'WD'), require, char = TRUE) )
}
