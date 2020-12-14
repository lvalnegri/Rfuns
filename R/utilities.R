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
