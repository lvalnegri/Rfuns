#' Geocode addresses in a data.table adding longitude and latitude coordinates using the Google Maps Engine
#' 
#' @param y  The data.table with an \code{address} column
#' @param na The name for the \emph{longitude} column. If it already exists it will be deleted
#' @param nx The name for the \emph{longitude} column. If it already exists it will be deleted
#' @param ny The name for the \emph{latitude} column. If it already exists it will be deleted
#' @param update_address if \code{TRUE}, the address column will be updated with the result from the Google Maps Engine
#'
#' @return a data.table with two additional columns
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#' @importFrom mapsapi mp_geocode mp_get_points
#' 
#' @export
#'
dd_geocode <- function(y, na = 'address', nx = 'x_lon', ny = 'y_lat', update_address = FALSE){
    y[, c(nx, ny) := NA_real_]
    for(idx in 1:nrow(y))
        tryCatch({
                yt <-as.character(y[idx, na, with = FALSE]) |> 
                        mp_geocode(region = 'uk', key = Sys.getenv('GOOGLE_MAPS_API_KEY')) |> 
                        mp_get_points()
                y[idx, c(nx, ny) := as.list(yt |> sf::st_coordinates()) ]
                if(update_address) y[idx, c(na) := yt$address_google]
            }, 
            error = function(err) NULL 
        )
}



#' Add geographical area codes to a dataset, starting from a postcode column
#'
#' @param dt a data.table
#' @param clean_pc if the postcode column needs to be cleaned beforehand
#' @param pcn if clean_pc is TRUE, this is the column name to consider
#' @param oa_only Add only the Output Area column, disregarding all the other options
#' @param census Add geographies related to the "Census" hierarchy: 'LSOA', 'MSOA', 'LAD'
#' @param admin Add geographies related to the "Admin" hierarchy: 'LAD', 'CTY', 'RGN', 'CTRY'
#' @param postal Add geographies related to the "Postal" hierarchy: 'PCS', 'PCD', 'PCT', 'PCA'
#' @param electoral Add geographies related to the "Electoral" hierarchy: 'PCON', 'WARD', 'CED'
#' @param health Add geographies related to the "NHS" hierarchy: 'CCG', 'NHSO', 'NHSR'
#' @param urban Add geographies related to the "NHS" hierarchy: 'CCG', 'NHSO', 'NHSR'
#' @param social Add geographies related to the "NHS" hierarchy: 'CCG', 'NHSO', 'NHSR'
#' @param crime Add geographies related to the "Police" hierarchy: 'CSP', 'PFA'
#' @param cols_in Insert here isolated columns to add to the output dataset
#' @param cols_out The columns you don't want to be included in the output Note that you can not exclude neither OA nor WPZ.
#'
#' @return a data.table with possibly cleaned postcode, OA and WPZ columns, plus all other specified
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#' @importFrom fst read_fst
#'
#' @export
#'
dd_add_geocodes <- function(dt,
                        clean_pc = TRUE, pcn = 'PCU',
                        oa_only = FALSE, add_oa = TRUE, add_wpz = FALSE, 
                        census = TRUE, admin = TRUE, postal = TRUE, electoral = FALSE, 
                            health = FALSE, urban = FALSE, social = FALSE, crime = FALSE,
                        cols_in = NULL, cols_out = NULL
                ){
    dt <- copy(dt)
    cname <- names(dt)[1:which(grepl(pcn, names(dt)))]
    if(clean_pc) dd_clean_pcu(dt, pcn)
    if(add_oa){
        cols <- 'OA'
        if(add_wpz) cols <- c(cols, 'WPZ')
        y <- read_fst( file.path(geouk_path, 'postcodes'), columns = c('PCU', cols), as.data.table = TRUE )
        setnames(dt, pcn, 'PCU')
        dt <- y[dt, on = 'PCU']
        setnames(dt, 'PCU', pcn)
    }
    cols <- 'OA'
    if(!oa_only){
        cols_all <- dmpkg.geouk::location_types$location_type
        for(th in tolower(unique(dmpkg.geouk::location_types$theme)))
            if(get(th)) cols <- c(cols, dmpkg.geouk::location_types[theme == th, location_type])
        if(!is.null(cols_in)) cols <- c(cols, cols_in)
        if(!is.null(cols_out)) cols <- setdiff(cols, setdiff(cols_out, 'OA'))
        cols <- unique(intersect(cols, cols_all))
        y <- read_fst( file.path(geouk_path, 'output_areas'), columns = cols, as.data.table = TRUE )
        dt <- y[dt, on = 'OA']
    }
    setcolorder(dt, c(cname, cols, 'WPZ'))
    droplevels(dt)
}


#' Check if one or more strings are valid location ids, returning the correct common area type
#'
#' @param ids a vector containing location ids of one unspecified location type
#' @param give_warning if there should be any explanation for the outcome
#'
#' @return a list with five components:
#' the valid ids, the invalid ids, the area type, the suffix name for the indexed location files, the parent id
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table fst
#'
#' @export
#'
check_area_ids <- function(ids, give_warning = FALSE){
    ids <- unique(toupper(ids))
    lcn <- read_fst(file.path(geouk_path, 'locations'), as.data.table = TRUE)
    tpe <- NULL
    inv <- NULL
    for(id in ids){
        y <- lcn[location_id == id, as.character(type)]
        if(length(y) == 0){ inv <- c(inv, id) } else { tpe <- c(tpe, y) }
    }
    tpe <- unique(tpe)
    inv <- inv
    ids <- setdiff(ids, inv)
    if(length(tpe) == 0){
        if(give_warning) message('Sorry, there is no valid code to process.')
        return( list('ids' = NULL, 'inv' = inv, 'type' = tpe, 'fname' = NULL) )
    }
    if(length(tpe) > 1){
        if(give_warning) message('Sorry, the codes refer to more than one location type: ', paste(tpe, collapse = ', '), '.')
        return( list('ids' = ids, 'inv' = inv, 'type' = NULL, 'fname' = NULL) )
    }
    linv <- length(inv)
    if(linv > 0 & give_warning)
        warning('The code', ifelse(linv == 1, ' ', 's '), paste(inv, collapse = ', '), ifelse(linv == 1, ' is', ' are'), ' invalid.')

    fname <- switch(tpe,
        'MSOA' = '_msls', # 1st
        'LSOA' = '_msls', # 2nd
        'LAD' = '_ldwd',  # 1st
        'WARD' = '_ldwd', # 2nd
        'PAR' =  '_ldpr', # 2nd
        'PCON' = '_pcoa', # 1st
        'PFN' = '_pfan',  # 2nd
        'PCS' = '_pcds',  # 2nd
        'PCD' = '_pcds',  # 1st
        'PCT' = '_pcat'   # 2nd
    )

    list('ids' = ids, 'inv' = inv, 'type' = tpe, 'fname' = fname)  # parent id + type still missing

}


#' Return all entries in the locations list including or exactly with the specified string
#'
#' @param x the string to look for
#' @param tpe the location type. If NA he search is done on all the records
#' @param exact if FALSE uses a regex search
#'
#' @return a data.table with two or three columns, depending on the tpe parameter
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table fst
#'
#' @export
#'
get_area_code <- function(x, tpe = NA, exact = FALSE){
    if(is.na(tpe)){
        lcn <- read_fst(file.path(geouk_path, 'locations'), columns = c('type', 'location_id', 'name'), as.data.table = TRUE)
    } else {
        lcn <- read_fst_idx(file.path(geouk_path, 'locations'), tpe, c('location_id', 'name'))
    }
    if(exact){
        lcn[toupper(x) == toupper(name)][order(name)]
    } else {
        lcn[grepl(toupper(x), toupper(name))][order(name)]
    }
}


#' Calculate a square or circle bounding box given a distance and a pair of coordinates
#'
#' @param x_lon The longitude of the center point.
#' @param y_lat The latitude of the center point.
#' @param dist The distance from the center point.
#' @param in.miles logical.  If \code{TRUE} uses miles as the units of \code{dist}. If \code{FALSE} uses kilometers.
#'
#' @references \url{http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates}
#'
#' @return a matrix with two rows (the axis) and two columns (the limits)
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
bounding_box <- function(x_lon, y_lat, dist, in.miles = TRUE) {

    `%+/-%`   <- function(x, margin)  x + c(-1, +1) * margin
    lon_range <- function(lonr, dlon) lonr %+/-% dlon * (180 / pi)
    lat_range <- function(latr, r)    latr %+/-% r * (180 / pi)

    r <- dist / ifelse(in.miles, 3958.756, 1000)
    lonr <- x_lon / (180 / pi)
    latr <- y_lat / (180 / pi)
    dlon <- asin(sin(r) / cos(latr))

    m <- matrix(c(lon_range(lonr, dlon), lat_range(latr, r)), nrow = 2, byrow = TRUE)
    dimnames(m) <- list(c('x_lon', 'y_lat'), c('min', 'max'))
    m

}


#' Append together multiple SpatialPolygonDataFrame into one single SpatialPolygonDataFrame
#'
#' @param fnames the list of names (or ids) of the boundaries to be "merged" together
#' @param bpath the directory where ALL the boundaries are stored
#'
#' @return a SpatialPolygonDataFrame
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @importFrom raster bind
#'
#' @export
#'
bnd_merge_path <- function(fnames, bpath){
    bnd <- readRDS(file.path(bpath, fnames[1]))
    for(fname in fnames)
        bnd <- bind(bnd, readRDS(file.path(bpath, fname)))
    bnd
}


#' Rewrite a polygon inside a SpatialPolygonsDataframe cutting out some of its interior equal to another of its polygons
#'
#' @param bnd the SpatialPolygonsDataframe object of the change
#' @param outer the id of the parent polygon
#' @param hole the id of the child (hole) polygon
#'
#' @return a SpatialPolygonsDataFrame
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import sp
#'
#' @importFrom rgeos gDifference
#'
#' @export
#'
fix_hole <- function(bnd, outer, hole){
    bnd@polygons[grepl(outer, bnd@data$id)] <-
        gDifference( bnd[grepl(outer, bnd@data$id),], bnd[grepl(hole,  bnd@data$id),])@polygons
    bnd
}

#' Rewrite a list of polygons inside a SpatialPolygonsDataframe cutting out some of its interior equal to another of its polygons
#'
#' @param bnd the SpatialPolygonsDataframe object of the change
#' @param ids a 2-cols dataframe, the first column with the ids of the outer polygons, the second the ids of the corresponding holes
#'
#' @return a SpatialPolygonsDataFrame
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
fix_holes <- function(bnd, ids){ # ids => df with col1 = outer, col2 = holes
    for(idx in 1:nrow(ids))
        bnd <- fix_hole(bnd, ids[idx, 1], ids[idx, 2])
}

#' Save a SpatialPolygonsDataframe either/both as a shapefile and/or in RDS format
#'
#' @param bnd the SpatialPolygonsDataframe to be saved
#' @param bname the name of the output file
#' @param shp if true, save as shapefile
#' @param rds if true, save in RDS format
#' @param bpath the "root" directory where to store the  file
#' @param pct a 2-char string identifying the percentage of simplification: must be '05', '10', '20', '30', '40', '50'. '00' stand for "original size"
#'
#' @return none
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @importFrom sf st_write
#'
#' @export
#'
save_bnd <- function(bnd, bname, shp = TRUE, rds = TRUE, bpath = bnduk_path, pct = '00'){

    pct <- ifelse(is.null(pct), '', paste0('s', pct))

    if(shp){
        message('Saving boundaries as shapefile...')
        st_write(bnd, file.path(bpath, 'shp', pct), bname, driver = 'ESRI Shapefile', append = FALSE)
    }

    if(rds){
        message('Saving boundaries as RDS...')
        saveRDS(bnd, file.path(bpath, 'rds', pct, bname))
    }

}


#' Clip (crop) a SpatialPolygonsDataframe against the UK extent (with or without Northern Scotland Islands)
#'
#' @param bnd the SpatialPolygonsDataframe to be clipped
#' @param crop_islands if the extent should exlude the northern Scotland isles
#'
#' @return a SpatialPolygonsDataframe
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @importFrom raster crop
#'
#' @export
#'
crop_uk <- function(bnd, crop_islands = TRUE){
    uk <- readRDS(file.path(bnduk_path, 'rds', 's00', ifelse(crop_islands, 'UKni', 'UK')))
    crop(bnd, uk)
}


#' Calculates the complete set of pair distances for a set of locations
#'
#' @param x a data.table with at least 3 columns, one for the id, the other two for the coordinates
#' @param spec double each pair, so you can query a single column
#' @param knn calculate the order of the neighbour for each pair (implies \code{spec = TRUE})
#' @param cid the name of the id column
#' @param clon the name of the longitude column
#' @param clat the name of the latitude column
#' @param verbose tracks the calculation
#'
#' @return a data.table in long format with the ids of each pair of locations and the corresponding distance.
#'         Optionally, the ranking order.
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#' @importFrom geosphere distVincentyEllipsoid
#'
#' @export
#'
calc_distances <- function(x, spec = TRUE, knn = TRUE, cid = 'id', clon = 'x_lon', clat = 'y_lat', verbose = FALSE){
    y <- x[, c(cid, clon, clat), with = FALSE]
    setnames(y, c('id', 'x_lon', 'y_lat'))
    setorder(y, 'id')
    dist <- rbindlist(
                lapply(1:(nrow(y) - 1),
                    function(idx){
                        if(verbose) message(' * Processing record ', idx, ' (', round(100 * idx / nrow(y), 1), '%)')
                        yt <- y[(idx + 1):nrow(y)]
                        data.table(
                            idA = y[idx, id],
                            idB = yt[, id],
                            distance = distVincentyEllipsoid(
                                y[idx, .(x_lon, y_lat)],
                                yt[, .(x_lon, y_lat)])
                        )
                    }
                )
    )
    if(knn) spec <- TRUE
    if(spec){
        dist <- rbindlist(list( dist, dist[, .(idA = idB, idB = idA, distance)] ))
        dist <- dist[order(idA, distance)]
        if(knn) dist[, knn := 1:.N, idA]
    }

    dist

}


#' dd_conv2spat
#' 
#' Convert a data.table in a spatial object
#'
#' @param x a data.table with at least 3 columns: one for the id, the other two for the coordinates
#' @param cid the name of the id column
#' @param clon the name of the longitude or Easting column
#' @param clat the name of the latitude or Northing column
#' @param output specify which output to return: sf, sp, df, dt, map.
#' @param all when \code{FALSE}, only \code{id} and the coordinates are returned, otherwise all of \code{x} is returned
#' @param drop_coords when \code{FALSE}, only \code{id} and the coordinates are returned
#' @param out_names the name of the coordinates columns in the output table
#' @param crs.in the CRS of the input dataset
#' @param crs.out the CRS of the returning object
#'
#' @return depending on the choice of \code{output}, an object of type: sf, SpatialPointsDataFrame, dataframe, data.table, leaflet
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table 
#' @importFrom sf st_drop_geometry st_coordinates st_transform st_as_sf
#' @importFrom mapview mapview
#'
#' @export
#'
dd_df2spat <- function(x, 
                     cid = 'id', 
                     clon = 'Easting', 
                     clat = 'Northing', 
                     output = 'sf', 
                     all = FALSE,
                     drop_coords = TRUE,
                     out_names = c('x_lon', 'y_lat'), 
                     crs.in = 27700,
                     crs.out = 4326
                ){
    if (!any(c(cid, clon, clat) %in% names(x))) stop('The required columns are not in the table!')
    if (!is.data.table(x)) x <- as.data.table(x)
    y <- x[, c(cid, clon, clat), with = FALSE]
    setnames(y, c(cid, 'x_lon', 'y_lat'))
    y <- y |> sf::st_as_sf(coords = c('x_lon', 'y_lat'), crs = crs.in)
    y <- y |> sf::st_transform(crs.out)
    if (!output %in% c('sf', 'sp', 'df', 'dt', 'map')) output <- 'sf'
    if(all){
        y <- y |> merge(x)
        if(drop_coords) y[, c(clon, clat)] <- NULL
    }
    switch(output, 
        'sf'   = y, 
        'sp'   = as(y, 'Spatial'), 
        'df'   = as.data.frame(cbind( 
                    y |> sf::st_drop_geometry(), 
                    y |> sf::st_coordinates() |> as.data.table() |> setnames(out_names)
                 )), 
        'dt'   = cbind( 
                    y |> sf::st_drop_geometry(), 
                    y |> sf::st_coordinates() |> as.data.table() |> setnames(out_names)
                 ), 
        'map'  = if (crs.out != 4326) { 
                    mapview::mapview(y) 
                 } else { 
                    basemap(pnts = y, pntsid = cid) 
                 }
    )
}

#' Calculates points in polygons, returning an augmented data.table or a map
#'
#' @param x a data.table with at least 3 columns, one for the id, the other two for the coordinates
#' @param y an \code{sf} object
#' @param cid the name of the column in the dataframe to be considered as <id>
#' @param clon the name of the column in the dataframe to be considered as longitude
#' @param clat the name of the column in the dataframe to be considered as latitude
#' @param pid the name of the column in the polygons to be considered as <id>
#' @param pname how to name the pid column in the output
#' @param output the type of output: 'df' returns only the two matching ids for points and polygons, 'mgdf' merge the result of the operation with the input df, <map> a leaflet map
#' @param verbose if TRUE, annoying message on the steps of the process
#' @param drop_void if TRUE and output is <map>, delete all polygons without any included point before mapping
#' @param ... Additional parameters to pass to the <basemap> function if output is <map>
#'
#' @return a data.table or a leaflet map
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table sp
#' @importFrom raster compareCRS
#'
#' @export
#'
do_pip <- function(x, y,
            cid = 'id',
            clon = 'x_lon',
            clat = 'y_lat',
            pid = 'id',
            pname = NA,
            output = 'mgdf',
            verbose = FALSE,
            drop_void = TRUE,
            ...
    ){
        if(!drop_void & output == 'map' & length(y) > 4000){
            pr <- readline(paste0("You're trying to map lots of feature (", length(y), "). Are you sure? (n/N to quit)" ))
            if(toupper(pr) == 'N') exit()
        }
        if(verbose) message('Converting dataframe into a spatial object...')
        xp <- conv2spdf(x, cid = cid, clon = clon, clat = clat)
        if(!compareCRS(xp, y)){
            if(verbose) message('Aligning coordinates reference system...')
            xp <- spTransform(xp, proj4string(y))
        }
        if(verbose) message('Processing point in polygons...')
        xp <- data.table(xp@data[, cid], over(xp, y))
        if(is.na(pname)) pname <- pid
        setnames(xp, c(cid, pname))
        if(verbose) message('...')
        x <- xp[x, on = cid]
        switch(output,
            'df' = xp,
            'mgdf' = {
                setcolorder(x, names(x)[1:which(names(x) == clat)])
                x
            },
            'map' = {
                if(verbose) message('...')
                if(drop_void) y <- subset(y, y[[pid]] %in% x[, get(pid)])
                basemap(pnts = x, pntsid = cid, bnd = y, bndid = pid, ...)
            }
    )
}


#' Calculates points in polygons using default UK polygons, returning an augmented data.table
#'
#' @param x a data.table with at least 3 columns, one for the id, the other two for the geographic coordinates
#' @param areatype the reference area to operate on for the point in polygon process
#' @param ... Additional parameters to pass to the <do_pip> function, and/or the <basemap> function if "output" is <map>
#'
#' @return a data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @export
#'
do_pip_uk <- function(x, areatype = 'OA', ...){
    y <- readRDS(file.path(bnduk_path, 'rds', 's00', areatype))
    do_pip(x, y, pname = areatype, ...)
}
