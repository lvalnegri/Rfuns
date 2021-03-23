#' Add geographical area codes to a dataset, starting from a postcode column
#'
#' @param dt a data.table
#' @param clean_pc if the postcode column needs to be cleaned beforehand
#' @param pc_cname if clean_pc is TRUE, this is the column name to consider
#' @param oa_only Add only the Output Area column, disregarding all the other options
#' @param census Add geographies related to the "Census" hierarchy: 'LSOA', 'MSOA', 'LAD'
#' @param admin Add geographies related to the "Admin" hierarchy: 'LAD', 'CTY', 'RGN', 'CTRY'
#' @param postal Add geographies related to the "Postal" hierarchy: 'PCS', 'PCD', 'PCT', 'PCA'
#' @param electoral Add geographies related to the "Electoral" hierarchy: 'PCON', 'WARD', 'CED'
#' @param nhs Add geographies related to the "NHS" hierarchy: 'CCG', 'NHSO', 'NHSR'
#' @param crime Add geographies related to the "Police" hierarchy: 'CSP', 'PFA'
#' @param cols_in Insert here isolated columns to add to the output dataset
#' @param cols_out The columns you don't want to be included in the output Note that you can not exclude neither OA nor WPZ.
#'
#' @return a data.table with possibly cleaned postcode, OA and WPZ columns, plus all other specified
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @importFrom fst read_fst
#'
#' @export
#'
add_geocodes <- function(dt,
                        clean_pc = TRUE, pc_cname = 'PCU',
                        oa_only = FALSE, add_oa = TRUE,
                        census = TRUE, admin = TRUE, postal = TRUE, electoral = FALSE, nhs = FALSE, crime = FALSE,
                        cols_in = NULL, cols_out = NULL
                ){
    dt <- copy(dt)
    cname <- names(dt)[1:which(grepl(pc_cname, names(dt)))]
    if(clean_pc) clean_postcode(dt, pc_cname)
    if(add_oa){
        y <- read_fst( file.path(geouk_path, 'postcodes'), columns = c('PCU', 'OA', 'WPZ'), as.data.table = TRUE )
        setnames(dt, pc_cname, 'PCU')
        dt <- y[dt, on = 'PCU']
        setnames(dt, 'PCU', pc_cname)
    }
    cols <- 'OA'
    if(!oa_only){
        cols_all <- c(
            'OA', 'LSOA', 'MSOA', 'LAD', 'CTY', 'RGN', 'CTRY',
            'PCS', 'PCD', 'PCT', 'PCA',
            'TTWA', 'WARD', 'PCON', 'CED', 'PAR', 'BUA', 'BUAS', 'MTC', 'CSP', 'PFA',
            'STP', 'CCG', 'NHSO', 'NHSR'
        )
        if(census) cols <- c(cols, c('LSOA', 'MSOA', 'LAD'))
        if(admin) cols <- c(cols, c('LAD', 'CTY', 'RGN', 'CTRY'))
        if(postal) cols <- c(cols, c('PCS', 'PCD', 'PCT', 'PCA'))
        if(electoral) cols <- c(cols, c('PCON', 'WARD', 'CED'))
        if(nhs) cols <- c(cols, c('CCG', 'NHSO', 'NHSR'))
        if(crime) cols <- c(cols, c('CSP', 'PFA'))
        if(!is.null(cols_in)) cols <- c(cols, cols_in)
        if(!is.null(cols_out)) cols <- setdiff(cols, setdiff(cols_out, 'OA'))
        cols <- unique(intersect(cols, cols_all))
        y <- read_fst( file.path(geouk_path, 'output_areas'), columns = cols, as.data.table = TRUE )
        dt <- y[dt, on = 'OA']
    }
    setcolorder(dt, c(cname, cols, 'WPZ'))
    droplevels(dt)
}

#' Build a lookup table child <=> parent using the postcodes table from the ONS geography database
#' This function should not be used with 'OA' as child because in the csv files from ONS there are 265 OAs missing (36 ENG, 229 SCO)
#' Always remember to check column 'pct_coverage' for values less than 100
#'
#' @param child the code for the lower level geography
#' @param parent the code for the higher level geography
#' @param is_active if TRUE, keep only live postcodes for the calculation
#' @param filter_country indicates if the calculation must be done on less than the UK
#' @param save_results if TRUE, the result dataset will also be saved
#' @param out_path if save_results is TRUE, the folder where to save the output file (which will be called "paste0(child, '_to_', parent))"
#'
#' @return a data.table with two columns
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table fst
#'
#' @export
#'
build_lookups_table <- function(
                            child,
                            parent,
                            is_active = TRUE,
                            filter_country = NULL,
                            save_results = FALSE,
                            out_path = file.path(ext_path, 'uk', 'geography', 'lookups')
                        ){
    message('Processing ', child, 's to ', parent, 's...')
    message(' - Reading postcodes data...')
    cols <- c(child, parent)
    if(!is.null(filter_country)) cols <- c(cols, 'CTRY')
    if(is_active == 1){
        pc <- read_fst_idx(file.path(geouk_path, 'postcodes'), 1, cols = cols)
    } else {
        pc <- read_fst(file.path(geouk_path, 'postcodes'), columns = cols, as.data.table = TRUE)
    }
    if(!is.null(filter_country)) pc <- pc[CTRY %in% filter_country][, CTRY := NULL]
    message(' - Aggregating...')
    setnames(pc, c('child', 'parent'))
    y <- unique(pc[, .(child, parent)])[, .N, child][N == 1][, child]
    if(length(y) > 0) y1 <- unique(pc[child %in% y, .(child, parent, pct = 100)])
    y <- unique(pc[, .(child, parent)])[, .N, child][N > 1][!is.na(child), child]
    if(length(y) > 0){
        y2 <- pc[child %in% y][, .N, .(child, parent)][order(child, -N)]
        y2 <- y2[, pct := round(100 * N / sum(N), 2), child][, .SD[1], child][, .(child, parent, pct)]
    }
    if(!exists('y1')){
        y <- y2
        exact_cov <- 0
        partial_cov <- nrow(y2)
    } else if(!exists('y2')){
        y <- y1
        exact_cov <- nrow(y1)
        partial_cov <- 0
    } else {
        y <- rbindlist(list(y1, y2))
        exact_cov <- nrow(y1)
        partial_cov <- nrow(y2)
    }
    y <- y[order(child)]
    ov_par <- nrow(unique(y[pct < 100, .(parent)]))
    setnames(y, c(child, parent, 'pct_coverage'))
    if(save_results){
        message(' - Saving results to csv file...')
        if(substr(out_path, nchar(out_path), nchar(out_path)) != '/') out_path <- paste0(out_path, '/')
        fwrite(y, paste0(out_path, child, '_to_', parent, ifelse(is.null(filter_country), '', paste0('-', filter_country)), '.csv'))
    }
    message(
        'Done! Found ', add_Kcomma(exact_cov), ' exact associations and ', add_Kcomma(partial_cov), ' partial coverage (',
        add_pct(exact_cov / nrow(y)), ' exact coverage)',
        ifelse(ov_par == 0, '.', paste0(', with ', add_Kcomma(ov_par), ' ', parent, 's involved.'))
    )
    return(y)
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


#' Convert a dataframe in SpatialPointsDataFrame
#'
#' @param x a dataframe with at least 3 columns, one for the id, the other two for the coordinates
#' @param cid the name of the id column
#' @param clon the name of the longitude or Easting column
#' @param clat the name of the latitude or Northing column
#' @param output Specify which output to return in case ENtoLL is TRUE: spdf, df, mgdf, map.
#' @param ENtoLL When TRUE, it first apply a EN transformation, before calculating geographical coordinates
#' @param crs if ENtoLL is true, the CRS to apply to x to calculate longitude and latitude
#'
#'
#' @return a SpatialPointsDataFrame in WGS84, a dataframe, a leaflet map object
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table sp
#'
#' @export
#'
conv2spdf <- function(x, cid = 'id', clon = 'x_lon', clat = 'y_lat', output = 'spdf', ENtoLL = FALSE, crs = crs.gb){
    x <- setDT(x)
    if(ENtoLL & clon == 'x_lon' & clat == 'y_lat') { clon <- 'Easting'; clat <- 'Northing' }
    y <- x[, c(cid, clon, clat), with = FALSE]
    setnames(y, c(cid, 'x_lon', 'y_lat'))
    coordinates(y) <- ~x_lon+y_lat
    if(ENtoLL){
        proj4string(y) <- crs
        y <- spTransform(y, crs.wgs)
        yc <- data.table(y@data, y@coords)
        y <- merge(y, yc, cid)
        if(!output %in% c('spdf', 'df', 'mgdf', 'map')) output <- 'spdf'
        switch(output,
            'spdf' = y,
            'df'   = yc,
            'mgdf' = {
                xn <- names(x)
                x <- yc[x, on = cid]
                setcolorder(x, c(xn[1:which(xn == clat)], 'x_lon', 'y_lat', xn[(which(xn == clat) + 1):length(xn)]))
                x
            },
            'map'  = basemap(pnts = yc, pntsid = cid)
        )
    } else {
        proj4string(y) <- crs.wgs
        y
    }

}


#' Calculates points in polygons, returning an augmented data.table or a map
#'
#' @param x a data.table with at least 3 columns, one for the id, the other two for the coordinates
#' @param y a SpatialPolygonsDataFrame
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
