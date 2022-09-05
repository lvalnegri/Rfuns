#' dd_geocode
#' 
#' Geocode addresses in a data.table, adding longitude and latitude coordinates using the `Google Maps Engine`
#' 
#' @param y   A data.table with an *address* column
#' @param rgn The ISO2 2-chars string for the Country 
#' @param na  The name for the *address* column
#' @param nx  The name for the *longitude* column (if it already exists it will be deleted)
#' @param ny  The name for the *latitude* column (if it already exists it will be deleted)
#' @param update_address if `TRUE`, the address column will be updated with the result from the Google Maps Engine
#'
#' @return A data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#' @importFrom mapsapi mp_geocode mp_get_points
#' @importFrom sf st_coordinates
#' 
#' @export
#'
dd_geocode <- function(y, rgn, na = 'address', nx = 'x_lon', ny = 'y_lat', update_address = FALSE){
    y[, c(nx, ny) := NA_real_]
    for(idx in 1:nrow(y))
        tryCatch(
            {
                yt <-as.character(y[idx, na, with = FALSE]) |> 
                        mp_geocode(region = tolower(rgn), key = Sys.getenv('GOOGLE_MAPS_API_KEY')) |> 
                        mp_get_points()
                y[idx, c(nx, ny) := as.list(yt |> st_coordinates()) ]
                if(update_address) y[idx, c(na) := yt$address_google]
            }, 
            error = function(err) NULL 
        )
}


#' dd_bbox
#' 
#' Calculate a square or circle bounding box given a distance and a pair of coordinates
#'
#' @param x_lon double. The longitude of the centre point.
#' @param y_lat double. The latitude of the centre point.
#' @param dst double. The distance from the centre point.
#' @param in.miles logical (optional). If `TRUE` uses *miles* as the units of `dist`, otherwise *kilometres*.
#'
#' @references \url{http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates}
#'
#' @return a matrix with two rows (the axis) and two columns (the limits)
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
dd_bbox <- function(x_lon, y_lat, dst, in.miles = TRUE) {

    `%+/-%`   <- \(x, margin)  x + c(-1, +1) * margin
    lon_range <- \(lonr, dlon) lonr %+/-% dlon * (180 / pi)
    lat_range <- \(latr, r)    latr %+/-% r * (180 / pi)

    r <- dist / ifelse(in.miles, 3958.756, 1000)
    lonr <- x_lon / (180 / pi)
    latr <- y_lat / (180 / pi)
    dlon <- asin(sin(r) / cos(latr))

    m <- matrix(c(lon_range(lonr, dlon), lat_range(latr, r)), nrow = 2, byrow = TRUE)
    dimnames(m) <- list(c('x_lon', 'y_lat'), c('min', 'max'))
    m

}


#' dd_calc_dist
#' 
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
dd_calc_dist <- function(x, 
                         spec = TRUE, 
                         knn = TRUE, 
                         cid = 'id', 
                         clon = 'x_lon', 
                         clat = 'y_lat', 
                         verbose = FALSE
    ){
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
#' Convert a data.table with a set of coordinates in a spatial object
#'
#' @param x a data.table with at least 3 columns: one for the id, the other two for the coordinates
#' @param cid the name of the id column
#' @param clon the name of the longitude or Easting column
#' @param clat the name of the latitude or Northing column
#' @param output specify which output to return: `sf`, `sp`, `df`, `dt`, `map`.
#' @param all when `FALSE`, only `id` and the coordinates are returned, otherwise all of `x` is returned
#' @param drop_coords when `FALSE`, only `id` and the coordinates are returned
#' @param out_names the name of the coordinates columns in the output table
#' @param crs.in the CRS of the input dataset (default is projected *British Grid*, `27700`)
#' @param crs.out the CRS of the returning object (default is geodetic *WGS84*, `4326)
#'
#' @return depending on the choice of `output`, an object of type: `sf`, `SpatialPointsDataFrame`, `dataframe`, `data.table`, `leaflet`
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
        'map'  = basemap(pnts = y |> st_transform(4326), pntsid = cid) 
    )
}

#' dd_do_pip
#' 
#' Calculates *points in polygons*, returning an augmented data.table or a map
#'
#' @param x a data.table with at least 3 columns, one for the id, the other two for the coordinates
#' @param y an `sf` object
#' @param cid the name of the column in the dataframe to be considered as <id>
#' @param clon the name of the column in the dataframe to be considered as longitude
#' @param clat the name of the column in the dataframe to be considered as latitude
#' @param pid the name of the column in the polygons to be considered as <id>
#' @param pname how to name the pid column in the output
#' @param output the type of output: 
#'               `df` returns only the two matching ids for points and polygons
#'               `mgdf` merge the result of the operation with the input dataset `x`
#'               `map` a *leaflet* map
#' @param verbose if `TRUE`, annoying message on the steps of the process
#' @param drop_void if `TRUE` and `output` is `map`, delete all polygons without any included point before mapping
#' @param ... Additional parameters to pass to the `basemap` function when output is `map`
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
dd_do_pip <- function(x, y,
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
