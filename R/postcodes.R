#' Check a character string is a UK postcode, then convert it to a 7-char format
#'
#' The UK postcode system is hierarchical, the top level being "Postcode Area" (PCA) identified by 1 or 2 alphabetical character.
#' The next level is the "Postcode District" (PCD), also commonly known as the "outcode", and can take on several different formats, and anywhere from 2 to 4 alphanumeric characters long.
#' Next comes the Postcode Sector" (PCS), always identified by a single number, then finally the "unit", always formed by two alphabetical characters.
#' The combination of "sector" and "unit" is often called "incode", which is always 1 numeric character followed by 2 alphabetical characters.
#'
#' @param dt a data.table
#' @param cname the column of <dt> to be checked and converted
#'
#' @return None (albeit the data.table in input is modified with the content of the specified column cleaned and converted)
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#'   clean_postcode(dts, 'postcode')
#' }
#'
#' @export
#'
clean_postcode <- function(dt, cname = 'postcode'){
    setnames(dt, cname, 'X')
    dt[, X := toupper(gsub('[[:punct:]| ]', '', X)) ]
    dt[!grepl('[[:digit:]][[:alpha:]][[:alpha:]]$', X), X := NA]
    dt[grepl('^[0-9]', X), X := NA]
    dt[nchar(X) < 5 | nchar(X) > 7, X := NA]
    dt[nchar(X) == 5, X := paste0( substr(X, 1, 2), '  ', substring(X, 3) ) ]
    dt[nchar(X) == 6, X := paste0( substr(X, 1, 3), ' ', substring(X, 4) ) ]
    setnames(dt, 'X', cname)
}

#' Check a character string is a UK postcode, then convert it to a 7-char format
#'
#' The UK postcode system is hierarchical, the top level being "Postcode Area" (PCA) identified by 1 or 2 alphabetical character.
#' The next level is the "Postcode District" (PCD), also commonly known as the "outcode", and can take on several different formats, and anywhere from 2 to 4 alphanumeric characters long.
#' Next comes the Postcode Sector" (PCS), always identified by a single number, then finally the "unit", always formed by two alphabetical characters.
#' The combination of "sector" and "unit" is often called "incode", which is always 1 numeric character followed by 2 alphabetical characters.
#'
#' Faster version than <clean_postcode>, returns a data.table instead of directly updating its argument
#'
#' @param dt a data.table
#' @param cname the column of <dt> to be checked and converted
#'
#' @return A data.table
#'
#' @author Luca Valnegri, \email{LucaValnegri@theambassadors.com}
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#'   dts <- clean_postcode(dts)
#' }
#'
#' \dontrun{
#'   y <- clean_postcode(dts, 'pc')
#' }
#'
#' @export
#'
clean_postcode_dt <- function(dt, cname = 'postcode'){
    nms <- names(dt)[1:which(names(dt) == cname)]
    yo <- copy(dt)
    setnames(yo, cname, 'Y')
    y <- unique(yo[, .(X = Y, Y)])
    y[, X := toupper(gsub('[[:punct:]| ]', '', X)) ]
    y[!grepl('[[:digit:]][[:alpha:]][[:alpha:]]$', X), X := NA]
    y[grepl('^[0-9]', X), X := NA]
    y[nchar(X) < 5 | nchar(X) > 7, X := NA]
    y[nchar(X) == 5, X := paste0( substr(X, 1, 2), '  ', substring(X, 3) ) ]
    y[nchar(X) == 6, X := paste0( substr(X, 1, 3), ' ', substring(X, 4) ) ]
    y <- y[!is.na(X)]
    setnames(y, 'X', cname)
    y <- y[yo, on = 'Y'][, Y := NULL]
    setcolorder(y, nms)
    y
}

#' Extraxt the coordinates of a postcode
#'
#' @param postcode
#'
#' @return a data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table fst
#'
#' @export
#'
get_postcode_coords <- function(postcode){
                pc <- clean_postcode(data.table(postcode = postcode))
                pcs <- read_fst_idx(
                            file.path(geouk_path, 'postcodes_pcds'),
                            c(NA, substr(postcode, 1, 5)),
                            c('postcode', 'x_lon', 'y_lat')
                )
                pcs[postcode == pc$postcode]
}


#' Calculate the set of postcodes included in a bounding box or circle with a specified postcode unit as centroid
#'
#' @param postcode
#' @param distance
#' @param circle
#' @param in.miles
#' @param active_only
#'
#' @return a data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table fst
#'
#' @importFrom raster pointDistance
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
#'
get_postcode_neighbors <- function(postcode, distance = 0.5, circle = TRUE, in.miles = TRUE, active_only = TRUE){
    pc <- get_postcode_coords(postcode)
    yb <- bounding_box(pc$x_lon, pc$y_lat, distance, in.miles)
    cols <- c('postcode', 'x_lon', 'y_lat', 'is_active')
    pcs <- read_fst(file.path(geouk_path, 'postcodes'), columns = cols, as.data.table = TRUE)
    if(active_only) pcs <- pcs[is_active == 1, -c('is_active')]
    pcs <- pcs[ x_lon >= yb[1, 1] & x_lon <= yb[1, 2] & y_lat >= yb[2, 1] & y_lat <= yb[2, 2] ]
    if(circle){
        setorder(pcs, 'postcode')
        y <- data.table(postcode = pcs$postcode, dist = pointDistance(pcs[, 2:3], pc[, 2:3], lonlat = TRUE))
        y <- y[dist > distance * ifelse(in.miles, 1609.34, 1000), postcode]
        pcs <- pcs[!postcode %in% y]
    }
    pcs
}


#' Return a map of the set of postcodes included in a bounding box or circle with a specified postcode unit as centroid
#'
#' @param postcode
#' @param distance
#' @param circle
#' @param in.miles
#'
#' @return a leaflet map
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import leaflet
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
#'
map_postcode_neighbors <- function(postcode,
                                    distance = 0.5, circle = TRUE, in.miles = TRUE, active_only = FALSE,
                                    tiles = tiles.lst, use_icons = FALSE
                                    # active: radius, weight, color, fcolor, fopacity
                                    # term:   radius, weight, color, fcolor
                        ){
    pc <- get_postcode_neighbors(postcode, distance, circle, in.miles, active_only)
    mp <- basemap(bbox = data.frame(c(min(pc$x_lon), min(pc$y_lat)), c(max(pc$x_lon), max(pc$y_lat))), tiles = tiles)
    if(active_only){
        mp <- mp %>%
            addCircles(
                data = pc,
                lng = ~x_lon, lat = ~y_lat,
                radius = 18,
                weight = 1,
                color = 'darkgreen',
                opacity = 1,
                fillColor = 'green',
                fillOpacity = 0.5,
                label = ~postcode
            ) %>%
            addLayersControl( baseGroups = names(tiles.lst) )
    } else {
        if(nrow(pc[is_active == 0]) > 0){
            mp <- mp %>%
                addCircles(
                    data = pc[is_active == 0],
                    lng = ~x_lon, lat = ~y_lat,
                    group = 'Terminated Postcodes',
                    radius = 8,
                    weight = 1,
                    color = 'darkred',
                    opacity = 1,
                    fillColor = 'red',
                    fillOpacity = 0.5,
                    label = ~postcode
                ) %>%
                addCircles(
                    data = pc[is_active == 1],
                    lng = ~x_lon, lat = ~y_lat,
                    group = 'Active Postcodes',
                    radius = 8,
                    weight = 1,
                    color = 'darkgreen',
                    opacity = 1,
                    fillColor = 'green',
                    fillOpacity = 0.5,
                    label = ~postcode
                ) %>%
                addLayersControl(
                    baseGroups = names(tiles.lst),
                    overlayGroups = c('Active Postcodes', 'Terminated Postcodes'),
                    options = layersControlOptions(collapsed = TRUE)
                )
        } else {
            addCircles(
                data = pc,
                lng = ~x_lon, lat = ~y_lat,
                radius = 8,
                weight = 1,
                color = 'darkgreen',
                opacity = 1,
                fillColor = 'green',
                fillOpacity = 0.5,
                label = ~postcode
            ) %>%
            addLayersControl( baseGroups = names(tiles.lst) )
        }
    }

    # mp %>%
    #     addControl(
    #         htmltools::tags$div(htmltools::HTML('
    #             <span style="border-radius:20px;font-size:24pt;font-color:#FFF;strong:700">',
    #             '???',
    #             '</span>'
    #         ))  ,
    #         position = 'bottomleft'
    #     )

    mp

}

#' Returns the set of postcodes included in a specified geographical area
#'
#' @param id The ONS code of the location we want postcodes from
#' @param active_only If TRUE, only active postcodes are returned.
#' If FALSE, all postcodes are returned, with an additional <is_active> column to determine if a postcode is active or terminated
#'
#' @return a data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table fst
#'
#' @importFrom raster pointDistance
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
#'
get_postcodes_area <- function(ids, active_only = TRUE){
    ids <- check_area_ids(ids)
    if(is.null(ids$type) | is.null(ids$ids)){
        message('Sorry, there is no valid code to process.')
        return(NULL)
    }
    if(!ids$type %in% c('LSOA', 'MSOA', 'LAD', 'PCON', 'WARD', 'PAR', 'PFN', 'PCS', 'PCD', 'PCT')){
        message('Sorry, the location type <', tpe, '> is not implemented for this feature.')
        return(NULL)
    }
    y <- NULL
    for(id in ids$ids){
        cid <- if(ids$type %in% c('MSOA', 'LAD', 'PCON', 'PCD')){ id } else { c(NA, id) }
        y <- rbindlist(list(
                y,
                read_fst_idx(
                    file.path(geouk_path, paste0('postcodes', ids$fname)),
                    cid,
                    c('postcode', 'is_active', 'x_lon', 'y_lat')
                )
        ))
    }
    if(active_only) y <- y[is_active == 1, -c('is_active')]
    y
}


#' Return a map of the set of postcodes included in a bounding box or circle with a specified postcode unit as centroid
#'
#' @param id The ONS code of the location we want the map from
#' @param active_only If TRUE, only active postcodes are mapped.
#' If FALSE, all postcodes are mapped, divided in two separate sub-maps as <active> and <terminated>
#'
#' @return a leaflet map
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import leaflet
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
#'
map_postcodes_area <- function(ids,
                            active_only = FALSE, ch = TRUE, oa = TRUE,
                            tiles = tiles.lst,
                            use_icons = FALSE
                            # active:   radius, weight, color, fcolor, fopacity
                            # term:     radius, weight, color, fcolor
                            # ch poly: weight, color, fcolor, fopacity
                            # oa poly: weight, color, fcolor, fopacity
                        ){
    ids <- toupper(ids)
    pcs <- get_postcodes_area(ids, active_only)
    if(is.null(pcs)) stop('The code provided is not valid.')
    mp <- basemap(bbox = data.frame(c(min(pcs$x_lon), min(pcs$y_lat)), c(max(pcs$x_lon), max(pcs$y_lat))), tiles = tiles) %>%
            addMapPane('polygons', zIndex = 410) %>%
            addMapPane('points', zIndex = 420)

    grps <- 'Postcodes (active)'
    y <- if(active_only){ pcs } else { pcs[is_active == 1] }
    mp <- mp %>%
        addCircles(
            data = y,
            lng = ~x_lon, lat = ~y_lat,
            group = grps,
            radius = 8,
            weight = 2,
            color = 'darkgreen',
            opacity = 1,
            fillColor = 'green',
            fillOpacity = 0.5,
            label = ~postcode,
            options = pathOptions(pane = 'points')
        )
    if(ch){
        grps <- c('Concave Hull', grps)
        bnd <- bnd_merge(ids, file.path(bnduk_path, 'postcodes', 'ch'))
        mp <- mp %>%
            addPolygons(
                data = bnd,
                group = grps[1],
                color = 'orange',
                options = pathOptions(pane = 'polygons')
            )
    }
    if(oa){
        grps <- c('Output Areas', grps)
        bnd <- bnd_merge(ids, file.path(bnduk_path, 'postcodes', 'oa'))
        mp <- mp %>%
            addPolygons(
                data = bnd,
                group = grps[1],
                smoothFactor = 2,
                stroke = TRUE,
                dashArray = '1 1 2',
                color = 'gray',
                options = pathOptions(pane = 'polygons')
            )
    }
    if(!active_only){
        y <- pcs[is_active == 0]
        if(nrow(y) > 0){
            grps <- c(grps, 'Postcodes (terminated)')
            mp <- mp %>%
                addCircles(
                    data = y,
                    lng = ~x_lon, lat = ~y_lat,
                    group = grps[4],
                    radius = 8,
                    stroke = TRUE,
                    weight = 2,
                    color = 'darkred',
                    opacity = 1,
                    fillColor = 'red',
                    fillOpacity = 0.5,
                    label = ~postcode,
                    options = pathOptions(pane = 'points')
                )
        }
    }

    mp <- mp %>%
        addLayersControl( baseGroups = names(tiles.lst), overlayGroups = grps )

    # mp <- mp %>%
    # 	addLegendFixedCustom(
    #         colors = c('orange', 'lightblue'),
    #         labels = c('Big Chains', 'Other Shops'),
    # 		opacity = 1,
    #         title = '',
    #         position = 'bottomright',
    # 	)

    mp

}


#' Return a map of a location passed using a string instead of a code
#'
#' @param x
#' @param tpe
#' @param exact
#' @param ... accept all parameters used in <map_postcodes_area> apart from the id
#'
#' @return a leaflet map
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import leaflet
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
#'
map_postcodes_areaname <- function(x, tpe = NA, exact = FALSE, ...){
    y <- get_area_code(x, tpe, exact)
    if(nrow(y) > 1){
        message('
            Multiple locations found.
            You must narrow your search or include a location type in the call.
        ')
        return(y)
    }
    if(nrow(y) == 0){
        message('No location found.')
        return(NULL)
    }
    map_postcodes_area(y$location_id, ...)
}
