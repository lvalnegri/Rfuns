#' Check a character string is a UK valid postcode, then convert it to a 7-char format
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
#' @export
#'
clean_postcode <- function(dt, cname = 'PCU'){
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
#' @export
#'
clean_postcode_dt <- function(dt, cname = 'PCU'){
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
                pc <- clean_postcode(data.table(PCU = postcode))
                pcs <- read_fst_idx(
                            file.path(geouk_path, 'postcodes_pcds'),
                            c(NA, substr(postcode, 1, 5)),
                            c('PCU', 'x_lon', 'y_lat')
                )
                pcs[PCU == pc$PCU]
}

#' Add to a dataset with a postcodes column the corresponding geographic coordinates
#'
#' @param x a data.table
#' @param cname the column containing the postcodes
#'
#' @return a data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table fst
#'
#' @export
#'
add_postcodes_coords <- function(x, cname = 'PCU'){
    xn <- names(x)
    x <- clean_postcode_dt(x, cname)
    pc <- read_fst(file.path(geouk_path, 'postcodes'), columns = c('PCU', 'x_lon', 'y_lat'), as.data.table = TRUE)
    x <- pc[x, on = c('PCU' = cname)]
    setnames(x, 'PCU', cname)
    setcolorder(x, xn)
    reorder_columns(x, c('x_lon', 'y_lat'), cname)
    x
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
#' @importFrom raster pointDistance
#'
#' @export
#'
get_postcode_neighbors <- function(postcode, distance = 0.5, circle = TRUE, in.miles = TRUE, active_only = TRUE){
    pc <- get_postcode_coords(postcode)
    yb <- bounding_box(pc$x_lon, pc$y_lat, distance, in.miles)
    cols <- c('PCU', 'x_lon', 'y_lat', 'is_active')
    pcs <- read_fst(file.path(geouk_path, 'postcodes'), columns = cols, as.data.table = TRUE)
    if(active_only) pcs <- pcs[is_active == 1, -c('is_active')]
    pcs <- pcs[ x_lon >= yb[1, 1] & x_lon <= yb[1, 2] & y_lat >= yb[2, 1] & y_lat <= yb[2, 2] ]
    if(circle){
        setorder(pcs, 'PCU')
        y <- data.table(PCU = pcs$PCU, dist = pointDistance(pcs[, 2:3], pc[, 2:3], lonlat = TRUE))
        y <- y[dist > distance * ifelse(in.miles, 1609.34, 1000), PCU]
        pcs <- pcs[!PCU %in% y]
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
#' @export
#'
map_postcode_neighbors <- function(postcode,
                                    distance = 0.5, circle = TRUE, in.miles = TRUE, active_only = FALSE,
                                    tiles = tiles.lst, use_icons = FALSE
                                    # active: radius, weight, color, fcolor, fopacity
                                    # term:   radius, weight, color, fcolor
                        ){
    pc <- get_postcode_neighbors(postcode, distance, circle, in.miles, active_only)
    mp <- basemap(viewport = data.frame(c(min(pc$x_lon), min(pc$y_lat)), c(max(pc$x_lon), max(pc$y_lat))), tiles = tiles)
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
                label = ~PCU
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
                    label = ~PCU
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
                    label = ~PCU
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
                label = ~PCU
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
#' @importFrom raster pointDistance
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
                    c('PCU', 'is_active', 'x_lon', 'y_lat')
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
    mp <- basemap(viewport = data.frame(c(min(pcs$x_lon), min(pcs$y_lat)), c(max(pcs$x_lon), max(pcs$y_lat))), tiles = tiles) %>%
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
            label = ~PCU,
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
                    label = ~PCU,
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
#' @export
#'
map_postcodes_areaname <- function(x, tpe = NA, exact = FALSE, ...){
    y <- get_area_code(x, tpe, exact)
    if(nrow(y) > 1){
        message('
            Multiple locations found.
            You must narrow your search, or include a suitable location type in the call.
        ')
        return(y)
    }
    if(nrow(y) == 0){
        message('No location found.')
        return(NULL)
    }
    map_postcodes_area(y$location_id, ...)
}


#' Return the code mapping between postcodes and a shapefile boundaries online (from the ONS geoportal) referring to a specified location type
#'
#' @param furl the url of the shapefile
#' @param onsid the name of the column in the shapefile that represents the
#' @param tpe
#'
#' @return a data.table with two columns, postcode and location id
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table fst sf
#'
#' @importFrom dplyr select filter rename arrange 
#'
#' @export
#'
lookup_postcodes_shp <- function (
                            furl,
                            onsid,
                            tpe,
                            save_names = NULL,
                            npath = file.path(ext_path, 'uk', 'geography', 'locations'),
                            bpath = file.path(ext_path, 'uk', 'geography', 'boundaries')
){

    zfile <- tempfile()

    message('Downloading file...')
    download.file(furl, zfile)

    message('Extracting shapefiles...')
    unzip(zfile, exdir = tmp_path)
    yn <- setDT(unzip(zfile, exdir = tmp_path, list = TRUE))[, Name]
    yn <- gsub('.shp$', '', yn[grepl('.shp$', yn)])

    message('Reading boundaries...')
    bnd <- st_read(tmp_path, gsub('.shp', '', yn))
    if(!is.null(bpath)){
        message('Saving original boundaries...')
        save_bnd(bnd, yn, rds = FALSE, pct = NULL, bpath = bpath)
    }

    if(!is.null(save_names)){
        message('Saving dataframe with codes and names...')
        y <- setDT(bnd %>% st_drop_geometry() %>% select(onsid, save_names))
        setnames(y, c('X', 'Y'))
        y[, Y := gsub(paste0(' ?', tpe), '', Y)]
        setnames(y, paste0(tpe, c('', 'n')))
        setcolorder(y, 1)
        fwrite(y, file.path(npath, paste0(tpe, '.csv')))
    }

    message('Transforming coordinates...')
    bnd <- bnd %>% st_transform(4326)

    message('Cleaning and sorting data slot...')
    bnd <- bnd %>% select(onsid) %>% rename(!!tpe := onsid) %>% arrange(tpe)
    if(!is.null(bpath)){
        message('Saving transformed boundaries as shapefile...')
        save_bnd(bnd, tpe, rds = FALSE, pct = NULL, bpath = bpath)
    }

    message('Reading geographical postcodes file...')
    pc <- readRDS(file.path(geouk_path, 'postcodes.sp'))

    message('Performing Points In Polygons...')
    y <- data.table('PCU' = pc$PCU, over(pc, as_Spatial(bnd)) )

    message('Cleaning...')
    unlink(zfile)
    file.remove(file.path(tmp_path, yn))

    y

}

#' Find overlapping areas created when merging children areas using the postcodes table. If asked, updates the table itself
#'
#' @param parent The location type of the higher level area
#' @param child The location type of the lower level area
#' @param dts If TRUE, it return the postcodes tables
#' @param update If TRUE, it updates the saved or passed postcodes table, then returns it.
#'               Otherwise, it returns the table of overlapping children
#'
#' @return A data table of overlapping areas, or the updated postcodes table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @export
#'
get_areas_ovelapping <- function(parent, child, dts = NULL, update = FALSE){

    if(is.null(dts)){
        message('Reading data...')
        y <- read_fst_idx(file.path(geouk_path, 'postcodes'), 1, c('PCU', parent, child))
    } else {
        message('Filtering data...')
        y <- dts[is_active == 1, .(PCU, get(parent), get(child))]
    }
    setnames(y, c('PCU', 'P', 'C'))

    message('Finding overlapping...')
    y <- y[!is.na(P), .N, .(P, C)]
    dp <- y[, .N, .(P, C)][, .N, C][N > 1][order(-N, C)]
    y <- y[C %in% dp$C][order(C, -N)]

    if(update){

        message('Updating postcodes table...')
        y <- y[y[, .I[which.max(N)], C]$V1][, N := NULL]
        if(is.null(dts)) dts <- read_fst(file.path(geouk_path, 'postcodes'), as.data.table = TRUE)
        yn <- names(dts)
        setnames(dts, c(parent, child), c('P', 'C'))
        y <- rbindlist(list(
                    dts[!C %in% y$C, .(PCU, P, C)],
                    y[dts[C %in% y$C, .(PCU, C)], on = 'C']
                ), use.names = TRUE
        )
        dts[, `:=`( P = NULL, C = NULL )]
        dts <- y[dts, on = 'PCU']
        setnames(dts, c('P', 'C'), c(parent, child))
        setcolorder(dts, yn)
        return(dts)

    } else {

        setnames(y, c(parent, child, 'N'))
        return(y)

    }

}

#' Save the postcodes table in multiple files in fst format, each with its own index
#'
#' @param dts the dataset to be saved
#' @param pfn if TRUE, it saves also a version with index over PFA+PFN
#'
#' @return None
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
save_postcodes <- function(dts, pfn = FALSE){
    fnames <- list(
        'postcodes' = c('is_active', 'LSOA'),
        'postcodes_msls' = c('MSOA', 'LSOA'),
        'postcodes_pcoa' = c('PCON', 'OA'),
        'postcodes_ldwd' = c('LAD', 'WARD'),
        'postcodes_ldpr' = c('LAD', 'PAR'),
        'postcodes_pcds' = c('PCD', 'PCS'),
        'postcodes_pcat' = c('PCA', 'PCT')
    )
    if(pfn) fnames <- append(fnames, list('postcodes_pfan' = c('PFA', 'PFN')))
    save_multi_idx(dts, fnames, geouk_path)
}


#' Find duplications in geographies hierarchy inside the postcodes table
#'
#' Examine the postcodes table over two locations types allegedly in a hierarchy, the first being the parent, the second the child
#'
#' @param parent the bigger area type
#' @param child the smaller area type
#' @param subset_only if TRUE, returns only the codes without the counts of postcodes for each parent-child pair
#'
#' @return a list with two data.tables:
#'  - "dups" with all the ...
#'  - "overs" with ...
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @export
#'
get_postcodes_dup <- function(parent, child, subst_only = FALSE){

    # check if the two location types are correct
    y <- read_fst(file.path(geouk_path, 'postcodes'), to = 1)
    if(!parent %in% names(y)) stop('Parent does not exist')
    if(!child %in% names(y)) stop('Child does not exist')

    # read the postcode file for active postcodes only
    y <- read_fst_idx(file.path(geouk_path, 'postcodes'), 1, cols = c(parent, child))
    setnames(y, c('P', 'C'))

    # count the postcodes and determine dups and overs
    y <- y[, .N, .(P, C)]
    dp <- unique(y[!is.na(P), .(P, C)])[, .N, C][N > 1][order(-N, C)]
    y <- y[C %in% dp$C][order(C, -N)]

    # drop the count if asked
    if(subst_only) y <- y[y[, .I[which.max(N)], C]$V1]

    # adjust the structure of the tables
    setnames(dp, c(child, 'N'))
    setnames(y, c(parent, child, 'N'))
    setcolorder(y, child)

    # return the above tables as a list
    list('dups' = dp, 'overs' = y)

}

