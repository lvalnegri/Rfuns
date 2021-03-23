#' A leaflet map, empty or with points and/or polygons layer, possibly styled agreeing with a numeric variable
#'
#' @param coords Center the map around this long/lat point (default is UK centroid)
#' @param bbox Alternative to <coords>, set the bounds of a map. In a 2x2 matrix form, long as first row, lat as second row; min as first  column, max as second column
#' @param zm The admissible minimum zoom. Default is 6, which is the minimum for the UK to be visible as a whole
#' @param tiles List of map backgroud tiles, by default all layers listed in tiles.lst. Pass a NULL value to add only the default tile.
#' @param pnts a dataframe of locations, with longitude named 'x_lon' and latitutde named 'y_lat'
#' @param pntsid the name of the column in the points dataframe to be queried for the points labels (NA for no label)
#' @param pradius if num the (equal) radius of each point, if char the (numeric) column where query the radius for each pont
#' @param pcolor
#' @param popacity
#' @param pfcolor
#' @param pfopacity
#' @param bnd a SpatialPolygonsDataFrame
#' @param bndid the name of the column in the data slots of the SpatialPolygons to be queried for the polygons labels (NA for no label)
#' @param bcolor
#' @param bweight
#' @param bopacity
#' @param bfcolor
#' @param bfopacity
#' @param hltopt if TRUE, highlight the polygon when hovering, using default style
#' @param hlt.options set of options to style the polygons when hovering
#' @param lbl.options set of options for the polygons labels
#' @param menu logical for the menu choosing the tiles
#' @param add_pb_menu logical for adding both polygons and points to the menu
#' @param output
#' @param verbose
#' @param extra plugins options to add to the map: 'search', 'reset', 'full', 'print', 'mini', 'scale'
#'
#' @return a leaflet map or a HTML file
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import leaflet leaflet.extras leaflet.extras2 sf
#'
#' @export
#'
basemap <- function(
            center = NULL, viewport = NULL, zm = 5, tiles = tiles.lst,
            pnts = NULL, pntsid = 'id',
                pradius = 8, pcolor = 'black', pweight = 1, popacity = 0.6, pfcolor = 'red', pfopacity = 0.6,
#                pvar = '', ppal = '',
            bnd = NULL, bndid = 'id',
                bcolor = 'red', bweight = 0.6, bopacity = 0.8, bfcolor = 'orange', bfopacity = 0.6,
                hltopt = TRUE, lblopt = lbl.options,
#                bmethod = 'exact',  bpal = '',
            menu = TRUE, add_pb_menu = FALSE,
            output = 'map',
            verbose = FALSE,
            extras = c('search', 'reset', 'full', 'mini', 'scale', 'print')
    ){

        # starts with
        mp <- leaflet(options = leafletOptions(minZoom = zm)) %>% enableTileCaching()
        if('search' %in% extras) mp <- mp %>% addSearchOSM()
        if('reset' %in% extras)  mp <- mp %>% addResetMapButton()
        if('full' %in% extras)   mp <- mp %>% addFullscreenControl()
        if('mini' %in% extras)   mp <- mp %>% addMiniMap(width = 100, height = 80, zoomLevelOffset = -4, toggleDisplay = TRUE)
        if('scale' %in% extras)  mp <- mp %>% addScaleBar(position = 'bottomleft')
        if('print' %in% extras)  mp <- mp %>% addEasyprint()
        if('draw' %in% extras)
            mp <- mp %>%
                addDrawToolbar(targetGroup = 'Polygons', editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
                addStyleEditor()


        # check map center or viewport
        vp <- FALSE
        if(!is.null(center)){
            if(is.numeric(center)){
                center <- center[1:2]
                if(sum(center >= -180 & center <= 180) == 2){
                    mp <- mp %>% setView(center[1], center[2], zoom = zm)
                    vp <- TRUE
                }
            }
        }
        if(!vp){
            if(!is.null(viewport)){
                if(is.numeric(viewport)){
                    viewport <- viewport[1:2, 1:2]
                    if(sum(viewport >= -180 & viewport <= 180) == 4){
                        mp <- mp %>% fitBounds(viewport[1, 1], viewport[2, 1], viewport[1, 2], viewport[2, 2])
                        vp <- TRUE
                    }
                }
            }
            if(!vp){
                if(is.null(pnts) & is.null(bnd)){
                    mp <- mp %>% setView(centers.uk[['UK']][1], centers.uk[['UK']][2], zoom = zm)
                    vp <- TRUE
                } else if(is.null(bnd)) {
                    viewport <- st_bbox(pnts)
                } else if(is.null(pnts)) {
                    viewport <- st_bbox(bnd)
                } else {
                    viewport <- pmax(st_bbox(pnts), st_bbox(bnd))
                }
                if(!vp) mp <- mp %>% fitBounds(viewport[1, 1], viewport[2, 1], viewport[1, 2], viewport[2, 2])
            }
        }

        # add background tiles
        if(is.null(tiles)){
            mp <- mp %>% addTiles()
        } else {
            for(idx in 1:length(tiles))
                mp <- mp %>%
                    addProviderTiles(
                        tiles[[idx]],
                        group = names(tiles)[idx],
                        options = providerTileOptions(useCache = TRUE, crossOrigin = TRUE)
                    ) %>%
                    showGroup(tiles[1])
        }

        # add PLACES - points
        if(!is.null(pnts)){
            yn <- names(pnts)
            if(sum(grepl(pntsid, yn)) == 0){
                warning('Column not found for labels')
                pntsid <- NA
            }
            if(sum(grepl('^(lon|lng|x_lon)', yn)) == 0) stop('Longitude not found for points')
            xc <- yn[grepl('^(lon|lng|x_lon)', yn)][1]
            if(sum(grepl('^(lat|y_lat)', yn)) == 0) stop('Latitude not found for points')
            yc <- yn[grepl('^(lat|y_lat)', yn)][1]
            mp <- mp %>%
                addMapPane('Points', zIndex = 400) %>%
                addCircleMarkers(
                    data = pnts,
                    lng = ~eval(parse(text = xc)),
                    lat = ~eval(parse(text = yc)),
                    group = 'Points',
                    radius = pradius,
                    color = pcolor,
                    weight = pweight,
                    opacity = popacity,
                    stroke = TRUE,
                    fillColor = pfcolor,
                    fillOpacity = pfopacity,
                    label = if(is.na(pntsid)) NULL else ~eval(parse(text = pntsid)),
                    labelOptions = lblopt,
                    options = pathOptions(pane = 'Points')
                )
        }

        # add AREAS - polygons
        if(!is.null(bnd)){
            if(sum(grepl(bndid, names(bnd@data))) == 0) bndid <- NA
            mp <- mp %>%
                addMapPane('Polygons', zIndex = 420) %>%
                addPolygons(
                    data = bnd,
                    group = 'Polygons',
                    color = bcolor,
                    weight = bweight,
                    opacity = bopacity,
                    fillColor = bfcolor,
                    fillOpacity = bfopacity,
                    smoothFactor = 0.2,
                    highlightOptions = if(hltopt) hlt.options else NULL,
                    label = if(is.na(bndid)) NULL else ~eval(parse(text = bndid)),
                    labelOptions = lblopt,
                    options = pathOptions(pane = 'Polygons')
                )
        }

        # add control menu
        if(menu){
            if(add_pb_menu & !is.null(pnts) & !is.null(bnd)){
                mp <- mp %>%
                    addLayersControl(
                        baseGroups = names(tiles),
                        overlayGroups = c('Points', 'Polygons')
                    )
            } else {
                mp <- mp %>% addLayersControl(baseGroups = names(tiles))
            }
        }

        mp

}

#' Provides a mapping between numeric data values and colours, according to specified classification method and palette
#'
#' @param x
#' @param cls_mth
#' @param n_brks
#' @param fxd_brks
#' @param use_palette
#' @param br_pal
#' @param fxd_cols
#' @param rev_cols
#' @param add_legend
#' @param dec.fig
#' @param bsep
#' @param pct_sign
#' @param del_signs
#'
#' @return a character vector with the RGB string describing the colours for the required palette
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table classInt RColorBrewer
#'
#' @export
#'
get_palette <- function(X,
                        cls_mth = 'quantile',
                        n_brks = 7,
                        fxd_brks = NULL,
                        use_palette = TRUE,
                        br_pal = 'Greys',
                        fxd_cols = c('#ff0000', '#ffff00', '#00ff00'),
                        rev_cols = FALSE,
                        add_legend = TRUE,
                        dec.fig = 0,
                        bsep = ',',
                        pct_sign  = FALSE,
                        del_signs = TRUE
    ){

    if(!cls_mth %in% c('fixed', 'equal', 'quantile', 'pretty', 'jenks', 'hclust', 'kmeans')){
        warning('The provided method does not exist! Reverting to "quantile"')
        cls_mth <- 'quantile'
    }

    if(cls_mth == 'fixed'){
        if(is.null(fxd_brks)) stop('When asking for the "fixed" method, you have to include a vector of convenient bin limits.')
        if(!is.numeric(fxd_brks)) stop('The vector containing the bin limits must be numeric.')
        fxd_brks <- sort(fxd_brks)
        mX <- min(X, na.rm = TRUE)
        MX <- max(X, na.rm = TRUE)
        if(MX > max(fxd_brks)) fxd_brks <- c(fxd_brks, MX)
        if(mX < min(fxd_brks)) fxd_brks <- c(mX, fxd_brks)
        n_brks <- length(fxd_brks) - 1
    }

    brks_poly <-
        if(cls_mth == 'fixed'){
            classIntervals(X, n = n_brks, style = 'fixed', fixedBreaks = fxd_brks)
        } else {
            classIntervals(X, n = n_brks, style = cls_mth)
        }

    # Determine the color palette
    if(use_palette){
        if(!br_pal %in% rownames(brewer.pal.info)){
            warning('The provided palette does not exist! Reverting to "Greys"')
            br_pal <- 'Greys'
        }
        col_codes <-
            if(n_brks > brewer.pal.info[br_pal, 'maxcolors']){
                colorRampPalette(brewer.pal(brewer.pal.info[br_pal, 'maxcolors'], br_pal))(n_brks)
            } else {
                brewer.pal(n_brks, br_pal)
            }
        if(rev_cols) col_codes <- rev(col_codes)
    } else {
        col_codes <- colorRampPalette(fxd_cols)(n_brks)
    }

    # return a list
    if(add_legend){
        list(
            findColours(brks_poly, col_codes), brks_poly, col_codes,
            get_map_legend(X, brks_poly$brks, dec.fig, del_signs)
        )
    } else {
        list(findColours(brks_poly, col_codes), brks_poly, col_codes)
    }
}

#' Build a legend to be used in a leaflet thematic map
#'
#' @param mtc the metric used in the choropleth
#' @param brks a vector that describes the $(n + 1)$ breaks that build the $n$ bins
#' @param dec.fig the number of decimal to keep
#' @param del_signs if TRUE, drop $+/-$ from numbers
#'
#' @return a character vector
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#' @importFrom stringr str_pad
#'
#' @export
#'
get_map_legend <- function(mtc, brks, dec.fig = 2, del_signs = TRUE) {
    lbl <- get_fxb_labels(brks, dec.fig, del_signs)
    brks <- sort(as.numeric(unique(c(lbl$lim_inf, lbl$lim_sup))))
    mtc <- data.table('value' = mtc)
    mtc <- mtc[, .N, value][!is.na(value)]
    mtc[, label := cut(value, brks, lbl$label, ordered = TRUE)]
    mtc <- mtc[, .(N = sum(N)), label][order(label)][!is.na(label)]
    mtc <- mtc[lbl[, .(label)], on = 'label'][is.na(N), N := 0]
    mtc[, N := format(N, big.mark = ',')]
    ypad <- max(nchar(as.character(mtc$N))) + 3
    mtc[, label := paste0(label, str_pad(paste0(' (', N, ')'), ypad, 'left'))]
    mtc$label
}

#' Determines the labels for n bins given a series of (n + 1) breaks
#'
#'
#' @param y a vector describing the limits of the bins
#' @param dec.fig the number of decimal to keep
#' @param del_signs if TRUE, drop +/- from numbers
#'
#' @return a data.table with limits and labels
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#' @importFrom stringr str_pad
#'
get_fxb_labels <- function(y, dec.fig = 1, del_signs = TRUE){
    y <- gsub('^ *|(?<= ) | *$', '', gsub('(?!\\+|-|\\.)[[:alpha:][:punct:]]', ' ', y, perl = TRUE), perl = TRUE)
    y <- paste(y, collapse = ' ')
    if(del_signs){
        y <- gsub('*\\+', Inf, y)
        y <- gsub('*\\-', -Inf, y)
    }
    y <- unique(sort(as.numeric(unlist(strsplit(y, split = ' ')))))
    lbl_brks <- format(round(y, 3), nsmall = dec.fig)
    lbl_brks <- str_pad(lbl_brks, max(nchar(lbl_brks)), 'left')
    y <- data.table(
        'lim_inf' = lbl_brks[1:(length(lbl_brks) - 1)],
        'lim_sup' = lbl_brks[2:length(lbl_brks)],
        'label' = sapply(1:(length(lbl_brks) - 1), function(x) paste0(lbl_brks[x], ' ─ ', lbl_brks[x + 1]))
    )

}
