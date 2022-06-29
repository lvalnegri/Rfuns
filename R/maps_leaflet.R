#' registerPlugin
#' 
#' Helper function to register plugins in `leaflet` maps
#'
#' @export
#' 
registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
}


#' faPlugin
#' 
#' Defines a dependency to allow the use of *fontawesome pro* icons pro in `leaflet` maps
#'
#' @references \url{https://fontawesome.com/v6/docs/web/style/styling}. 
#'
#' @importFrom htmltools htmlDependency
#'
#' @export
#' 
faPlugin <- htmlDependency(
    name = 'font-awesome',
    version = '99.0',
    src = c(href = 'https://datamaps.uk/assets/datamaps/icons/fontawesome'),
    stylesheet = 'css/all.css'
)


#' tiles.lst
#' 
#' List of background tiles for `leaflet` maps
#'
#' @references \url{https://leaflet-extras.github.io/leaflet-providers/preview/}. 
#' 
#' To work correctly, needs to be paired with `add_maptile`.
#'
#' @export
#' 
tiles.lst <- list(
    'Google Maps Standard' = 'https://{s}.google.com/vt/lyrs=m&x={x}&y={y}&z={z}&hl=it',
    'Google Maps Satellite' = 'https://{s}.google.com/vt/lyrs=s&x={x}&y={y}&z={z}&hl=it',
    'Google Maps Terreno' = 'https://{s}.google.com/vt/lyrs=p&x={x}&y={y}&z={z}&hl=it',
    'Google Maps Alternativo' = 'https://{s}.google.com/vt/lyrs=r&x={x}&y={y}&z={z}&hl=it',
    'Google Maps Solo Strade' = 'https://{s}.google.com/vt/lyrs=h&x={x}&y={y}&z={z}&hl=it',
    'OSM Mapnik' = 'OpenStreetMap.Mapnik',
    'OSM HOT' = 'OpenStreetMap.HOT',
    'OSM Topo' = 'OpenTopoMap',
    'OSM Cycle' = 'https://{s}.tile-cyclosm.openstreetmap.fr/cyclosm/{z}/{x}/{y}.png',
    'Stamen Toner' = 'Stamen.Toner',
    'Stamen Toner Lite' = 'Stamen.TonerLite',
    'Stamen Toner Background' = 'Stamen.TonerBackground',
    'Stamen Terrain' = 'Stamen.Terrain',
    'Stamen Watercolor' = 'Stamen.Watercolor',
    'Esri Street' = 'Esri.WorldStreetMap',
    'Esri Topo' = 'Esri.WorldTopoMap',
    'Esri Imagery' = 'Esri.WorldImagery',
    'CartoDB Voyager' = 'CartoDB.Voyager',
    'CartoDB Positron' = 'CartoDB.Positron',
    'CartoDB Dark Matter' = 'CartoDB.DarkMatter',
    'OPNVKarte' = 'https://tileserver.memomaps.de/tilegen/{z}/{x}/{y}.png',
    'Hike Bike' = 'HikeBike.HikeBike',
    'Mtb' = 'MtbMap'
)

#' lbl.options
#' 
#' Generic options for the labels appearing when hovering polygons in `leaflet` maps
#'
#' @importFrom leaflet labelOptions
#' 
#' @export
#' 
lbl.options <- labelOptions(
    nohide = TRUE,
    textsize = '12px',
    direction = 'right',
    sticky = FALSE,
    opacity = 0.8,
    offset = c(10, -10),
    style = list(
        'color' = 'black',
        'border-color' = 'rgba(0,0,0,0.5)',
        'font-family' = 'verdana',
        'font-style' = 'normal',
        'font-size' = '14px',
        'font-weight' = 'normal',
        'padding' = '2px 6px',
        'box-shadow' = '3px 3px rgba(0,0,0,0.25)'
    )
)


#' hlt.options
#' 
#' Generic options for the border highlight when hovering polygons in `leaflet` maps
#'
#' @importFrom leaflet highlightOptions
#' 
#' @export
#' 
hlt.options <- highlightOptions(
    weight = 6,
    color = 'white',
    opacity = 1,
    bringToFront = TRUE,
    sendToBack = TRUE
)


#' mrkr_cls_lims
#' 
#' Writes `JS` function to redefine *Marker Clusters* in `leaflet` maps using five bins
#'
#' @param n proportional factor to alter the four included fixed breaks: 0, 20, 100, 200
#'
#' @references \url{https://github.com/Leaflet/Leaflet.markercluster}. 
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @importFrom leaflet markerClusterOptions
#' @importFrom htmlwidgets JS
#'
#' @export
#' 
mrkr_cls_lims <- function(n = 1){
    markerClusterOptions(
        iconCreateFunction = JS(
            paste0("
                function (cluster) {    
                    var childCount = cluster.getChildCount(); 
                    var c = ' marker-custom-';  
                    if (childCount < ", n * 20, ") {  
                      c += 'small';  
                    } else if (childCount < ", n * 50, ") {  
                      c += 'smlmed';  
                    } else if (childCount < ", n * 100, ") {  
                      c += 'medium';  
                    } else if (childCount < ", n * 200, ") {  
                      c += 'medlrg';  
                    } else { 
                      c += 'large';  
                    }    
                    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
                }"
            )
        )
    )
}


#' marker_colours
#' 
#' List of the available colours to fill *Awesome Markers* in `leaflet` maps
#'
#' @references \url{https://rstudio.github.io/leaflet/markers.html}
#' 
#' @export
marker_colours <- c(
    'rosso' = 'red', 'rosso scuro' = 'darkred', 'arancione' = 'orange', 'rosa' = 'pink', 'beige' = 'beige', 'verde' = 'green', 
    'verde scuro' = 'darkgreen', 'verde chiaro' = 'lightgreen', 'blu' = 'blue', 'blu chiaro' = 'lightblue', 'blu cadetto' = 'cadetblue', 
    'viola' = 'purple', 'bianco' = 'white', 'grigio chiaro' = 'lightgray', 'grigio' = 'gray', 'nero' = 'black'
)


#' add_maptile
#' 
#' Add a background layer to a \code{leaflet} map
#'
#' @param m a `leaflet` object
#' @param x text or url description of the required maptile (see \code{tiles.lst})
#' @param grp an optional group name for the layer
#'
#' @return A `leaflet` object
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @importFrom leaflet addProviderTiles addTiles
#' 
#' @export
#'
add_maptile <- function(m, x, grp = NULL){
    switch(stringr::str_extract(x, 'google|memomaps|cycl'),
        'google' = m |> addTiles(
                            urlTemplate = x, 
                            attribution = 'Map data &copy; <a href="https://maps.google.com/">Google Maps</a>', 
                            options = tileOptions(subdomains = c('mt0', 'mt1', 'mt2', 'mt3'), useCache = TRUE, crossOrigin = TRUE),
                            group = grp
                ),
        'memomaps' = m |> addTiles(
                        urlTemplate = x, 
                        attribution = 'Map <a href="https://memomaps.de/">memomaps.de</a> <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, map data &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
                        options = tileOptions(useCache = TRUE, crossOrigin = TRUE),
                        group = grp    
                ),
        'cycl' = m |> addTiles(
                        urlTemplate = 'https://{s}.tile-cyclosm.openstreetmap.fr/cyclosm/{z}/{x}/{y}.png', 
                        attribution = '<a href="https://github.com/cyclosm/cyclosm-cartocss-style/releases" title="CyclOSM - Open Bicycle render">CyclOSM</a> | Map data: &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
                        options = tileOptions(useCache = TRUE, crossOrigin = TRUE),
                        group = grp
                ),
        `NA` = m |> addProviderTiles(x, group = grp, options = providerTileOptions(useCache = TRUE, crossOrigin = TRUE))
    )
}


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

