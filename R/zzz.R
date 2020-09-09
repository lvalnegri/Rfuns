#' Top root of the shared repository in every datamaps VPS
#'
#' @export
pub_path <- Sys.getenv('PUB_PATH')

#' Root of any external source data
#'
#' @export
ext_path <- file.path(pub_path, 'ext_data')

#' Root of source data for any Shiny app
#'
#' @export
app_path <- file.path(pub_path, 'shiny_apps')

#' Location of the boundaries for the UK in the shared repository
#'
#' @export
bnduk_path <- file.path(pub_path, 'boundaries', 'uk')

#' Location of the boundaries of the UK, in the shared repository, tipycally used in R applications (= rds format)
#'
#' @export
bnduk_spath <- file.path(bnduk_path, 'rds', 's20')

#' Location of the UK datasets in the shared repository
#'
#' @export
datauk_path <- file.path(pub_path, 'datasets', 'uk')

#' Location of the geographic UK datasets in the shared repository
#'
#' @export
geouk_path <- file.path(datauk_path, 'geography')

#' Location of the Italian boundaries in the shared repository
#'
#' @export
bndit_path <- file.path(pub_path, 'boundaries', 'it')

#' Location of the Italian boundaries, in the shared repository, tipycally used in R applications (= rds format)
#'
#' @export
bndit_spath <- file.path(bndit_path, 'rds', 's05')

#' Location of the Italian datasets in the shared repository
#'
#' @export
datait_path <- file.path(pub_path, 'datasets', 'it')

#' Location of the geographic Italian datasets in the shared repository
#'
#' @export
geoit_path <- file.path(datait_path, 'geography')


#' Coordinate Reference Systems (CRS) in EPSG code for OSGB 1936 / OSGB 1936 / Airy 1830 / British National Grid
#'
#' Coordinate system: Cartesian 2D CS
#' Axes: easting, northing (E,N)
#' proj4js: "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs";
#' Bounds: [WGS84] ; [Projected]
#' Centroid: [WGS84] ; [Projected]
#' Unit: metre
#' Area of use: Ireland - onshore.
#' Accuracy: 1m
#'
#' sp::CRS(crs_code) to see the proj4js format for the specified epsg code
#' rgdal::make_EPSG() to build a table with code, name and proj4js format for all epsg codes
#'
#' @export
crs.gb <- '+init=epsg:27700'

#' Coordinate Reference Systems (CRS) in EPSG code for TM65 / TM65 / Airy Modified 1849 / Irish Grid
#'
#' Coordinate system: Cartesian 2D CS
#' Axes: easting, northing (E,N)
#' proj4js: "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +a=6377340.189 +b=6356034.447938534 +units=m +no_defs"
#' Bounds: [WGS84] ; [Projected]
#' Centroid: [WGS84] ; [Projected]
#' Unit: metre
#' Area of use: Ireland - onshore
#' Accuracy: 1m
#'
#' sp::CRS(crs_code) to see the proj4js format for the specified epsg code
#' rgdal::make_EPSG() to build a table with code, name and proj4js format for all epsg codes
#'
#' @export
crs.ni = '+init=epsg:29902'

#' Coordinate Reference Systems (CRS) in EPSG code for WGS84 / World Geodetic System 1984 / WGS84 / GPS Worlwide
#'
#' Coordinate system: Ellipsoidal 2D CS
#' Axes: latitude, longitude
#' proj4js: "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0";
#' Bounds: NA
#' Centroid: NA
#' Unit: degree
#' Area of use: World
#' Accuracy: NA
#'
#' sp::CRS(crs_code) to see the proj4js format for the specified epsg code
#' rgdal::make_EPSG() to build a table with code, name and proj4js format for all epsg codes
#'
#' @export
crs.wgs <- '+init=epsg:4326'  #

#' List of background tiles for leaflet maps
#'
#' For the complete list see https://leaflet-extras.github.io/leaflet-providers/preview/
#'
#' @export
tiles.lst <- list(
    'OSM Mapnik' = 'OpenStreetMap.Mapnik',
    'OSM B&W' = 'OpenStreetMap.BlackAndWhite',
    'OSM HOT' = 'OpenStreetMap.HOT',
    'OSM Topo' = 'OpenTopoMap',
    'Stamen Toner' = 'Stamen.Toner',
    'Stamen Toner Lite' = 'Stamen.TonerLite',
    'Stamen Terrain' = 'Stamen.Terrain',
    'Stamen Watercolor' = 'Stamen.Watercolor',
    'Esri Street Map' = 'Esri.WorldStreetMap',
    'Esri Topo Map' = 'Esri.WorldTopoMap',
    'Esri Imagery' = 'Esri.WorldImagery',
    'CartoDB Positron' = 'CartoDB.Positron',
    'CartoDB Dark Matter' = 'CartoDB.DarkMatter',
    'Hike Bike' = 'HikeBike.HikeBike'
)

#' List of available fill colours for Awesome Markers in leaflet maps
#'
#' @export
marker_colours <- c(
    'red', 'darkred', 'orange', 'pink', 'beige', 'green', 'darkgreen', 'lightgreen',
    'blue', 'lightblue', 'purple', 'cadetblue', 'white', 'lightgray', 'gray', 'black'
)

#' List of available methods when classifying numeric variables
#'
#' List prepared to be used as choices in Shiny input controls
#'
#' @export
class.methods <- c(
    'Fixed' = 'fixed',                  # need an additional argument fixedBreaks that lists the n+1 values to be used
    'Equal Intervals' = 'equal',        # the range of the variable is divided into n part of equal space
    'Quantiles' = 'quantile',           # each class contains (more or less) the same amount of values
    'Pretty Integers' = 'pretty',       # sequence of about ‘n+1’ equally spaced ‘round’ values which cover the range of the values in ‘x’. The values are chosen so that they are 1, 2 or 5 times a power of 10.
    'Natural Breaks' = 'jenks',         # seeks to reduce the variance within classes and maximize the variance between classes
    'Hierarchical Cluster' = 'hclust',  # Cluster with short distance
    'K-means Cluster' = 'kmeans'        # Cluster with low variance and similar size
)

#' List of ColorBrewer palettes partitioned by type of visualization scale
#'
#' List prepared to be used as choices in Shiny input controls
#'
#' @export
palettes.lst <- list(
    'SEQUENTIAL' = c( # ordinal data where (usually) low is less important and high is more important
        'Blues' = 'Blues', 'Blue-Green' = 'BuGn', 'Blue-Purple' = 'BuPu', 'Green-Blue' = 'GnBu', 'Greens' = 'Greens', 'Greys' = 'Greys',
        'Oranges' = 'Oranges', 'Orange-Red' = 'OrRd', 'Purple-Blue' = 'PuBu', 'Purple-Blue-Green' = 'PuBuGn', 'Purple-Red' = 'PuRd', 'Purples' = 'Purples',
        'Red-Purple' = 'RdPu', 'Reds' = 'Reds', 'Yellow-Green' = 'YlGn', 'Yellow-Green-Blue' = 'YlGnBu', 'Yellow-Orange-Brown' = 'YlOrBr',
        'Yellow-Orange-Red' = 'YlOrRd'
    ),
    'DIVERGING' = c(  # ordinal data where both low and high are important (i.e. deviation from some reference "average" point)
        'Brown-Blue-Green' = 'BrBG', 'Pink-Blue-Green' = 'PiYG', 'Purple-Red-Green' = 'PRGn', 'Orange-Purple' = 'PuOr', 'Red-Blue' = 'RdBu', 'Red-Grey' = 'RdGy',
        'Red-Yellow-Blue' = 'RdYlBu', 'Red-Yellow-Green' = 'RdYlGn', 'Spectral' = 'Spectral'
    ),
    'QUALITATIVE' = c(  # categorical/nominal data where there is no logical order
        'Accent' = 'Accent', 'Dark2' = 'Dark2', 'Paired' = 'Paired', 'Pastel1' = 'Pastel1', 'Pastel2' = 'Pastel2',
        'Set1' = 'Set1', 'Set2' = 'Set2', 'Set3' = 'Set3'
    )
)

#' List of available shapes with outline colour
#'
#' List prepared to be used as choices in Shiny input controls
#'
#' @export
point.shapes <- c('circle' = 21, 'square' = 22, 'diamond' = 23, 'triangle up' = 24, 'triangle down' = 25)

#' List of available line types
#'
#' @export
line.types <- c('dashed', 'dotted', 'solid', 'dotdash', 'longdash', 'twodash')

#' List of available font types
#'
#' List prepared to be used as choices in Shiny input controls
#'
#' @export
face.types <- c('plain', 'bold', 'italic', 'bold.italic')


# Generic options for labels when hovering polygons in leaflet maps
#'
#' @importFrom leaflet labelOptions
#'
#' @export
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

# Generic options for highlight when hovering polygons in leaflet maps
#'
#' @importFrom leaflet highlightOptions
#'
#' @export
hlt.options <- highlightOptions(
    weight = 6,
    color = 'white',
    opacity = 1,
    bringToFront = TRUE,
    sendToBack = TRUE
)

# List of UK Regions to use in Shiny Apps
#'
#' @export
rgns.lst <- list(
    'England' = c(
        'East Midlands', 'East of England', 'London',
        'North East', 'North West', 'South East', 'South West', 'West Midlands', 'Yorkshire and The Humber'
    ),
    'Northern Ireland', 'Scotland', 'Wales'
)


# .onLoad <- function(libname, pkgname) {
#
# }

.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        '\n
        ***************************************************
        * Welcome to the popiFun package. Have a nice day *
        ***************************************************
        \n'
    )
}
