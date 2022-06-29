#' Exit a function without any hassle
#'
exit <- function() .Internal(.invokeRestart(list(NULL, NULL), NULL))

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

#' Location of the UK datasets in the shared repository
#'
#' @export
data_path <- file.path(pub_path, 'datasets')

#' Location of the top boundaries folder in rds format in the shared repository
#'
#' @export
bnd_path <- file.path(pub_path, 'boundaries')

#' Location of the boundaries for the UK in rds format in the shared repository
#'
#' @export
bnduk_path <- file.path(bnd_path, 'uk')

#' Location of the boundaries of the UK in rds format in the shared repository
#'
#' @export
bnduk_spath <- file.path(bnduk_path, 's20')

#' Location of the UK datasets in the shared repository
#'
#' @export
datauk_path <- file.path(data_path, 'uk')

#' Location of the geographic UK datasets in the shared repository
#'
#' @export
geouk_path <- file.path(datauk_path, 'geography')

#' Location of the Italian boundaries in the shared repository
#'
#' @export
bndit_path <- file.path(bnd_path, 'it')

#' Location of the Italian boundaries, in the shared repository, typically used in R applications (= rds format)
#'
#' @export
bndit_spath <- file.path(bndit_path, 's20')

#' Location of the Italian datasets in the shared repository
#'
#' @export
datait_path <- file.path(data_path, 'it')

#' Location of the geographic Italian datasets in the shared repository
#'
#' @export
geoit_path <- file.path(datait_path, 'geography')

#' Location of the boundaries for the US in rds format in the shared repository
#'
#' @export
bndus_path <- file.path(bnd_path, 'us')

#' Location of the US datasets in the shared repository
#'
#' @export
dataus_path <- file.path(data_path, 'us')

#' Location of the US Italian datasets in the shared repository
#'
#' @export
geous_path <- file.path(dataus_path, 'geography')

#' Location for any temporary files
#'
#' @export
tmp_path <- file.path(pub_path, 'temp')

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

#' UK Centroids
#'
#' @export
centers.uk <- list(
    'UK' = c(-2.902945, 54.17413),
    'GB' = c(-2.668885, 54.14725),
    'EW' = c(-1.777830, 52.55795),
    'E'  = c(-1.463432, 52.59309)
)

#' UK Bounding Box
#'
#' longitude: -8.650007 to  1.76368
#' latitude:  49.882346 to 60.84567
#'
#' @export
bbox.uk <- matrix(
    c(-8.650007, 1.76368, 49.882346, 60.84567),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c('x_lon', 'y_lat'), c('min', 'max'))
)

#' UK Bounding Box [No Outer Islands]
#'
#' The Bounding Box for the UK without the four following Islands:
#'  - S12000013 Na h-Eileanan Siar
#'  - S12000023 Orkney Islands
#'  - S12000027 Shetland Islands
#'  - E06000053 Isles of Scilly
#'
#' longitude: -8.178225 to  1.76368
#' latitude:  49.958709 to 58.69690
#'
#' @export
bbox.uk.cut <- matrix(
    c(-8.178225, 1.76368, 49.958709, 58.69690),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c('x_lon', 'y_lat'), c('min', 'max'))
)

#' List of UK geographic types
#'
#' For the complete list see https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=name&tags=all(PRD_RGC)
#'
#' @export
ukgeo.lst <- list(
    Census = c(
        'OA' = 'Output Area',
        'LSOA' = 'Lower Layer Super Output Area',
        'MSOA' = 'Middle Layer Super Output Area'
    )
)

# List of UK Regions / Countries
#'
#' @export
ukrgn.lst <- list(
    'England' = c(
        'East Midlands', 'East of England', 'London',
        'North East', 'North West', 'South East', 'South West', 'West Midlands', 'Yorkshire and The Humber'
    ),
    'Northern Ireland', 'Scotland', 'Wales'
)

#' Coordinate Reference Systems (CRS) in EPSG code (23032) for ED50 / UTM zone 32N - Denmark
#'
#' Coordinate system: Cartesian 2D CS
#' Axes: easting, northing (E,N)
#' proj4js: "+proj=utm +zone=32 +ellps=intl +units=m +no_defs ";
#' Bounds: [WGS84] ; [Projected]
#' Centroid: [WGS84] ; [Projected]
#' Unit: metre
#' Area of use: Austria; Belgium; Denmark; Finland; Faroe islands; France; Germany (west); Gibraltar; Greece; Italy; Luxembourg; Netherlands; Norway; Portugal; Spain; Sweden; Switzerland.
#' Accuracy: various
#'
#' sp::CRS(crs_code) to see the proj4js format for the specified epsg code
#' rgdal::make_EPSG() to build a table with code, name and proj4js format for all epsg codes
#'
#' @export
crs.it <- '+init=epsg:23032'

#' Italian Centroid (Narni, Terni, Umbria)
#'
#' @export
center.it <- c(12.523453, 42.516435)

#' Italian Bounding Box
#'
#' @export
bbox.it <- matrix(
    c(6.749955, 18.480247, 36.619987, 47.115393),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c('x_lon', 'y_lat'), c('min', 'max'))
)


#' List of available positions on the screen for objects that stays in a corner
#'
#' @export
pos.lst <- c('Bottom Right' = 'bottomright', 'Bottom Left' = 'bottomleft', 'Top Left' = 'topleft', 'Top Right' = 'topright')

#' List of available shapes with outline colour
#'
#' @export
pntshapes.lst <- c('circle' = 21, 'square' = 22, 'diamond' = 23, 'triangle up' = 24, 'triangle down' = 25)

#' List of available line types
#'
#' @export
lnstypes.lst <- c('dashed', 'dotted', 'solid', 'dotdash', 'longdash', 'twodash')

#' List of available font types
#'
#' @export
fcstypes.lst <- c('plain', 'bold', 'italic', 'bold.italic')


# .onLoad <- function(libname, pkgname) {
#
# }

.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        '\n
        **********************************************************************
        * Welcome to the datamaps various functions package. Have a nice day *
        **********************************************************************
        \n'
    )
}
