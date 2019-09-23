#' Top root of the shared repository in every datamaps VPS
#'
#' @export
pub_path <- Sys.getenv('PUB_PATH')

#' Root of any external source data
#'
#' @export
ext_path <- file.path(pub_path, 'ext_data')


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
bndit_spath <- file.path(bndit_path, 'rds', 's20')

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
