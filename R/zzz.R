#' @export
pub_path <- Sys.getenv('PUB_PATH')

#' @export
bnduk_path <- file.path(pub_path, 'boundaries', 'uk')

#' @export
datauk_path <- file.path(pub_path, 'datasets', 'uk')

#' @export
geouk_path <- file.path(datauk_path, 'geography', 'uk')


# .onLoad <- function(libname, pkgname) {
#
# }

.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        'Welcome to the popiFun package. Have a nice day!'
    )
}
