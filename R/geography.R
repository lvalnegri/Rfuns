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
clean_postcode <- function(dt, cname){
    setnames(dt, cname, 'X')
    dt[, X := toupper(gsub('[[:punct:]| ]', '', X)) ]
    dt[!grepl('[[:digit:]][[:alpha:]][[:alpha:]]$', X), X := NA]
    dt[grepl('^[0-9]', X), X := NA]
    dt[nchar(X) < 5 | nchar(X) > 7, X := NA]
    dt[nchar(X) == 5, X := paste0( substr(X, 1, 2), '  ', substring(X, 3) ) ]
    dt[nchar(X) == 6, X := paste0( substr(X, 1, 3), ' ', substring(X, 4) ) ]
    setnames(dt, 'X', cname)
}
