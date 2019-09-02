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
#' @param dt a data.table
#' @param clean_pc
#' @param pc_cname
#' @param oa_only
#' @param census
#' @param admin
#' @param postal
#' @param electoral
#' @param nhs
#' @param cols_in
#' @param cols_out The columns you don't want to be included in the output. You can not avoid to choose neither OA nor WPZ.
#'
#' @return a data.table with possibly cleaned postcode, OA and WPZ columns, plus all other specified
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#' @importFrom fst read_fst
#'
#' @examples
#' \dontrun{
#'   add_geocodes(dts)
#'   add_geocodes(dts, postal = FALSE)
#'   add_geocodes(dts, postal = FALSE, workplace = TRUE)
#' }
#'
#' @export
#'
add_geocodes <- function(dt,
                        clean_pc = TRUE, pc_cname = 'postcode',
                        oa_only = FALSE,
                        census = TRUE, admin = TRUE, postal = TRUE, electoral = FALSE, nhs = FALSE,
                        cols_in = NULL, cols_out = NULL
                ){
    dt <- copy(dt)
    cname <- names(dt)[1:which(grepl(pc_cname, names(dt)))]
    if(clean_pc) clean_postcode(dt, pc_cname)
    y <- read_fst( file.path(geouk_path, 'postcodes'), columns = c('postcode', 'OA', 'WPZ'), as.data.table = TRUE )
    setnames(dt, pc_cname, 'postcode')
    dt <- y[dt, on = 'postcode']
    setnames(dt, 'postcode', pc_cname)
    cols <- 'OA'
    if(!oa_only){
        cols_all <- c(
            'OA', 'LSOA', 'MSOA', 'LAD', 'CTY', 'RGN', 'CTRY',
            'PCS', 'PCD', 'PCT', 'PCA',
            'TTWA', 'WARD', 'PCON', 'CED', 'PAR', 'BUA', 'BUAS', 'MTC', 'PFA',
            'STP', 'CCG', 'NHSO', 'NHSR'
        )
        if(census) cols <- c(cols, c('LSOA', 'MSOA', 'LAD'))
        if(admin) cols <- c(cols, c('LAD', 'CTY', 'RGN', 'CTRY'))
        if(postal) cols <- c(cols, c('PCS', 'PCD', 'PCT', 'PCA'))
        if(electoral) cols <- c(cols, c('PCON', 'WARD', 'CED'))
        if(nhs) cols <- c(cols, c('CCG', 'NHSO', 'NHSR'))
        if(!is.null(cols_in)) cols <- c(cols, cols_in)
        if(!is.null(cols_out)) cols <- setdiff(cols, setdiff(cols_out, 'OA'))
        cols <- unique(intersect(cols, cols_all))
        y <- read_fst( file.path(geouk_path, 'output_areas'), columns = cols, as.data.table = TRUE )
        dt <- y[dt, on = 'OA']
    }
    setcolorder(dt, c(cname, cols, 'WPZ'))
    droplevels(dt)
}
