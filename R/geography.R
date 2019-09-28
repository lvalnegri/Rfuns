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

#' Add geographical area codes to a dataset, starting from a postcode column
#'
#' @param dt a data.table
#' @param clean_pc if the postcode column needs to be cleaned beforehand
#' @param pc_cname if clean_pc is TRUE, this is the column name to consider
#' @param oa_only Add only the Output Area column, disregarding all the other options
#' @param census Add geographies related to the "Census" hierarchy: 'LSOA', 'MSOA', 'LAD'
#' @param admin Add geographies related to the "Admin" hierarchy: 'LAD', 'CTY', 'RGN', 'CTRY'
#' @param postal Add geographies related to the "Postal" hierarchy: 'PCS', 'PCD', 'PCT', 'PCA'
#' @param electoral Add geographies related to the "Electoral" hierarchy: 'PCON', 'WARD', 'CED'
#' @param nhs Add geographies related to the "NHS" hierarchy: 'CCG', 'NHSO', 'NHSR'
#' @param cols_in Insert here isolated columns to add to the output dataset
#' @param cols_out The columns you don't want to be included in the output Note that you can not exclude neither OA nor WPZ.
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
                        oa_only = FALSE, add_oa = TRUE,
                        census = TRUE, admin = TRUE, postal = TRUE, electoral = FALSE, nhs = FALSE,
                        cols_in = NULL, cols_out = NULL
                ){
    dt <- copy(dt)
    cname <- names(dt)[1:which(grepl(pc_cname, names(dt)))]
    if(clean_pc) clean_postcode(dt, pc_cname)
    if(add_oa){
        y <- read_fst( file.path(geouk_path, 'postcodes'), columns = c('postcode', 'OA', 'WPZ'), as.data.table = TRUE )
        setnames(dt, pc_cname, 'postcode')
        dt <- y[dt, on = 'postcode']
        setnames(dt, 'postcode', pc_cname)
    }
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
