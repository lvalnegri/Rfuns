#' Capitalize the first only or all the words in a text column, meanwhile cleaning of redundant spaces
#'
#' @param dt a data.table
#' @param x the column whose valued have to be cleaned and capitalized
#' @param all_words if TRUE the capitalization is applied on all the words of the content. When FALSE only the first word is capitalized.
#' @param as_factor if TRUE, the specified column is converted to factor
#'
#' @return A data.table similar to the one in input, but with the specified column modified as asked
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @export
#'
capitalize <- function(dt, x, all_words = TRUE, as_factor = TRUE){
    ndt <- names(dt)
    y <- unique(dt[, .(capitalize = get(x))])
    y[, new_capitalize := tolower(gsub('\\s+', ' ', trimws(capitalize)))]
    if(all_words){
        y[, new_capitalize := gsub('(^|[[:space:]]|-)([[:alpha:]])', '\\1\\U\\2', new_capitalize, perl = TRUE)]
    } else {
        y[, new_capitalize := paste0(toupper(substr(new_capitalize, 1, 1)), substring(new_capitalize, 2))]
    }
    if(as_factor) y[, new_capitalize := factor(new_capitalize)]
    dt <- y[dt, on = c('capitalize' = x)][, `:=`(capitalize, NULL)]
    setnames(dt, 'new_capitalize', x)
    setcolorder(dt, ndt)
    dt
}


#' Add a trailing space to a value which is less than 10
#'
#' @param x a numeric value
#'
#' @return A character value
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
add_space <- function(x){ ifelse(x < 10, paste0('0', x), as.character(x)) }


#' Format a decimal number as percentage, multiplying by 100 and adding the percent sign
#'
#' @param x a numeric vector
#' @param dgt number of decimal digits to retain
#'
#' @return A character vector
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
add_pct <- function(x, dgt = 1){ paste0(formatC(100 * x, digits = dgt, format = 'f'), '%') }


#' Add a comma to thousands
#'
#' @param x a numeric vector
#'
#' @return A character vector
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
add_Kcomma <- function(x){ formatC(x, big.mark = ',') }
