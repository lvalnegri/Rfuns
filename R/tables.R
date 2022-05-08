#' Build a simple frequency table with relative/percentage and cumulative
#'
#' @param x a vector of values to be tabulated
#' @param use_na if TRUE the table will include missing values as a separate value/category 
#' @param ord_freq if TRUE the table will be ordered by frequency instead of attributes 
#' @param ord_desc if TRUE the table will be ordered in decreasing instead of increasing
#' @param add_pct to add relative/percentage frequency
#' @param mlt_pct an integer multiplier for the "percentage" column 
#' @param dgt_pct the decimal digits to keep for the "percentage" column
#' @param add_cum a character string to indicate to add cumulative frequency ('f') and/or percentage ('p')
#' @param col_names a character vector of the required number of elements for the column names in the output dataset. 
#'                  By default, they are: \code{c('X', 'freq', 'pct', 'cum_freq', 'cum_pct')}
#'
#' @return A data.table
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#' 
#' @export
#'
dd_tab_pct <- function(x, 
                       use_na = FALSE, 
                       ord_freq = FALSE, 
                       ord_desc = FALSE, 
                       add_pct = FALSE, 
                       mlt_pct = 100, 
                       dgt_pct = 2, 
                       add_cum = NA,
                       col_names = NULL
              ){
    cnames <- if(is.null(col_names)) c('X', 'freq') else col_names
    y <- {if(use_na) data.table(X = as.character(x)) else data.table(X = as.character(x))[!is.na(X)]}[, .(XF = .N), X]
    setorderv(y, cols = ifelse(ord_freq, 'XF', 'X'), order = ifelse(ord_desc, -1, 1))
    if(!is.na(add_cum)){
        if('f' %in% strsplit(add_cum, '')[[1]]){
            y[, XCF := cumsum(XF)]
            if(is.null(col_names)) cnames <- c(cnames, 'cum_freq')
        }
    }
    if(add_pct){
        y[, XP := formatC(mlt_pct * XF / sum(XF), dgt_pct, format = 'f')]
        if(is.null(col_names)) cnames <- c(cnames, 'pct')
        if(!is.na(add_cum)){
            if('p' %in% strsplit(add_cum, '')[[1]]){
                y[, XCP := round(cumsum(mlt_pct * XF/sum(XF)), dgt_pct )]
                if(is.null(col_names)) cnames <- c(cnames, 'cum_pct')
            }
        }
    }
    if(sum(suppressWarnings(is.na(as.numeric(y[!is.na(X), X])))) == 0) y[, X := as.numeric(X)]
    setnames(y, cnames[1:ncol(y)])
    y
}

