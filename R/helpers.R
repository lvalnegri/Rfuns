#' Insert >>> TITLE <<< here ...
#'
#' @param alpha <description of alpha>
#' @param beta  <description of beta>
#'
#' @return describe what the function is giving back to the user (insert "None" if there's no value returned)
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#' @importFrom stringr str_pad
#'
#' @export
#'
get_num_sfx <- function(x){
    x <- as.character(x)
    switch(substring(x, nchar(x)),
        '1' = paste0(x, 'st'),
        '2' = paste0(x, 'nd'),
        '3' = paste0(x, 'rd'),
        paste0(x, 'th')
    )
}
