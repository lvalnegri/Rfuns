#' read_html_ns
#' 
#' Read an HTML file when a website is not certified as secure, or self-certified
#'
#' @param x  The url of the file to download
#' @param el If not `NULL`, add an expression to retrieve the elements involved with the selector CSS `el`
#'
#' @return An `xml_document` or an `xml_nodeset` depending ont the value of the parameter `el`.
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @importFrom httr GET config
#' @importFrom rvest read_html html_elements
#'
#' @export
#'
read_html_ns <- function(x, el = NULL){
    y <- x |> GET(config = config(ssl_verifypeer = FALSE)) |> read_html()
    if(!is.null(el)) return(y |> html_elements(el))
    y
}
