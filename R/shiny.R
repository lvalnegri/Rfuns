#' copy_app_code
#' 
#' Copy the content of the `shinyapp` folder inside a package into a Shiny Server subfolder named `name`.
#'
#' @param name Name to assign to the subfolder in the Server Shiny (not necessarily equal to package name) 
#' @param dest Shiny Server root folder
#' 
#' @return None 
#' 
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#' 
#' @export
#' 
copy_app_code <- function(name, dest = '/srv/shiny-server/'){
    pn <- file.path(dest, nome)
    if(dir.exists(pn)) system(paste0('rm -rf ', pn, '/')) 
    dir.create(pn)
    system(paste('cp -rf ./shinyapp/*', pn))
    system(paste0('touch ', pn, '/restart.txt'))
}

