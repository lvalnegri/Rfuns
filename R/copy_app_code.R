#' copy_app_code
#' 
#' Copy the content of a shiny app folder into a convenient sub folder in the Shiny Server root folder.
#'
#' @param name Name to assign to the sub folder in the Shiny Server root folder. 
#'             Only letters, numbers, underscore and dash are allowed (you will be asked to substitute others with underscore).
#'             If such a folder already exists, any content will be removed; otherwise, a new folder will be created.
#' @param src  The source folder of the code.
#'             If `NULL`, it is assumed that the command has been run from a package project, and
#'             the content of the `shinyapp` folder will be copied (see the `create_package` function of the `Rtemplate` package)
#' @param dest the Shiny Server root folder
#' @param browse logical, when `TRUE` tries to open the app at the given `url`
#' @param url  the url of the website frontend for the shiny server
#' 
#' @return None 
#' 
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#' 
#' @export
#' 
copy_app_code <- function(name, src = NULL, dest = '/srv/shiny-server', browse = TRUE, url = 'analytics-hub.ml/shiny'){
    if(is.null(src)){
        if(!dir.exists('./shinyapp/')) stop('The subfolder `shininyapp` does not exists!')
    } else {
        if(!dir.exists(src)) stop('The given source folder does not exists!')
    }
    if(!dir.exists(dest)) stop('The given server root folder does not exists!')
    if(grepl('[^a-zA-Z0-9_-]', name)){
        ux <- readline('
                    There are characters not allowed in `name`.
                    Do you want me to substitute them with *underscore*? (y/Y to proceed)
        ')
        if(toupper(ux) != 'Y') stop('Invalid characters in `name`.')
    }
    pn <- file.path(dest, gsub('[^a-zA-Z0-9_-]', '_', name))
    if (dir.exists(pn)) system(paste0('rm -rf ', pn, '/'))
    dir.create(pn)
    if(is.null(src)){
        system(paste('cp -rf ./shinyapp/*', pn))
    } else {
        system(paste0('cp -rf ', src, '/* ', pn))
    }
    system(paste0('touch ', pn, '/restart.txt'))
    if(browse) browseURL(paste0('https://', url, '/', name))
}
