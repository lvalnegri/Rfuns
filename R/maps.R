#' A leaflet map to start with, centered around a givcen coordinate or fitting a given bounding box
#'
#'
#'
#' @param coords Center the map around this long/lat point (default is UK centroid)
#' @param bbox Alternative to <coords>, set the bounds of a map. In a 2x2 matrix form, long as first row, lat as second row; min as first  column, max as second column
#' @param zm The admissible minimum zoom. Default is 6, which is the minimum for the UK to be visible as a whole
#' @param tiles List of map backgroud tiles, by default all layers listed in tiles.lst. Pass a NULL value to add only the default tile.
#'
#' @return a leaflet map
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#' @references leaflet, maps
#'
#' @import leaflet leaflet.extras
#'
#' @examples
#' \dontrun{
#'     example code
#' }
#'
#' @export
#'
basemap <- function(coords = c(-2.903205, 54.17463), bbox = NULL, zm = 6, tiles = tiles.lst){
    mp <- leaflet(options = leafletOptions(minZoom = zm)) %>%
        enableTileCaching() %>%
        addSearchOSM() %>%
        addResetMapButton() %>%
        addFullscreenControl()
    if(is.null(bbox)){
        mp <- mp %>% setView(coords[1], coords[2], zoom = zm)
    } else {
        mp <- mp %>% fitBounds(bbox[1, 1], bbox[2, 1], bbox[1, 2], bbox[2, 2])
    }
    if(is.null(tiles)){
        mp <- mp %>% addTiles()
    } else {
        for(idx in 1:length(tiles))
            mp <- mp %>%
                addProviderTiles(tiles[[idx]], group = names(tiles)[idx]) %>%
                showGroup(tiles[1])
    }
    mp
}

#' Insert >>> TITLE <<< here ...
#'
#' Insert (longer) >>> DESCRIPTION <<< here ...
#'
#' @param alpha <description of alpha>
#' @param beta  <description of beta>
#'
#' @return describe what the function is giving back to the user (insert "None" if there's no value returned)
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#' @references leaflet, maps
#' @seealso \code{\link{...}}
#'
#' @keywords
#'
#' @import data.table classInt RColorBrewer
#'
#' @examples
#' \dontrun{
#'     example code
#' }
#'
#' @export
#'
get_palette <- function(X,
                        cls_mth = 'quantile',
                        n_brks = 7,
                        fxd_brks = NULL,
                        use_palette = TRUE,
                        br_pal = 'Greys',
                        fxd_cols = c('#ff0000', '#ffff00', '#00ff00'),
                        rev_cols = FALSE,
                        add_legend = TRUE,
                        dec.fig = 0,
                        bsep = ',',
                        del_signs = TRUE
    ){

    if(!cls_mth %in% c('fixed', 'equal', 'quantile', 'pretty', 'jenks', 'hclust', 'kmeans')){
        warning('The provided method does not exist! Reverting to "quantile"')
        cls_mth <- 'quantile'
    }

    if(cls_mth == 'fixed'){
        if(is.null(fxd_brks)) stop('When asking for the "fixed" method, you have to include a vector of convenient bin limits.')
        if(!is.numeric(fxd_brks)) stop('The vector containing the bin limits must be numeric.')
        fxd_brks <- sort(fxd_brks)
        mX <- min(X, na.rm = TRUE)
        MX <- max(X, na.rm = TRUE)
        if(MX > max(fxd_brks)) fxd_brks <- c(fxd_brks, MX)
        if(mX < min(fxd_brks)) fxd_brks <- c(mX, fxd_brks)
        n_brks <- length(fxd_brks) - 1
    }

    brks_poly <-
        if(cls_mth == 'fixed'){
            classIntervals(X, n = n_brks, style = 'fixed', fixedBreaks = fxd_brks)
        } else {
            classIntervals(X, n = n_brks, style = cls_mth)
        }

    # Determine the color palette
    if(use_palette){
        if(!br_pal %in% rownames(brewer.pal.info)){
            warning('The provided palette does not exist! Reverting to "Greys"')
            br_pal <- 'Greys'
        }
        col_codes <-
            if(n_brks > brewer.pal.info[br_pal, 'maxcolors']){
                colorRampPalette(brewer.pal(brewer.pal.info[br_pal, 'maxcolors'], br_pal))(n_brks)
            } else {
                brewer.pal(n_brks, br_pal)
            }
        if(rev_cols) col_codes <- rev(col_codes)
    } else {
        col_codes <- colorRampPalette(fxd_cols)(n_brks)
    }

    # return a list
    if(add_legend){
        list(
            findColours(brks_poly, col_codes), brks_poly, col_codes,
            get_map_legend(X, brks_poly$brks, dec.fig, del_signs)
        )
    } else {
        list(findColours(brks_poly, col_codes), brks_poly, col_codes)
    }
}

#' Build a legend to be used in a leaflet thematic map
#'
#' @param alpha <description of alpha>
#' @param beta  <description of beta>
#'
#' @return describe what the function is giving back to the user (insert "None" if there's no value returned)
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#' @references leaflet, maps
#'
#' @keywords
#'
#' @examples
#'
#' @import data.table
#'
#' @importFrom stringr str_pad
#'
#' @export
#'
#' \dontrun{
#'     example code
#' }
#'
get_map_legend <- function(mtc, brks, dec.fig = 2, del_signs = TRUE) {
    lbl <- get_fxb_labels(brks, dec.fig, del_signs)
    brks <- sort(as.numeric(unique(c(lbl$lim_inf, lbl$lim_sup))))
    mtc <- data.table('value' = mtc)
    mtc <- mtc[, .N, value][!is.na(value)]
    mtc[, label := cut(value, brks, lbl$label, ordered = TRUE)]
    mtc <- mtc[, .(N = sum(N)), label][order(label)][!is.na(label)]
    mtc <- mtc[lbl[, .(label)], on = 'label'][is.na(N), N := 0]
    mtc[, N := format(N, big.mark = ',')]
    ypad <- max(nchar(as.character(mtc$N))) + 3
    mtc[, label := paste0(label, str_pad(paste0(' (', N, ')'), ypad, 'left'))]
    mtc$label
}

#' Determines labels for (n + 1) bins given a series of n breaks or a convenient building method
#'
#' Insert (longer) >>> DESCRIPTION <<< here ...
#'
#' @param y a vector describing
#' @param beta  <description of beta>
#'
#' @return a data.table with limits and labels
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#'
#' @importFrom stringr str_pad
#'
get_fxb_labels <- function(y, dec.fig = 1, del_signs = TRUE){
    y <- gsub('^ *|(?<= ) | *$', '', gsub('(?!\\+|-|\\.)[[:alpha:][:punct:]]', ' ', y, perl = TRUE), perl = TRUE)
    y <- paste(y, collapse = ' ')
    if(del_signs){
        y <- gsub('*\\+', Inf, y)
        y <- gsub('*\\-', -Inf, y)
    }
    y <- unique(sort(as.numeric(unlist(strsplit(y, split = ' ')))))
    lbl_brks <- format(round(y, 3), nsmall = dec.fig)
    lbl_brks <- str_pad(lbl_brks, max(nchar(lbl_brks)), 'left')
    y <- data.table(
        'lim_inf' = lbl_brks[1:(length(lbl_brks) - 1)],
        'lim_sup' = lbl_brks[2:length(lbl_brks)],
        'label' = sapply(1:(length(lbl_brks) - 1), function(x) paste0(lbl_brks[x], ' ─ ', lbl_brks[x + 1]))
    )

}
