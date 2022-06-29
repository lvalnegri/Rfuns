#' Provides a mapping between numeric data values and colours, according to specified classification method and palette
#'
#' @param x
#' @param cls_mth
#' @param n_brks
#' @param fxd_brks
#' @param use_palette
#' @param br_pal
#' @param fxd_cols
#' @param rev_cols
#' @param add_legend
#' @param dec.fig
#' @param bsep
#' @param pct_sign
#' @param del_signs
#'
#' @return a character vector with the RGB string describing the colours for the required palette
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table classInt RColorBrewer
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
                        pct_sign  = FALSE,
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
#' @param mtc the metric used in the choropleth
#' @param brks a vector that describes the $(n + 1)$ breaks that build the $n$ bins
#' @param dec.fig the number of decimal to keep
#' @param del_signs if TRUE, drop $+/-$ from numbers
#'
#' @return a character vector
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#' @importFrom stringr str_pad
#'
#' @export
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

#' Determines the labels for n bins given a series of (n + 1) breaks
#'
#'
#' @param y a vector describing the limits of the bins
#' @param dec.fig the number of decimal to keep
#' @param del_signs if TRUE, drop +/- from numbers
#'
#' @return a data.table with limits and labels
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
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

#' List of available methods when classifying numeric variables
#'
#' @export
class.lst <- c(
    'Fixed' = 'fixed',                   # need an additional argument fixedBreaks that lists the n+1 values to be used
    'Equal Intervals' = 'equal',         # the range of the variable is divided into n part of equal space
    'Quantiles' = 'quantile',            # each class contains (more or less) the same amount of values
    'Pretty Integers' = 'pretty',        # sequence of about ‘n+1’ equally spaced ‘round’ values which cover the range of the values in ‘x’. The values are chosen so that they are 1, 2 or 5 times a power of 10.
    'Natural Breaks' = 'jenks',          # seeks to reduce the variance within classes and maximize the variance between classes
    'Hierarchical Cluster' = 'hclust',   # Cluster with short distance
    'K-means Cluster' = 'kmeans',        # Cluster with low variance and similar size
    'Outliers: Percentages' = 'out_pct', # 
    'Outliers: Boxplot' = 'out_box',     # 
    'Outliers: Standardized' = 'out_std' #  
)

#' List of ColorBrewer palettes partitioned by type of visualization scale
#'
#' @export
palettes.lst <- list(
    'SEQUENTIAL' = c( # ordinal data where (usually) low is less important and high is more important
        'Blues' = 'Blues', 'Blue-Green' = 'BuGn', 'Blue-Purple' = 'BuPu', 'Green-Blue' = 'GnBu', 'Greens' = 'Greens', 'Greys' = 'Greys',
        'Oranges' = 'Oranges', 'Orange-Red' = 'OrRd', 'Purple-Blue' = 'PuBu', 'Purple-Blue-Green' = 'PuBuGn', 'Purple-Red' = 'PuRd', 'Purples' = 'Purples',
        'Red-Purple' = 'RdPu', 'Reds' = 'Reds', 'Yellow-Green' = 'YlGn', 'Yellow-Green-Blue' = 'YlGnBu', 'Yellow-Orange-Brown' = 'YlOrBr',
        'Yellow-Orange-Red' = 'YlOrRd'
    ),
    'DIVERGING' = c(  # ordinal data where both low and high are important (i.e. deviation from some reference "average" point)
        'Brown-Blue-Green' = 'BrBG', 'Pink-Blue-Green' = 'PiYG', 'Purple-Red-Green' = 'PRGn', 'Orange-Purple' = 'PuOr', 'Red-Blue' = 'RdBu', 'Red-Grey' = 'RdGy',
        'Red-Yellow-Blue' = 'RdYlBu', 'Red-Yellow-Green' = 'RdYlGn', 'Spectral' = 'Spectral'
    ),
    'QUALITATIVE' = c(  # categorical/nominal data where there is no logical order
        'Accent' = 'Accent', 'Dark2' = 'Dark2', 'Paired' = 'Paired', 'Pastel1' = 'Pastel1', 'Pastel2' = 'Pastel2',
        'Set1' = 'Set1', 'Set2' = 'Set2', 'Set3' = 'Set3'
    )
)
