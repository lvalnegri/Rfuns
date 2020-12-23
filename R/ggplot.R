#' Global style for \code{ggplot2} charts
#'
#' @param g a ggplot
#' @param xaxis.draw
#' @param yaxis.draw
#' @param axis.draw
#' @param ticks.draw
#' @param axis.colour
#' @param axis.size
#' @param hgrid.draw
#' @param vgrid.draw
#' @param grids.colour
#' @param grids.size
#' @param grids.type
#' @param labels.rotation
#' @param labels.rotate
#' @param bkg.colour
#' @param font.size
#' @param ttl.font.size.mult
#' @param ttl.face
#' @param legend.pos
#' @param plot.border
#' @param font.family
#' @param
#'
#' @import ggplot2
#'
#' @return a ggplot
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#'
my_ggtheme <- function(g,
                    xaxis.draw = FALSE, yaxis.draw = FALSE, axis.draw = FALSE, ticks.draw = FALSE, axis.colour = 'black', axis.size = 0.1,
                    hgrid.draw = FALSE, vgrid.draw = FALSE, grids.colour = 'black', grids.size = 0.1, grids.type = 'dotted',
                    labels.rotation = c(45, 0), labels.rotate = FALSE,
                    font.size = 6, ttl.font.size.mult = 1.2, ttl.face = 'bold', font.family = 'Arial',
                    bkg.colour = 'white',
                    plot.border = FALSE,
                    legend.pos = 'bottom'
){

    g <- g + theme(

        text             = element_text(family = font.family),

        plot.title       = element_text(hjust = 0, size = rel(1.2) ),  # hjust: 0-left, 0.5-center, 1-right
        plot.background  = element_blank(),
        plot.margin      = unit(c(1, 0.5, 0, 0.5), 'lines'),  # space around the plot as in: TOP, RIGHT, BOTTOM, RIGHT
        plot.caption     = element_text(size = 8, face = 'italic'),

        axis.line        = element_blank(),
        axis.ticks       = element_blank(),
        axis.text        = element_text(size = font.size, color = axis.colour),
        axis.text.x      = element_text(angle = labels.rotation[1], hjust = 1), # vjust = 0.5),
        axis.text.y      = element_text(angle = labels.rotation[2]), # , hjust = , vjust = ),
        axis.title       = element_text(size = font.size * (1 + ttl.font.size.mult), face = ttl.face),
        axis.title.x     = element_text(vjust = -0.3),
        axis.title.y     = element_text(vjust = 0.8, margin = margin(0, 10, 0, 0) ),

        legend.text        = element_text(size = 6),
        legend.title       = element_text(size = 8),
        legend.title.align = 1,
        legend.position    = legend.pos,
        legend.background  = element_blank(),
        legend.spacing     = unit(0, 'cm'),
        # legend.key         = element_blank(),
        legend.key.size    = unit(0.2, 'cm'),
        legend.key.height  = unit(0.4, 'cm'),
        legend.key.width   = unit(1, 'cm'),

        panel.background = element_rect(fill = bkg.colour, colour = bkg.colour),
        panel.border     = element_blank(),
        panel.grid       = element_blank(),
        panel.spacing.x  = unit(3, 'lines'),
        panel.spacing.y  = unit(2, 'lines'),

        strip.text       = element_text(hjust = 0.5, size = font.size * (1 + ttl.font.size.mult), face = ttl.face),
        strip.background = element_blank()

    )

    if(plot.border) g <- g + theme( panel.border = element_rect(colour = axis.colour, size = axis.size, fill = NA) )

    if(axis.draw){
        g <- g + theme( axis.line = element_line(color = axis.colour, size = axis.size ) )
    } else {
        if(xaxis.draw) g <- g + theme( axis.line.x = element_line(color = axis.colour, size = axis.size ) )
        if(yaxis.draw) g <- g + theme( axis.line.y = element_line(color = axis.colour, size = axis.size ) )
    }

    if(ticks.draw)  g <- g + theme( axis.ticks = element_line(color = axis.colour, size = axis.size ) )

    if(hgrid.draw & vgrid.draw){
        g <- g + theme( panel.grid.major = element_line(colour = grids.colour, size = grids.size, linetype = grids.type ) )
    } else{
        if(vgrid.draw) g <- g + theme( panel.grid.major.x = element_line(colour = grids.colour, size = grids.size, linetype = grids.type ) )
        if(hgrid.draw) g <- g + theme( panel.grid.major.y = element_line(colour = grids.colour, size = grids.size, linetype = grids.type ) )
    }

    if(labels.rotate){
        g <- g + theme( axis.text.x = element_text(hjust = 1, angle = 45 ) )
    }

    g
}

#' Convert a \code{ggplot} into its corresponding \code{htmlwidgets} interactive plot using the \code{ggiraph} extension
#'
#' @param p
#' @param gg.width
#' @param sel.type
#'
#' @return a ggiraph ggplot
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import ggplot2 ggiraph
#'
#' @export
#'
gg.to.ggiraph <- function(p, sel.type = 'single', gg.width = 0.8){
        girafe( ggobj = p,
            width  = gg.width,
            zoom_max  = 1,
            selection_type = sel.type,
            # selected_css = "",
            tooltip_offx = 20, tooltip_offy = -10,
            hover_css = "fill:red;cursor:pointer;r:4pt;opacity-value:0.5;",
            tooltip_extra_css= "background-color:wheat;color:gray20;border-radius:10px;padding:3pt;",
            tooltip_opacity = 0.9,
            pointsize = 12
        )
}

#' Return correct spacing for axis labels rotation in \code{ggplot2} charts to use in \code{my_ggtheme}
#'
#' @return a ggplot
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import ggplot2
#'
lbl.plt.rotation = function(angle, position = 'x'){
    positions = list(x = 0, y = 90, top = 180, right = 270)
    rads  = (angle - positions[[ position ]]) * pi / 180
    hjust = 0.5 * (1 - sin(rads))
    vjust = 0.5 * (1 + cos(rads))
    element_text(angle = angle, vjust = vjust, hjust = hjust)
}

