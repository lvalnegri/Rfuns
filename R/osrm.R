#' Crea un sistema di bande iscrone basate sia su tempo sia su mezzo di locomozione  
#'
#' @param x
#' @param prf codice del profilo: 1-auto, 2-cammino, 3-bici
#' @param dri
#' @param drx
#' @param rsl
#'
#' @return una lista di due oggetti poligoni \code{sf}
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @importFrom osrm osrmIsochrone
#' @importFrom leaflet addCircles addPolylines
#' 
#' @export
#'
crea_isocrone <- function(x, prf, dri, drx, rsl = NA){
    
    if(is.na(rsl)) rsl <- switch(prf, 30, 60, 40)
    sc <- masteRfun::conv2spat(x, crs.out = crs.it)
    y <- NULL
    y0i <- NULL
    brks <- unique(c(seq(dri, drx, by=dri), drx))
    
    for(brk in brks){
        y0 <- tryCatch(
            osrmIsochrone(sc, 
                breaks = brk, 
                res = rsl, 
                returnclass = 'sf', 
                osrm.server = paste0('http://master-i.ml:500', prf, '/')
            ),
            error = function(err) return(NULL)
        )
        if(!is.null(y0)){
            y0 <- sfheaders::sf_remove_holes(y0) |> st_transform(crs.it)
            # y0 <- y0[as.numeric(st_area(y0)) > ??? numero minimo in metri quadri ???]
            if(!is.null(y0i)){
                if(nrow(y0) > 0)
                    if(nrow(y0i) > 0){ suppressWarnings( st_union(y0, y0i |> st_geometry()) ) } else { y0 <- y0i }
            }
            y <- rbind(y, y0)
            y0i <- y0
        }
    }
    
    y <- y |> st_transform(4326)
    yd <- y[1,]
    if(nrow(y) > 1){
        for(idx in 2:nrow(y)){
            yt <- st_difference(y[idx,], y[idx - 1,])
            yt$min <- y[idx - 1, 'max'] |> st_drop_geometry()
            yd <- rbind(yd, subset(yt, select = !grepl('\\.', names(yt))))
        }
    }
    list('d' = yd[, c('min', 'max')], 'c' = y[, 'max'])
}

#' 
#'
#' @param x vettore punto con coordinate e codice sezione
#' @param y bacini geografici di isocrone in formato \code{sf}
#' @param dist indica se aggiungere o meno la distanza nel formato tabellare
#' @param dato_num codice per un dato indice potenziale o territoriale
#' @param dato_den codice per un dato territoriale da porre a confronto con \code{dato_num}
#'        (non considerato se a \code{dato_num} viene associato un indice)
#'
#' @return una lista di due oggetti SZN: uno composto da poligoni \code{sf}, l'altro una data.table con coordinate
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @importFrom dplyr filter arrange
#' @importFrom sf st_join st_within
#' 
#' @export
#'
estrai_SZN_isocrone <- function(x, y, dist = TRUE, dato_num = NA, dato_den = N){ # x-, y-, dato_num = ,  dato_num = codice dati_indici o terr den 
    y <- y |> 
        SZN_in_bbox(buffer = 0.025) |>
        st_join(y, join = st_within) |>
        filter(!is.na(max)) |>
        st_drop_geometry() |>
        arrange(max) |> 
        data.table()
    y <- y[y[, .I[which.min(max)], SZN]$V1, .(SZN, max)]
    y <- rbindlist(list( data.table(SZN = x$SZN, max = min(y$max)), y[SZN != x$SZN] ))
    if(!is.na(dato_num)){
        if(masteRdati::diz_dati[codice == dato_num, tipo] == 'indici'){
            y <- masteRdati::dati_indici[dato == dato_num & SZN %in% y$SZN, .(SZN, dato = valore)][y, on = 'SZN']
        } else {
            y <- masteRdati::dati_terr[dato == dato_num & SZN %in% y$SZN, .(SZN, dato = valore)][y, on = 'SZN']
            if(!is.na(dato_den)){
                y <- masteRdati::dati_terr[dato == dato_den & SZN %in% y$SZN, .(SZN, datod = valore)][y, on = 'SZN']
                y[, dato := round(dato / datod, 3)][, datod := NULL]
            }
        }
    }
    yg <- merge(masteRconfini::SZN, y)
    y <- masteRgeo::sezioni[SZN %in% y$SZN, .(SZN, x_lon, y_lat)][y, on = 'SZN']
    if(dist){
        yd <- calc_dist( x, masteRgeo::sezioni[SZN %in% y$SZN, .(SZN, x_lon, y_lat)], dist2int = TRUE )
        y <- yd[, .(SZN = id.y, distanza)][y, on = 'SZN'][order(max, distanza)]
    }
    list('g' = yg, 'p' = y)
}

#' Crea e visualizza il percorso piu breve fra due punti, tenendo presente un profilo di locomozione
#'
#' @param ps coordinate del punto di partenza
#' @param pe coordinate del punto di arrivo 
#' @param prf codice del profilo: 1-auto, 2-cammino, 3-bici
#' @param tessera tessera mappa di background (si veda l'elenco \code{tiles.lst})
#'
#' @return una mappa \emph{leaflet}
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @importFrom osrm osrmRoute
#' @importFrom leaflet addCircles addPolylines
#' 
#' @export
#'
mostra_percorso <- function(ps, pe, prf = 1, tessera = tiles.lst[[1]]){ #1-auto, 2-cammino, 3-bici
    yr <- osrmRoute(
        src = ps, 
        dst = pe, 
        overview = 'full', 
        returnclass = 'sf',
        osrm.server = paste0('http://master-i.ml:500', prf, '/')
    ) |> st_transform(4326)

    leaflet() |> 
        aggiungi_tessera(tessera) |> 
        addCircles(
            data = ps, 
            color = 'black', opacity = 1, radius = 6, weight = 3, 
            fillColor = 'green',  fillOpacity  = 1
        ) |>
        addCircles(
            data = pe, 
            color = 'black', opacity = 1, radius = 6, weight = 3, 
            fillColor = 'red',  fillOpacity  = 1
        ) |>
        addPolylines(
            data = yr, 
            color = 'darkred', opacity = 1,
            label = ~HTML(paste('Distanza:', yr$distance, ' metri<br/> Durata:', yr$duration, 'minuti'))
        )
}
