% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osrm.R
\name{crea_isocrone}
\alias{crea_isocrone}
\title{Crea un sistema di bande iscrone basate sia su tempo sia su mezzo di locomozione}
\usage{
crea_isocrone(x, prf, dri, drx, rsl = NA)
}
\arguments{
\item{prf}{codice del profilo: 1-auto, 2-cammino, 3-bici}

\item{rsl}{}
}
\value{
una lista di due oggetti poligoni \code{sf}
}
\description{
Crea un sistema di bande iscrone basate sia su tempo sia su mezzo di locomozione
}
\author{
Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
}
