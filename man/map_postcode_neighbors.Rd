% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postcodes.R
\name{map_postcode_neighbors}
\alias{map_postcode_neighbors}
\title{Return a map of the set of postcodes included in a bounding box or circle with a specified postcode unit as centroid}
\usage{
map_postcode_neighbors(
  postcode,
  distance = 0.5,
  circle = TRUE,
  in.miles = TRUE,
  active_only = FALSE,
  tiles = tiles.lst,
  use_icons = FALSE
)
}
\arguments{
\item{in.miles}{}
}
\value{
a leaflet map
}
\description{
Return a map of the set of postcodes included in a bounding box or circle with a specified postcode unit as centroid
}
\author{
Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
}
