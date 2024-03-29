% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geography.R
\name{dd_calc_dist}
\alias{dd_calc_dist}
\title{dd_calc_dist}
\usage{
dd_calc_dist(
  x,
  spec = TRUE,
  knn = TRUE,
  cid = "id",
  clon = "x_lon",
  clat = "y_lat",
  verbose = FALSE
)
}
\arguments{
\item{x}{a data.table with at least 3 columns, one for the id, the other two for the coordinates}

\item{spec}{double each pair, so you can query a single column}

\item{knn}{calculate the order of the neighbour for each pair (implies \code{spec = TRUE})}

\item{cid}{the name of the id column}

\item{clon}{the name of the longitude column}

\item{clat}{the name of the latitude column}

\item{verbose}{tracks the calculation}
}
\value{
a data.table in long format with the ids of each pair of locations and the corresponding distance.
Optionally, the ranking order.
}
\description{
Calculates the complete set of pair distances for a set of locations
}
\author{
Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
}
