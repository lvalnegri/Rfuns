% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datetime.R
\name{dd_round_time}
\alias{dd_round_time}
\title{Transform and classify time values}
\usage{
dd_round_time(x, prec = 1, meth = floor)
}
\arguments{
\item{x}{a vector of time values in \code{POSIXct} format}

\item{prec}{the value in seconds to use as span for the bins}

\item{meth}{a valid \code{R} operation to be used to round values in classes}
}
\value{
a vector
}
\description{
Transform and classify time values
}
\author{
Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
}
