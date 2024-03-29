% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maps_legend.R
\name{get_fxb_labels}
\alias{get_fxb_labels}
\title{Determines the labels for n bins given a series of (n + 1) breaks}
\usage{
get_fxb_labels(y, dec.fig = 1, del_signs = TRUE)
}
\arguments{
\item{y}{a vector describing the limits of the bins}

\item{dec.fig}{the number of decimal to keep}

\item{del_signs}{if TRUE, drop +/- from numbers}
}
\value{
a data.table with limits and labels
}
\description{
Determines the labels for n bins given a series of (n + 1) breaks
}
\author{
Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
}
