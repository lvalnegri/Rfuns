% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postcodes.R
\name{get_postcodes_dup}
\alias{get_postcodes_dup}
\title{Find duplications in geographies hierarchy inside the postcodes table}
\usage{
get_postcodes_dup(parent, child, subst_only = FALSE)
}
\arguments{
\item{parent}{the bigger area type}

\item{child}{the smaller area type}

\item{subset_only}{if TRUE, returns only the codes without the counts of postcodes for each parent-child pair}
}
\value{
a list with two data.tables:
\itemize{
\item "dups" with all the ...
\item "overs" with ...
}
}
\description{
Examine the postcodes table over two locations types allegedly in a hierarchy, the first being the parent, the second the child
}
\author{
Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
}
