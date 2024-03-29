% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strings.R
\name{str_clean}
\alias{str_clean}
\title{str_clean}
\usage{
str_clean(x, subst = TRUE, subst_chr = "-")
}
\arguments{
\item{x}{the string}

\item{subst}{Indicate if dots and spaces should be kept but sobstituted with a different character}

\item{subst_car}{the character(s) to insert instead of dots and spaces}
}
\value{
a string
}
\description{
Clean a string from non alphanumeric characters
}
\author{
Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
}
