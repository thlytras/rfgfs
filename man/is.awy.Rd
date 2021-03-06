% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/awys.R
\name{is.awy}
\alias{is.awy}
\title{Is the argument a valid airway identifier?}
\usage{
is.awy(x)
}
\arguments{
\item{x}{A character vector (possibly of length >1) of airway identifiers.}
}
\value{
Returns TRUE for each element of x that is found in the database of airways.
}
\description{
Checks if the supplied argument is an airway identifier found in the database.
}
\seealso{
\code{\link{findApt}}, \code{\link{is.apt}}
}
