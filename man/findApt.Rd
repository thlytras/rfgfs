% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apts.R
\name{findApt}
\alias{findApt}
\title{Find airports in the database}
\usage{
findApt(x)
}
\arguments{
\item{x}{A character string containing a regular expression to be matched in the airport descriptions.}
}
\value{
A data.frame of airports found, with columns: "icaoCode", "fullName", "elevation", "lat", "lon".
}
\description{
\code{findApt} finds airports in the database by searching for matches in the airport description. Internally it uses \code{grep}.
}
\examples{
findApt("Makedonia")   # Find by description
findApt("LGTS")   # Find by ICAO code
}
