% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NVU-RSBN.R
\name{orthoCoords}
\alias{orthoCoords}
\title{Convert geographical coordinates to orthodromic coordinates.}
\usage{
orthoCoords(lat1, lon1, lat2, lon2, latP, lonP)
}
\arguments{
\item{lat1}{Latitude of departure point (in degrees)}

\item{lon1}{Longitude of departure point (in degrees)}

\item{lat2}{Latitude of destination point (in degrees)}

\item{lon2}{Longitude of destination point (in degrees)}

\item{latP}{Latitude of point to be converted to orthodromic coordinates (in degrees)}

\item{lonP}{Longitude of point to be converted to orthodromic coordinates (in degrees)}
}
\value{
Returns a named vector with orthodromic coordinates Z and S (in km),
as well as S0, the negative of the leg distance (which is equivalently
the S coordinate of the departure point).
}
\description{
This function is used in NVU calculations and corrections.
}
