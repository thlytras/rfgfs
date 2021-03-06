% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vn.R
\name{vnNVUtune}
\alias{vnNVUtune}
\title{"Virtual Navigator": Tune the Tu-154B NVU system}
\usage{
vnNVUtune(con, ZPY = NA, S = NA, Z = NA, active = TRUE)
}
\arguments{
\item{con}{A socket connection object created with \code{\link{fgfsConnect}}}

\item{ZPY}{ZPY angle, in degrees. This is rounded to the first decimal when
passing to FlightGear. If \code{NA}, the already tuned ZPY is not modified.}

\item{S}{Leg distance (S), in km. If \code{NA}, the already tuned S is not modified.}

\item{Z}{Cross track distance (Z), in km. If \code{NA}, the already tuned Z is not modified.}

\item{active}{If \code{TRUE}, tune the active device. If \code{FALSE}, tune the
inactive device (i.e. tune Sp and Zp).}
}
\description{
Through the FlightGear props (telnet) interface, tune the NVU system
of the Tu-154B to the required navigational parameters (ZPY, S, Z).
}
