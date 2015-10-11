#' rfgfs: A flight planner and companion package for FlightGear flight simulator (fgfs)
#'
#' The rfgfs package allows planning a route between two airports, using the airport, airway, navaid and navfix data that ship with FlightGear. These data have been parsed into .RData, are included with the package internally, and can be retrieved with the function \code{\link{flightData}}. The main workhorse of the package is the function \code{\link{planRoute}}; this returns a full flight plan as a data.frame, and this can be further written to a kml file (that can be opened with Google Earth or other kml viewer) using function \code{\link{makeKml}}.
#' 
# @section Foo functions:
# The foo functions ...
#'
#' @docType package
#' @name rfgfs-package
NULL
