# Functions that handle airport data

#' Get the navaid or navfix nearest to an airport
#'
#' \code{getAptFix} finds the navaid or navfix that is closest to a given airport.
#'
#' @param airport A 4-letter airport ICAO code (character vector of length 1).
#' @return A string identifying the nearest navfix/navaid. Because identifiers are not globally unique, the string includes a number, which is the difference between latitude and longitude.
#' @export
#' @seealso \code{\link{planRoute}}
getAptFix <- function(airport) {
  a <- fltData$apt[airport,]
  if (is.na(a$icaoCode)) return(NULL)
  b <- subset(fltData$pts, abs(fixLat-a$lat)<2 & abs(fixLon-a$lon)<2)
  if (nrow(b)==0) return(NA)
  b$dist <- spDistsN1(cbind(b$fixLon,b$fixLat), matrix(c(a$lon,a$lat), nrow=1))
  subset(b, dist==min(dist) & dist<1)$id
}

#' Is the argument a valid airport ICAO code?
#'
#' Checks if the supplied argument is a valid 4-letter airport ICAO code.
#'
#' @param x A character vector (possibly of length >1) of airport ICAO codes.
#' @return Returns TRUE for each element of x that is found in the database of airport ICAO codes.
#' @export
#' @seealso \code{\link{findApt}}, \code{\link{is.awy}}
is.apt <- function(x) {
    x %in% fltData$apt$icaoCode
}

#' Find airports in the database
#'
#' \code{findApt} finds airports in the database by searching for matches in the airport description. Internally it uses \code{grep}.
#'
#' @param x A character string containing a regular expression to be matched in the airport descriptions.
#' @return A data.frame of airports found, with columns: "icaoCode", "fullName", "elevation", "lat", "lon".
#' @examples
#' findApt("Makedonia")   # Find by description
#' findApt("LGTS")   # Find by ICAO code
#' @export
findApt <- function(x) {
  fltData$apt[unique(c(grep(x, fltData$apt$fullName, ignore.case=TRUE), grep(x, fltData$apt$icaoCode, ignore.case=TRUE))),]
}

