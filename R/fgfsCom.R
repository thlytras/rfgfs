#' Connect to FlightGear via telnet
#'
#' This function creates a socket connection to the FlightGear props (telnet)
#' interface, via which the property tree can be queried and/or modified.
#'
#' @param host Hostname/IP address of the computer running FlightGear and
#' the props interface (default: localhost).
#' @param port Port that the props server is listening on (default: 5501).
#'
#' @details As soon as the connection is opened, the interface is switched
#' to bulk data mode, and a help commend is issued. If FlightGear does not
#' respond within 5 seconds with the expected help message, the function
#' quits with an error. Otherwise the connection object is returned.
#'
#' @return A (socket) connection object, used with other package functions to
#' control the simulator. It is your responsibility to \code{\link{close}} it
#' when you don't need it anymore.
#'
#' @export
fgfsConnect <- function(host = "localhost", port = 5501) {
  con <- socketConnection(host, port)
  # Switch to raw data mode
  writeLines("data\r\n", con)
  # Test if help command gives the expected 15 lines of output
  writeLines("help\r\n", con)
  t0 <- proc.time()[3]
  l <- 0
  while ((proc.time()[3]-t0) < 5) {
    out <- readLines(con)
    l <- l + length(out)
    if (l>=15) return(con) # We're OK. Returning the connection object.
  }
  stop("Connection opened, but server did not respond to 'help' command as expected.\nIs this really the fgfs telnet server?")
}



#' Get (read) FlightGear property via telnet
#'
#' This function reads a property from the FlightGear property tree, via
#' the props (telnet) interface
#'
#' @param con A socket connection object created with \code{\link{fgfsConnect}}
#' @param var The property to read.
#' @param wait Maximum time waiting for a server response.
#'
#' @return The value of the property. This is always a string. If the
#' property does not exist on the tree, an empty string is returned. If the
#' server does not respond, a character vector of length zero is returned.
#'
#' @export
fgfsGet <- function(con, var, wait=2) {
  readLines(con) # Clean any previous output
  writeLines(sprintf("get %s \r\n", var), con)
  t0 <- proc.time()[3]
  while ((proc.time()[3]-t0)<wait) {
    out <- readLines(con)
    # We're reading simple properties, so we're just expecting one line of output
    if (length(out)>0) break
  }
  return(out)
}



#' Set FlightGear property via telnet
#'
#' This function sets a FlightGear property via the props (telnet) interface.
#'
#' @param con A socket connection object created with \code{\link{fgfsConnect}}
#' @param var The name of the property.
#' @param val The value to set.
#'
#' @details This value does no error checking. If the property you try to
#' set does not exist on the tree, failure is silent.
#'
#' @return None.
#'
#' @export
fgfsSet <- function(con, var, val) {
  # No need to wait for anything here
  writeLines(sprintf("set %s %s \r\n", var, val), con)
}


