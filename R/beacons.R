
#' Find beacon(s) in the database
#'
#' \code{findBeacon} finds navigational beacons in the database by searching for matches in the navaid id or name.
#' Internally it uses \code{grep}.
#'
#' @param x A character string containing a regular expression to be matched in the navaid ids or names.
#' @param type Type(s) of beacons to be returned. By default, return all possible beacons (VORs, NDBs, TACANs and RSBNs).
#' @param exact.id Match only on beacon id (as exact string).
#' @return A data.frame of beacons found, with columns: \code{lat}, \code{lon}. \code{elev}, \code{freq}, \code{id}, \code{name}.
#' @export
findBeacon <- function(x, type=c("VOR","NDB","TACAN","RSBN"), exact.id=FALSE) {
  beacons <- if (exact.id) { list(
    VOR = fltData$nav$VOR[which(fltData$nav$VOR$id==toupper(x)), -6],
    NDB = fltData$nav$NDB[which(fltData$nav$NDB$id==toupper(x)), -6],
    TACAN = fltData$nav$TACAN[which(fltData$nav$TACAN$id==toupper(x)), -6],
    RSBN = fltData$nav$RSBN[which(fltData$nav$RSBN$id==toupper(x)), -8]
  )} else { list(
    VOR = fltData$nav$VOR[unique(c(grep(x, fltData$nav$VOR$name, ignore.case=TRUE), grep(x, fltData$nav$VOR$id, ignore.case=TRUE))), -6],
    NDB = fltData$nav$NDB[unique(c(grep(x, fltData$nav$NDB$name, ignore.case=TRUE), grep(x, fltData$nav$NDB$id, ignore.case=TRUE))), -6],
    TACAN = fltData$nav$TACAN[unique(c(grep(x, fltData$nav$TACAN$name, ignore.case=TRUE), grep(x, fltData$nav$TACAN$id, ignore.case=TRUE))), -6],
    RSBN = fltData$nav$RSBN[unique(c(grep(x, fltData$nav$RSBN$name, ignore.case=TRUE), grep(x, fltData$nav$RSBN$id, ignore.case=TRUE))), -8]
  )}
  do.call(rbind, beacons[type])
}

