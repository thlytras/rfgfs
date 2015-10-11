# Functions that handle airway data

#' Is the argument a valid airway identifier?
#'
#' Checks if the supplied argument is an airway identifier found in the database.
#'
#' @param x A character vector (possibly of length >1) of airway identifiers.
#' @return Returns TRUE for each element of x that is found in the database of airways.
#' @export
#' @seealso \code{\link{findApt}}, \code{\link{is.apt}}
is.awy <- function(x) {
    x %in% fltData$awy$awy
}

#' Find airways in the database
#'
#' \code{findAwy} finds airways in the database that include the supplied navfix/navaid. Internally it uses \code{grep}.
#'
#' @param x A navaid/navfix as a character string.
#' @return A character vector with the name(s) of the airway(s) that include the supplied navaid/navfix. A vector of length zero if no matching airway is found.
#' @export
findAwy <- function(x) {
    unique(fltData$awy[sort(c(grep(x, fltData$awy$fix1, ignore.case=TRUE), grep(x, fltData$awy$fix2, ignore.case=TRUE))), "awy"])
}

#' Get airways data from the database
#'
#' \code{getAwy} gets detailed airways info from the database. Internally it uses \code{grep}.
#'
#' @param x An airway name (as a character string), or part thereof.
#' @param fix An optional navaid/navfix name. If given, only the airways that include this are returned.
#' @param exact If FALSE, x is matched partially (i.e. all airways that contain x in their name are returned. If TRUE, x is matched exactly (i.e. only airways that are exactly named x will be returned). Defaults to TRUE.
#' 
#' @return A list of data.frames. Each data.frame describes the segments of one airway, and its columns are: "fix1", "fix1Lat", "fix1Lon", "fix2", "fix2Lat", "fix2Lon", "isHigh", "baseFL", "topFL", "awy", "id1", "id2", "dist", "angle"
#' @export
getAwy <- function(x, fix=NA, exact=TRUE) {
    if (!(x %in% fltData$awy$awy)) stop(paste("No airway named", x, "in the database."))
    a <- if (exact) subset(fltData$awy, awy==x) else fltData$awy[grep(x, fltData$awy$awy),]
    edges <- table(append(a$id1, a$id2))
    edges <- names(edges[edges==1])
    awys <- list()
    while(length(edges)>0) {
        prev <- edges[1]; edges <- edges[-1]
        b <- subset(a, id1==prev | id2==prev)
        cur <- append(b$id1, b$id2)[append(b$id1, b$id2)!=prev]
        while(!(cur %in% edges)) {
            w <- subset(a, id1==cur | id2==cur)
            if (nrow(w)>1) w <- subset(w, id1!=prev & id2!=prev)
            b <- rbind(b,w)
            prev <- cur
            cur <- unique(append(w$id1, w$id2)[append(w$id1, w$id2)!=prev])
        }
        edges <- edges[-match(cur, edges)]
        awys[[length(awys)+1]] <- b
    }
    if (!is.na(fix)) awys <- awys[sapply(awys, function(x) fix %in% append(x$fix1,x$fix2))]
    awys
}
