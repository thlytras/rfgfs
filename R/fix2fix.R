#' Find the shortest route between two navfixes/navaids
#'
#' This function determines the shortest route between two points (navfixes/navaids)
#'
#' @param startFix Identifier for departure navfix/navaid.
#' @param endFix Identifier for destination navfix/navaid.
#' @param limitArc If TRUE, only search points at an angle of ±90 degrees towards the destination point in order to speed up the algorithm.
#' @param narrowArc If TRUE (and also limitArc==TRUE), search at a narrower angle of ±45 degrees towards the destination point.
#' @return Returns a vector of identifiers corresponding to the determined route.
fix2fix <- function(startFix, endFix, limitArc=TRUE, narrowArc=FALSE) {
# Determine difference of two angles (in radians)
    angDiffR <- function(a, b) {
        d <- a-b
        d[d>pi] <- d[d>pi]-(2*pi)
        d
    }
# This function is based on Djisktra's algorithm.
# Set of visited nodes, and put start as first visited node
    visited <- startFix
# Tentative distances (start node =0 and all others =Inf)
    tdists <- rep(Inf, nrow(fltData$pts))
    names(tdists) <- fltData$pts$id
    tdists[startFix] <- 0
# Keep track of which node the shortest distance came from
    tdists_from <- rep(startFix, nrow(fltData$pts))
    names(tdists_from) <- fltData$pts$id
# Initialize algorithm
    curdist <- 0	# visited[length(visited)] is the current node
# Iterate
    while(TRUE) {
    curAngle <- gcb(subset(fltData$pts, id==visited[length(visited)])$fixLat,
        subset(fltData$pts, id==visited[length(visited)])$fixLon,
        subset(fltData$pts, id==endFix)$fixLat, subset(fltData$pts, id==endFix)$fixLon) * pi/180
    a <- subset(fltData$dists, id1==visited[length(visited)])	# Get distances to all neighbours...
    a <- subset(a, !(id2 %in% visited))	# ...provided not already visited
    if (limitArc) {
        if (narrowArc) b <- subset(a, abs(angDiffR(course, curAngle))<(pi/4))	# Filter out nodes
        if (!narrowArc || nrow(b)==0) b <- subset(a, abs(angDiffR(course, curAngle))<(pi/2))	# Filter out nodes
        if (nrow(b)==0) b <- a	# Filter out nodes
        a <- b
    }
    rep1 <- a$dist+curdist	# Add the current tentative distance
    names(rep1) <- a$id2
    tdists_from[names(rep1)][rep1 < tdists[names(rep1)]] <- visited[length(visited)]
    tdists[names(rep1)] <- pmin(rep1, tdists[names(rep1)])	# Update tentative distances if lower
    curdist <- tdists[which(tdists==min(tdists[tdists>0 & !(names(tdists) %in% visited)]))]	# Find the next unvisited node with the shortest tentative distance
    visited <- c(visited, names(curdist))	# Add it to the list of next unvisited nodes
    if (endFix %in% visited) break
    }
# Finalize result
    route <- endFix
    while(route[length(route)] != startFix){
    route <- c(route, tdists_from[route[length(route)]])
    }
    route <- rev(route)
    names(route) <- NULL
    route
}



#' Find the initial course for an orthodromic route
#'
#' This function returs the initial course heading (in degrees) for an orthodromic course between two points.
#'
#' @param lat1 Latitude of departure point
#' @param lon1 Longitude of departure point
#' @param lat2 Latitude of destination point
#' @param lon2 Longitude of destination point
#' @return Returns the initial orthodromic course in degrees.
#'
#' @export
gcb <- function(lat1, lon1, lat2, lon2) (atan2(sin((lon2-lon1)*pi/180)*cos(lat2*pi/180),
    cos(lat1*pi/180)*sin(lat2*pi/180)-sin(lat1*pi/180)*cos(lat2*pi/180)*cos((lon2-lon1)*pi/180)) %% (2*pi)) * 180/pi



#' Determine difference between two course headings
#'
#' This function returs the difference (in degrees) between two course headings.
#' A negative sign means that a is to the left of b, a positive sign means that a is to the right of b.
#' Obviously the two course headings should be on the same reference, e.g. both magnetic or both true.
#'
#' @param a,b Course headings, in degrees
#' @return The difference a - b, in degrees
#'
#' @export
courseDiff <- function(a, b) {
  d <- (a - b) %% 360
  sign <- ifelse((a-b>=0 & a-b<=180) | (a-b<=-180 & a-b>=-360), 1, -1)
  ifelse(d>180, 360-d, d)*sign
}



#' Find navigational fix(es) by name
#'
#' This function finds navigational fixes (fixes or navaids) by name (id). If there are many fixes
#' with the same name, they are all returned unless an optional reference point is supplied, in which
#' case only the fix nearest to the reference point is returned.
#'
#' @param x Fix id to search for.
#' @param refPoint Optional reference point. A list containing elements \code{lat} and \code{lon},
#' holding the latitude and longitude in degrees.
#' @return A data.frame (with zero rows if no fix is found) with columns \code{lat}, \code{lon}, \code{elev},
#' \code{freq}, \code{range}, \code{id}, \code{name}, \code{type}.
#'
#' @export
findFixes <- function(x, refPoint=NULL) {
  fix <- subset(fltData$fix, fix==x)
  navNDB <- subset(fltData$nav$NDB, id==x)
  navVOR <- subset(fltData$nav$VOR, id==x)
  navTACAN <- subset(fltData$nav$TACAN, id==x)
  navRSBN <- subset(fltData$nav$RSBN, id==x)
  if (nrow(fix)==0) fix[1,] <- rep(NA, ncol(fix))
  if (nrow(navNDB)==0) navNDB[1,] <- rep(NA, ncol(navNDB))
  if (nrow(navVOR)==0) navVOR[1,] <- rep(NA, ncol(navVOR))
  if (nrow(navTACAN)==0) navTACAN[1,] <- rep(NA, ncol(navTACAN))
  if (nrow(navRSBN)==0) navRSBN[1,] <- rep(NA, ncol(navRSBN))
  names(fix)[3] <- "id"
  fix$elev <- NA; fix$freq <- NA; fix$range <- NA; fix$name <- NA
  fix$type <- "fix"; navNDB$type <- "NDB";
  navVOR$type <- "VOR"; navTACAN$type <- "TACAN"; navRSBN$type <- "RSBN"
  fixes <- rbind(fix[,c(1:2, 4:6, 3, 7:8)], navNDB[,-6], navVOR[,-6],
                 navTACAN[,-6], navRSBN[,-8])
  fixes <- fixes[!is.na(fixes$lat),]
  j <- c()
  for(i in which(fixes$type == "TACAN")) {
    if (length(which(abs(fixes$lat-fixes$lat[i])<0.01 & abs(fixes$lon-fixes$lon[i])<0.01 & fixes$type=="VOR"))>0) j <- c(j, i)
  }
  if (length(j)>0) fixes <- fixes[-j,]
  if (nrow(fixes)>0 && !is.null(refPoint)) {
    if (is.null(names(refPoint))) names(refPoint)[1:2] <- c("lat","lon")
    dists <- mapply(function(lat, lon){
      spDistsN1(cbind(lon, lat), cbind(refPoint$lon, refPoint$lat), longlat=TRUE)
    }, fixes$lat, fixes$lon)
    fixes <- fixes[which(dists==min(dists)),]
  }
  fixes
}

