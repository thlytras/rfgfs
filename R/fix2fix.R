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
    angDiff <- function(a, b) {
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
        subset(fltData$pts, id==endFix)$fixLat, subset(fltData$pts, id==endFix)$fixLon)
    a <- subset(fltData$dists, id1==visited[length(visited)])	# Get distances to all neighbours...
    a <- subset(a, !(id2 %in% visited))	# ...provided not already visited
    if (limitArc) {
        if (narrowArc) b <- subset(a, abs(angDiff(course, curAngle))<(pi/4))	# Filter out nodes
        if (!narrowArc || nrow(b)==0) b <- subset(a, abs(angDiff(course, curAngle))<(pi/2))	# Filter out nodes
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


# Great circle bearing
gcb <- function(lat1, lon1, lat2, lon2) atan2(sin((lon2-lon1)*pi/180)*cos(lat2*pi/180), 
    cos(lat1*pi/180)*sin(lat2*pi/180)-sin(lat1*pi/180)*cos(lat2*pi/180)*cos((lon2-lon1)*pi/180)) %% (2*pi)
