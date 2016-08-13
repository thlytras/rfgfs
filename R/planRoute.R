#' Create a flight plan route between two airports.
#'
#' This function determines the shortest route between two points (navfixes/navaids)
#'
#' @param aptFrom Departure airport (4-letter ICAO code).
#' @param aptTo Destination airport (4-letter ICAO code).
#' @param fixFrom Initial departure navfix (after SID procedure). If not supplied manually, the navfix nearest to the departure airport will be chosen automatically, using \code{\link{getAptFix}}.
#' @param fixTo Final destination navfix (before STAR procedure). If not supplied manually, the navfix nearest to the destination airport will be chosen automatically, using \code{\link{getAptFix}}
#' @param fixes Optional vector of intermediate navfixes. Use these to speed up the algorithm,
#'          or to manually adjust the desired route. If \code{NA}s are inserted between two navfixes,
#'          then the route is flown straight between them, without following ICAO runways.
#' @param limitArc If TRUE, only search points at an angle of ±90 degrees towards the destination point in order to speed up the algorithm.
#' @param narrowArc If TRUE (and also limitArc==TRUE), search at a narrower angle of ±45 degrees towards the destination point.
#' @return Returns a flight plan stored on a data.frame with the following columns:
#' \describe{
#'  \item{fix}{The name each point (airport/navaid/navfix) in the flight plan.}
#'  \item{fixLat}{Latitude of the point.}
#'  \item{fixLon}{Longitude of the point.}
#'  \item{fullName}{Full name }
#'  \item{freq}{Radio frequency (for navaids only).}
#'  \item{range}{Reception range (for navaids only).}
#'  \item{elevation}{Elevation (for navaids or airports).}
#'  \item{dev}{Deviation (for VOR stations).}
#'  \item{magdecl}{Magnetic declination at the point.}
#'  \item{dist}{Distance (in km, up to the next point in the plan).}
#'  \item{course}{Course (true) towards the next point.}
#'  \item{magcourse}{Course (magnetic) towards the next point.}
#'  \item{awy}{Airway identifier for the following segment (up to the next point in the plan), or SID/STAR.}
#'  \item{baseFL}{Base flight level for the following segment (up to the next point in the plan).}
#'  \item{topFL}{Top flight level for the following segment (up to the next point in the plan).}
#'  \item{altawy}{Alternative airway for the following segment, along with its permitted flight level range.}
#' }
#'
#' @examples
#' # Fly from Athens to Thessaloniki
#' plan1 <- planRoute("LGAV", "LGTS")
#' print(plan1)
#'
#' # Manually choose appropriate departure and approach fixes
#' # (by referring to the relevant approach plates)
#' plan2 <- planRoute("LGAV", "LGTS", fixFrom="ABLON", fixTo="LEKPO")
#' print(plan2)
#'
#' # Fly from Athens to Innsbrück
#' plan3 <- planRoute("LGAV", "LOWI", "KOR", "BRENO")
#'
#' # Follow a different route, via Thessaloniki and Belgrade
#' plan4 <- planRoute("LGAV", "LOWI", "ABLON", "RTT", fixes=c("TSL","ORVAN"))
#'
#' # Divert a bit from the ICAO airway (OMIRO -> SKP -> BAMOS -> AGISA)
#' # and fly over PELAS instead, i.e. over Alonissos instead of Skopelos island.
#' plan5 <- planRoute("LGAV", "LOWI", "ABLON", "RTT",
#'                    fixes=c("OMIRO", NA, "PELAS", NA, "AGISA", "TSL","ORVAN"))
#'
#' @export
planRoute <- function(aptFrom, aptTo, fixFrom=NA, fixTo=NA, fixes=character(), limitArc=TRUE, narrowArc=FALSE) {
  # This function is based on Djisktra's algorithm. fltData must be loaded

  # Initial sanity checks, and determine initial and final fixes
    aptFrom <- gsub("^\\s+|\\s+$", "", aptFrom)
    aptTo <- gsub("^\\s+|\\s+$", "", aptTo)
    fixFrom <- gsub("^\\s+|\\s+$", "", fixFrom)
    fixTo <- gsub("^\\s+|\\s+$", "", fixTo)
    if (!(aptFrom %in% fltData$apt$icaoCode)) stop("Departure airport does not exist in database")
    if (!(aptTo %in% fltData$apt$icaoCode)) stop("Destination airport does not exist in database")
    if (is.na(fixFrom)) {
      # Automatically determine initial fix
      startFix <- getAptFix(aptFrom)
      if (is.na(startFix)) stop("Found no navigational fix close to the departure airport.\nPlease supply one manually.\n")
    } else {
      startFix <- subset(fltData$pts, fix==fixFrom)
      if (nrow(startFix)==0) stop("Could not find the requested navigational fix for the departure airport in the database.\n")
      startFix$dist <- spDistsN1(cbind(startFix$fixLon,startFix$fixLat), as.matrix(fltData$apt[aptFrom,c("lon","lat")]), longlat=TRUE)
      startFix <- subset(startFix, dist==min(dist))
      if (startFix$dist>300) warning("The requested fix lies too far (>300km) from the departure airport.")
      startFix <- startFix$id
    }
    if (is.na(fixTo)) {
      # Automatically determine final fix
      endFix <- getAptFix(aptTo)
      if (is.na(endFix)) stop("Found no navigational fix close to the arrival airport.\nPlease supply one manually.\n")
    } else {
      endFix <- subset(fltData$pts, fix==fixTo)
      if (nrow(endFix)==0) stop("Could not find the requested navigational fix for the arrival airport in the database.\n")
      endFix$dist <- spDistsN1(cbind(endFix$fixLon,endFix$fixLat), as.matrix(fltData$apt[aptTo,c("lon","lat")]), longlat=TRUE)
      endFix <- subset(endFix, dist==min(dist))
      if (endFix$dist>300) warning("The requested fix lies too far (>300km) from the arrival airport.")
      endFix <- endFix$id
    }
    fixes <- c(startFix, fixes, endFix)

  # Convert everything to id's
    lastfix <- fixes[1]
    if (length(fixes)>2) {
      for (i in 2:(length(fixes)-1)) {
        if (is.na(fixes[i])) next
        nextfixes <- subset(fltData$pts, fix==fixes[i])
        if (nrow(nextfixes)==0) stop(paste("Could not find", fixes[i], "in the database.\n"))
        if (nrow(nextfixes)>1) {
          nextfixes$dist <- spDistsN1(cbind(nextfixes$fixLon,nextfixes$fixLat),
                                      as.matrix(fltData$pts[match(lastfix, fltData$pts$id),c("fixLon","fixLat")]), longlat=TRUE)
          nextfixes <- subset(nextfixes, dist==min(dist))
        }
        fixes[i] <- nextfixes$id
        lastfix <- fixes[i]
      }
    }

  # Calculate the routes where applicable
    lastfix <- fixes[1]
    route <- c()
    for (i in 2:length(fixes)) {
      if (is.na(fixes[i])) {
        if (!is.na(lastfix)) {
          route <- c(route, lastfix)
        }
        lastfix <- NA
      } else {
        if (!is.na(lastfix)) {
          route <- c(route, rev(rev(fix2fix(lastfix, fixes[i], limitArc, narrowArc))[-1]))
        }
        lastfix <- fixes[i]
      }
    }
    route <- c(route, fixes[length(fixes)])

  # Find the airways
    result <- fltData$pts[match(route,fltData$pts$id),]
    routeAwys <- apply(cbind(result$id[-nrow(result)], result$id[-1]), 1, function(x) subset(fltData$dists, id1==x[1] & id2==x[2]))

    for (i in 1:length(routeAwys)) {
      if (nrow(routeAwys[[i]])==0) {
        routeAwys[[i]][1,] <- NA
      }
      if (nrow(routeAwys[[i]])>1) {
        awy_to_keep <- which(routeAwys[[i]]$awy==routeAwys[[2]]$awy)[1]
        if (is.na(awy_to_keep)) {
          awy_to_keep <- with(routeAwys[[i]], which(topFL==max(topFL)))[1]
        }
        routeAwys[[i]]$altawy <- with(routeAwys[[i]][-awy_to_keep,],
                paste(paste(awy, "@FL", baseFL, "-FL", topFL, sep=""), collapse=", "))
	      routeAwys[[i]] <- routeAwys[[i]][awy_to_keep,]
      } else {
        routeAwys[[i]]$altawy <- NA
      }
    }

    routeAwys <- do.call(rbind, routeAwys)
    routeAwys$course <- NA   # Will recalculate everything later
    routeAwys$magcourse <- NA; routeAwys$magdecl <- NA

    navFreqs <- do.call(rbind, lapply(1:nrow(result), function(i){
      fr <- findFixes(result$fix[i], list(
        lon = result$fixLon[i],
        lat = result$fixLat[i]
      ))
      if (nrow(fr)==0) fr[1,] <- NA
      fr[1,]
    }))

    navFreqs <- navFreqs[,c("name","freq","range","elev","dev")]
    names(navFreqs)[c(1,4)] <- c("fullName", "elevation")
    result <- cbind(result[,-4], navFreqs)

    routeAwys <- routeAwys[,c("magdecl", "dist", "course", "magcourse", "awy", "baseFL", "topFL", "altawy")]

    endRoute <- unlist(cbind(subset(fltData$pts, id == endFix)[c("fixLat", "fixLon")],
	        subset(fltData$apt, icaoCode==aptTo)[c("lat", "lon")]))
    names(endRoute) <- c("lat1", "lon1", "lat2", "lon2")
    startRoute <- unlist(cbind(subset(fltData$apt, icaoCode==aptFrom)[c("lat", "lon")],
	        subset(fltData$pts, id == startFix)[c("fixLat", "fixLon")]))
    names(startRoute) <- c("lat1", "lon1", "lat2", "lon2")

    routeAwys <- rbind(routeAwys,
                       list(NA,
                            spDistsN1(t(endRoute[2:1]), t(endRoute[4:3]), longlat=TRUE),
                            NA, NA, "STAR", NA, NA, NA))
    result <- cbind(result, routeAwys)
    result <- result[c(NA, 1:nrow(result), NA), ]

    result[1,] <- list(
          aptFrom, startRoute[1], startRoute[2],
          subset(fltData$apt, icaoCode==aptFrom)$fullName, NA, NA,
          subset(fltData$apt, icaoCode==aptFrom)$elevation, NA, NA,
          spDistsN1(t(startRoute[2:1]), t(startRoute[4:3]), longlat=TRUE),
          NA, NA, "SID", NA, NA, NA)
    result[nrow(result),] <- list(
          aptTo, endRoute[3], endRoute[4],
          subset(fltData$apt, icaoCode==aptTo)$fullName, NA, NA,
          subset(fltData$apt, icaoCode==aptTo)$elevation,
          NA, NA, NA, NA, NA, NA, NA, NA, NA)

    rownames(result) <- NULL

    result$course[-nrow(result)] <- apply(
      cbind(result[-nrow(result),2:3], result[-1,2:3]), 1,
      function(x) gcb(x[1],x[2],x[3],x[4]))
    result$dev[which(result$dev>180)] <- result$dev[which(result$dev>180)] - 360

    h <- as.integer(result$baseFL)*1000; h[is.na(h)] <- 0
    result$magdecl <- mapply(magvar, result$fixLat, result$fixLon, h)
    result$magcourse <- result$course - result$magdecl
    result$magcourse[which(result$magcourse>360)] <- result$magcourse[which(result$magcourse>360)] - 360
    result$magcourse[which(result$magcourse<0)] <- result$magcourse[which(result$magcourse<0)] + 360

    result
}

