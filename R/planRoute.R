#' Create a flight plan route between two airports.
#'
#' This function determines the shortest route between two points (navfixes/navaids) 
#'
#' @param aptFrom Departure airport (4-letter ICAO code).
#' @param aptTo Destination airport (4-letter ICAO code).
#' @param fixFrom Initial departure navfix (after SID procedure). If not supplied manually, the navfix nearest to the departure airport will be chosen automatically, using \code{\link{getAptFix}}.
#' @param fixTo Final destination navfix (before STAR procedure). If not supplied manually, the navfix nearest to the destination airport will be chosen automatically, using \code{\link{getAptFix}}
#' @param fixes Optional vector of intermediate navfixes. Use these to speed up the algorithm, or to manually adjust the desired route.
#' @param limitArc If TRUE, only search points at an angle of ±90 degrees towards the destination point in order to speed up the algorithm.
#' @param narrowArc If TRUE (and also limitArc==TRUE), search at a narrower angle of ±45 degrees towards the destination point.
#' @return Returns a flight plan stored on a data.frame with the following columns:
#' \describe{
#'  \item{fix}{The name each point (airport/navaid/navfix) in the flight plan.}
#'  \item{freq}{Radio frequency (for navaids only).}
#'  \item{range}{Reception range (for navaids only).}
#'  \item{fullName}{Full name }
#'  \item{fixLat}{Latitude of the point.}
#'  \item{fixLon}{Longitude of the point.}
#'  \item{elevation}{Elevation (for navaids or airports).}
#'  \item{awy}{Airway identifier for the following segment (up to the next point in the plan), or SID/STAR.}
#'  \item{dist}{Distance (in km) (up to the next point in the plan).}
#'  \item{baseFL}{Base flight level for the following segment (up to the next point in the plan).}
#'  \item{topFL}{Top flight level for the following segment (up to the next point in the plan).}
#'  \item{course}{Course (true) towards the next point.}
#'  \item{altawy}{Alternative airway for the following segment, along with its permitted flight level range.}
#' }
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
      if (startFix$dist>300) stop("The requested fix lies too far (>300km) from the departure airport.\nPlease supply another one, or omit the argument to find one automatically.\n")
      startFix <- startFix$id
    }
    if (is.na(fixTo)) {
      # Automatically determine final fix
      endFix <- getAptFix(aptTo)
      if (is.na(endFix)) stop("Found no navigational fix close to the departure airport.\nPlease supply one manually.\n")
    } else {
      endFix <- subset(fltData$pts, fix==fixTo)
      if (nrow(endFix)==0) stop("Could not find the requested navigational fix for the departure airport in the database.\n")
      endFix$dist <- spDistsN1(cbind(endFix$fixLon,endFix$fixLat), as.matrix(fltData$apt[aptTo,c("lon","lat")]), longlat=TRUE)
      endFix <- subset(endFix, dist==min(dist))
      if (endFix$dist>300) stop("The requested fix lies too far (>300km) from the destination airport.\nPlease supply another one, or omit the argument to find one automatically.\n")
      endFix <- endFix$id
    }
    fixes <- c(startFix, fixes, endFix)
    if (length(fixes)>2) {
        for (i in 2:(length(fixes)-1)) {
            nextfixes <- subset(fltData$pts, fix==fixes[i])
            if (nrow(nextfixes)==0) stop(paste("Could not find", fixes[i], "in the database.\n"))
            if (nrow(nextfixes)>1) {
                nextfixes$dist <- spDistsN1(cbind(nextfixes$fixLon,nextfixes$fixLat), 
                        as.matrix(fltData$pts[match(fixes[i-1], fltData$pts$id),c("fixLon","fixLat")]), longlat=TRUE)
                nextfixes <- subset(nextfixes, dist==min(dist))
            }
            fixes[i] <- nextfixes$id
        }
    }
    route <- c(fixes[1], unlist(lapply(2:length(fixes), function(i) fix2fix(fixes[i-1], fixes[i], limitArc, narrowArc)[-1])))
    result <- fltData$pts[match(route,fltData$pts$id),]
    routeAwys <- apply(cbind(result$id[-nrow(result)], result$id[-1]), 1, function(x) subset(fltData$dists, id1==x[1] & id2==x[2]))
    for (i in 1:length(routeAwys)) {
      if (nrow(routeAwys[[i]])>1) {
        awy_to_keep <- which(routeAwys[[i]]$awy==routeAwys[[2]]$awy)[1]
        if (is.na(awy_to_keep)) awy_to_keep <- with(routeAwys[[i]], which(topFL==max(topFL)))[1]
	routeAwys[[i]]$altawy <- with(routeAwys[[i]][-awy_to_keep,], 
	  paste(paste(awy, "@FL", baseFL, "-FL", topFL, sep=""), collapse=", "))
	routeAwys[[i]] <- routeAwys[[i]][awy_to_keep,]
      } else { routeAwys[[i]]$altawy <- NA }
    } 
    routeAwys <- do.call(rbind, routeAwys)
    routeAwys$course <- routeAwys$course*180/pi
    navFreqs <- lapply(1:nrow(result), function(i) {
      if (nchar(result$fix[i])<4) {
	v <- subset(fltData$nav$VORDME, id==result$fix[i])
	if (nrow(v)>1) {
	  dist <- spDistsN1(cbind(v$lon, v$lat), cbind(result$fixLon[i], result$fixLat[i]))
	  v <- v[dist==min(dist),]
	}
	if (nrow(v)==0) {
	  v <- subset(fltData$nav$NDB, id==result$fix[i])
	  if (nrow(v)>1) {
	    dist <- spDistsN1(cbind(v$lon, v$lat), cbind(result$fixLon[i], result$fixLat[i]))
	    v <- v[dist==min(dist),]
	  }
	  if (nrow(v)>0) return(list(freq=v$freq, range=v$range, fullName=v$name, elevation=v$elev))
	} else {
	  return(list(freq=sprintf("%.2f", v$freq/100), range=v$range, fullName=v$name, elevation=v$elev))
	}
      }
      list(freq=NA, range=NA, fullName=NA, elevation=NA)
    })
    result <- cbind(result, do.call(rbind,navFreqs))
    result$freq <- as.numeric(result$freq)
    result$range <- as.integer(result$range)
    result$fullName <- as.character(result$fullName)
    result$elevation <- as.integer(result$elevation)
    result <- result[,c("fix", "freq", "range", "fullName", "fixLat","fixLon", "elevation")]
    routeAwys <- routeAwys[,c("awy", "dist", "baseFL", "topFL", "course", "altawy")]
    endRoute <- unlist(cbind(subset(fltData$pts, id == endFix)[c("fixLat", "fixLon")], 
	subset(fltData$apt, icaoCode==aptTo)[c("lat", "lon")]))
    names(endRoute) <- c("lat1", "lon1", "lat2", "lon2")
    startRoute <- unlist(cbind(subset(fltData$apt, icaoCode==aptFrom)[c("lat", "lon")], 
	subset(fltData$pts, id == startFix)[c("fixLat", "fixLon")]))
    names(startRoute) <- c("lat1", "lon1", "lat2", "lon2")
    routeAwys <- rbind(routeAwys, c("STAR", 
	spDistsN1(t(endRoute[2:1]), t(endRoute[4:3]), longlat=TRUE), 
	NA, NA, do.call(gcb, as.list(endRoute))*180/pi, NA))
    result <- cbind(result, routeAwys)
    result <- result[c(NA, 1:nrow(result), NA), ]
    result$dist <- as.numeric(result$dist)
    result$course <- as.numeric(result$course)
    result[1,] <- list(aptFrom, NA, NA, subset(fltData$apt, icaoCode==aptFrom)$fullName, startRoute[1],
            startRoute[2], subset(fltData$apt, icaoCode==aptFrom)$elevation, "SID", 
	    spDistsN1(t(startRoute[2:1]), t(startRoute[4:3]), longlat=TRUE), NA, NA, 
	    do.call(gcb, as.list(startRoute))*180/pi, NA)
    result[nrow(result),] <- list(aptTo, NA, NA, subset(fltData$apt, icaoCode==aptTo)$fullName,
            endRoute[3], endRoute[4], subset(fltData$apt, icaoCode==aptTo)$elevation, NA, NA, NA, NA, NA, NA)
    rownames(result) <- NULL
    h <- as.integer(result$baseFL)*1000; h[is.na(h)] <- 0
    result$magdecl <- mapply(magvar, result$fixLat, result$fixLon, h)
    result$magcourse <- result$course - result$magdecl
    result$magcourse[result$magcourse>360] <- result$magcourse[result$magcourse>360] - 360
    result$magcourse[result$magcourse<0] <- result$magcourse[result$magcourse<0] + 360
    result[,c(1:12,14,15,13)] # Rearrange columns so altawy comes last
}

