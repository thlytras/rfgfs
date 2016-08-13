#' Update (fill-in) a flight plan.
#'
#' This function takes a data.frame of fixes and their coordinates and fills in the flight plan details
#' (courses, distances, airways, etc as in \code{\link{planRoute}}). This can be used to modify by hand
#' a plan created with \code{\link{planRoute}}, or even to manually create a plan from scratch (see examples).
#'
#' @param plan A flight plan, as a character vector (of fix names) or as a data.frame. Plans created with
#' \code{\link{planRoute}} are accepted as input. If a data.frame, it should have a minimum of three columns:
#' waypoint names, latitudes and longitudes. However, it is usually OK to have missing values (see details).
#' @param cols An optional vector of column names, if these are named differently in \code{plan}
#'
#' @details Provided the fix names are correct and exist in the database, it is not necessary to provide
#' their coordinates; thus \code{plan} can be a character vector of fix names, or a data.frame with all
#' \code{NA}s on the coordinate columns. Even for non-unique fix names, the correct one will be chosen
#' based on proximity to the nearest fixes. Conversely, if you provide coordinates (in a data.frame),
#' then fixes don't have to be named. However if fixes are not named, then determining the
#' ICAO airway between them is not possible.
#'
#' If you fly the Tu-154B, the flight plan created with \code{updateRoute} can be used with
#' \code{\link{planNVU}}, but if you have unnamed fixes you must use row numbers for the \code{points=} argument.
#'
#' @return Returns a data.frame containing the full flight plan, with the same columns as the one
#' returned by \code{\link{planRoute}}.
#'
#' @export
updateRoute <- function(plan, cols=c("fix","fixLat","fixLon")) {
  if (class(plan)=="character") {
    a <- data.frame(fix=plan, fixLat=NA, fixLon=NA)
  } else {
    a <- plan[,cols]
    names(a) <- c("fix","fixLat","fixLon")
  }

  # Filling in any missing coordinates
    for (i in which(is.na(a$fixLat) | is.na(a$fixLon))) {
      f <- findFixes(a$fix[i], type="all")
      if (nrow(f)==0) stop(sprintf("Fix '%s' not found in the database!", a$fix[i]))
      if (nrow(f)==1) { a$fixLat[i] <- f$lat; a$fixLon[i] <- f$lon }
    }
    nas <- which(is.na(a$fixLat) | is.na(a$fixLon))
    if (length(nas)==nrow(a)) stop("Ambiguous fix names. You must specify at least one coordinate pair.")
    while(length(nas)>0) {
      i <- nas[1]
      j <- if (nas[1]==1) 2 else (nas[1]-1)
      if (j %in% nas) {
        nas <- c(nas[-1], nas[1])
      } else {
        f <- findFixes(a$fix[i], refPoint = list(lon = a$fixLon[j], lat = a$fixLat[j]), type="all")
        a$fixLat[i] <- f$lat; a$fixLon[i] <- f$lon
        nas <- nas[-1]
      }
    }

  a$id <- with(a, paste(fix, round(fixLat-fixLon,1), sep="|"))
  result <- a

  # Find the airways
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
    routeAwys <- rbind(routeAwys, list(NA, NA,NA, NA, "STAR", NA, NA, NA))
    result <- cbind(result, routeAwys)
    if (is.na(result$awy[1])) result$awy[1] <- "SID"
    rownames(result) <- NULL

    result$dist[1:(nrow(result)-1)] <- sapply(1:(nrow(result)-1), function(i)
      spDistsN1(cbind(result$fixLon[i],result$fixLat[i]), cbind(result$fixLon[i+1],result$fixLat[i+1]), longlat=TRUE)
      )

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

