#' Obtain NVU navigational data for a flight plan
#'
#' This function gets a flight plan as input (i.e. a list of waypoints and
#' geographical coordinates) and calculates appropriate data to feed the
#' NVU navigational computer of the Tu-154B.
#'
#' @param plan A flight plan, as a data.frame. Plans created with \code{\link{planRoute}}
#' are accepted as input. At a minimum, the data frame.should have three columns:
#' waypoint names, latitudes and longitudes.
#' @param points An optional vector of row numbers or waypoint names to select from the flight plan.
#' @param cols An optional vector of column names, if these are named differently in \code{plan}
#' @param init.mag At the initial waypoint, does the HSI show magnetic heading (instead of true heading)?
#' @param merge Merge the NVU data into \code{plan}?
#'
#' @return If \code{merge = FALSE}, returns a matrix with columns \code{ZPY}, \code{S} and \code{da}.
#' \code{ZPY} (orthodromic HSI course heading) and \code{S} (leg distance, negated) can be fed
#' to the NVU panels. \code{da} is the azimuth correction at each waypoint.
#'
#' If \code{merge = TRUE} the above columns are added to the data.frame \code{plan}.
#'
#' @export
planNVU <- function(plan, points=NULL, cols=c("fix","fixLat","fixLon"), init.mag=FALSE, merge=TRUE) {
  if (is.null(points)) points <- 1:nrow(plan)
  if (sum(points<0)) points <- (1:nrow(plan))[points]
  if (is.character(points)) points <- match(points, plan$fix)
  a <- plan[points, cols]
  names(a) <- c("fix","fixLat","fixLon")
  if (init.mag) da <- -magvar(a$fixLat[1], a$fixLon[1]) else da <- 0
  da0 <- da
  res <- t(sapply(1:(nrow(a)-1), function(i) {
    ZPY <- (gcb(a$fixLat[i], a$fixLon[i], a$fixLat[i+1], a$fixLon[i+1]) + da) %% 360
    ZPYinv <- (180 + gcb(a$fixLat[i+1], a$fixLon[i+1], a$fixLat[i], a$fixLon[i])) %% 360
    S <- -spDistsN1(cbind(a$fixLon[i],a$fixLat[i]), cbind(a$fixLon[i+1],a$fixLat[i+1]), longlat=TRUE)
    da <<- courseDiff(ZPY, ZPYinv)
    list(ZPY=ZPY, S=S, Da=da)
  }))
  res <- rbind(res, c(0,0,0))
  res[,3] <- c(da0, res[,3][1:(nrow(res)-1)])
  if (!merge) return(res)
  plan$ZPY <- NA; plan$S <- NA; plan$Da <- NA
  plan$ZPY[points] <- unlist(res[,"ZPY"])
  plan$S[points] <- unlist(res[,"S"])
  plan$Da[points] <- unlist(res[,"Da"])
  return(plan)
}



#' Calculate RSBN course correction
#'
#' This function returns the NVU course correction parameters (Zm, Sm, YK) given the latitude and longitude of
#' the start point, the end point and the RSBN beacon.
#'
#' This function is used by \code{corrPlanRSBN()}.
#'
#' @param lat1,lon1 Latitude and longitude of the start point
#' @param lat2,lon2 Latitude and longitude of the end point
#' @param latB,lonB Latitude and longitude of the beacon
#' @return A numeric vector of length 3, containing the values for Zm, Sm and YK.
#'
#' @export
corrRSBN <- function(lat1, lon1, lat2, lon2, latB, lonB) {
  # Get orthodromic coordinates Z and S of the beacon, plus S0 (negative leg distance)
  Bc <- orthoCoords(lat1, lon1, lat2, lon2, latB, lonB)

  f <- (Bc["S"] - Bc["S0"])/abs(Bc["S0"])
  d <- (Bc["S0"]/1.852)*(pi/(180*60))

  A <- sin((1-f)*d)/sin(d)
  B <- sin(f*d)/sin(d)
  x <- A*cos(lat1*pi/180)*cos(lon1*pi/180) + B*cos(lat2*pi/180)*cos(lon2*pi/180)
  y <- A*cos(lat1*pi/180)*sin(lon1*pi/180) + B*cos(lat2*pi/180)*sin(lon2*pi/180)
  z <- A*sin(lat1*pi/180) + B*sin(lat2*pi/180)
  lat <- atan2(z,sqrt(x^2+y^2))*180/pi
  lon <- atan2(y,x)*180/pi
  YK <- gcb(lat, lon, lat2, lon2)
  if (Bc["S"]>0) YK <- (180 + YK) %% 360

  c(
    Zm = as.numeric(Bc["Z"]),
    Sm = as.numeric(Bc["S"]),
    YK = as.numeric(YK)
  )
}



#' RSBN course correction(s) for a flight plan
#'
#' This function returns the NVU course correction parameters (Zm, Sm, YK)  for the various
#' legs of a flight plan.
#'
#' @param plan A flight plan, as a data.frame. Plans created with \code{\link{planNVU}(..., merge=TRUE)}
#' are accepted as input. At a minimum, the data frame.should have four columns:
#' waypoint names, latitudes, longitudes and ZPY (NVU course). The latter is needed to indirectly
#' identify the NVU legs, which may not correspond 1:1 to the legs of the flight plan (see
#' \code{points=} in \code{\link{planNVU}}).
#'
#' @param bcLeg A named list of flight plan legs to be corrected by each RSBN beacon. Each list element
#' should be a vector of flight plan legs, specified either as numbers or as initial fixes, and named
#' after the beacon ID that should be used for correction. Because RSBN beacon IDs are not always unique,
#' optionally append the two digits of the channel number to the ID in order to specify the desired beacon.
#'
#' If \code{bcLeg=NULL}, all beacons that lie a distance of \code{dlim} kilometers from the flight path
#' are used to correct each segment.
#'
#' @param dlim Use all beacons that lie \code{dlim} kilometers from the flight path, if \code{bcLeg=NULL}.
#'
#' @param cols An optional vector of column names, if these are named differently in \code{plan}
#'
#' @return A data.frame of leg-correction pairs. Each row contains the flight plan leg number, start and
#' end fix, the name, ID and channel of the RSBN beacon, and the correction parameters (Zm, Sm and YK), i.e.
#' columns \code{Leg}, \code{From}, \code{To}, \code{beaconName}, \code{bcID}, \code{bcChannel}, \code{Zm},
#' \code{Sm} and \code{YK}.
#'
#' @export
corrPlanRSBN <- function(plan, bcLeg=NULL, dlim=150, cols=c("fix","fixLat","fixLon", "ZPY")) {
  a <- plan[, cols]
  names(a) <- c("fix","fixLat","fixLon","ZPY")
  a <- subset(a, !is.na(ZPY))
  if (is.null(bcLeg)) {
    b <- do.call(rbind, lapply(1:(nrow(a)-1), function(i){
        ld <- spDistsN1(cbind(a$fixLon[i],a$fixLat[i]), cbind(a$fixLon[i+1],a$fixLat[i+1]), longlat=TRUE)
        d1 <- with(fltData$nav$RSBN, spDistsN1(cbind(lon,lat), cbind(a$fixLon[i],a$fixLat[i]), longlat=TRUE))
        d2 <- with(fltData$nav$RSBN, spDistsN1(cbind(lon,lat), cbind(a$fixLon[i+1],a$fixLat[i+1]), longlat=TRUE))
        rd <- as.data.frame(t(apply(fltData$nav$RSBN, 1, function(x){
            corrRSBN(a[i,"fixLat"], a[i,"fixLon"], a[i+1,"fixLat"], a[i+1,"fixLon"],
                as.numeric(x["lat"]), as.numeric(x["lon"]))
        })))
        beacons <- which((rd$Sm<0 & ld+rd$Sm>0 & abs(rd$Zm)<dlim) | d1<dlim | d2<dlim)
        cbind(leg=rep(i, length(beacons)), beacon=beacons)
    }))
  } else {
    b <- do.call(rbind, lapply(1:length(bcLeg), function(i)cbind(names(bcLeg)[i],bcLeg[[i]])))
    for(i in which(is.na(suppressWarnings(as.integer(b[,2]))))) {
        b[i,2] <- match(b[i,2], a$fix)
    }
    b <- as.data.frame(b, stringsAsFactors = FALSE)
    names(b) <- c("beacon","leg")
    b$leg <- as.integer(b$leg)
    b <- b[!is.na(b$leg) & b$leg<nrow(a),]
    b <- b[order(b$leg),]
    bnr <- lapply(substr(b[,"beacon"],1,2), function(x)with(fltData$nav$RSBN, which(id==x)))
    if (length(which(sapply(bnr, length)==0))) {
        stop(sprintf("RSBN beacon(s) '%s' not found in the database",
            paste(substr(b[,"beacon"],1,2)[which(sapply(bnr,length)==0)], collapse=", ")))
    }
    for (i in which(sapply(bnr, length)>1)) {
        chn <- as.integer(substr(b[i,"beacon"],3,4))
        if (!is.na(chn)) {
        if (is.na(match(chn, fltData$nav$RSBN[bnr[[i]], "channel"]))) {
            stop(sprintf("Can't find a RSBN beacon '%s' emitting on channel %s",
                substr(b[i,"beacon"],1,2), chn))
        }
        bnr[[i]] <- with(fltData$nav$RSBN, which(channel==chn & id==substr(b[i,"beacon"],1,2)))
        } else {
        d <- sapply(bnr[[i]], function(j){
            with(fltData$nav$RSBN, spDistsN1(cbind(lon[j],lat[j]), cbind(a$fixLon[i],a$fixLat[i]), longlat=TRUE))
        })
        bnr[[i]] <- bnr[[i]][which(d==min(d))[1]]
        }
    }
    b[,"beacon"] <- unlist(bnr)
  }
  if (nrow(b)==0) {
    return(data.frame("Leg"=NA, "From"=NA, "To"=NA, "beaconName"=NA, "bcID"=NA, "bcChannel"=NA,
        "Zm"=NA, "Sm"=NA, "YK"=NA)[c(),])
  }
  res <- cbind(b[,"leg"], a$fix[b[,"leg"]], a$fix[b[,"leg"]+1],
      fltData$nav$RSBN[b[,"beacon"], c("name","id","channel")],
      t(apply(b, 1, function(x){
      corrRSBN(
          a[x["leg"],"fixLat"], a[x["leg"],"fixLon"],
          a[x["leg"]+1,"fixLat"], a[x["leg"]+1,"fixLon"],
          fltData$nav$RSBN$lat[x["beacon"]], fltData$nav$RSBN$lon[x["beacon"]])
      })))
  names(res) <- c("Leg","From","To","beaconName","bcID","bcChannel","Zm","Sm","YK")
  rownames(res) <- NULL
  res$beaconName <- gsub(" VORTAC DME", "", res$beaconName)
  res
}



#' Calculate NVU orthodromic coordinates using VOR/DME
#'
#' This function calculates the current NVU orthodromic coordinates
#' (Z and S) based on radial and distance from a VOR/DME beacon.
#'
#' @param plan A flight plan, as a data.frame. Plans created with \code{\link{planNVU}(..., merge=TRUE)}
#' are accepted as input. At a minimum, the data frame.should have four columns:
#' waypoint names, latitudes, longitudes and ZPY (NVU course). The latter is needed to indirectly
#' identify the NVU legs, which may not correspond 1:1 to the legs of the flight plan (see
#' \code{points=} in \code{\link{planNVU}}).
#' @param leg The current leg of the flight plan, whose NVU parameters are to be corrected.
#' It is specified either as a number or as the initial fix.
#' @param VOR ID of the VOR to use.If multiple stations match, the correct one is
#' determined based on proximity to the initial fix of the leg.
#' @param radialTo Current \emph{inbound} radial (as indicated on the UShDB instrument of the Tu-154).
#' @param dist Distance in km, as measured by the DME
#' @param S Along track distance S, as currently indicated in the NVU computer. If supplied,
#' it must always be negative (as is the NVU convention).
#' @param cols An optional vector of column names, if these are named differently in \code{plan}
#' @return Returns the current orthodromic coordinates Z (cross track error) and S (along track
#' distance) as a named vector.These can be fed into the NVU computer to correct the course of the
#' aircraft.
#'
#' Because S is changing rapidly as the aircraft travels, for convenience if S is supplied as an
#' argument, an extra element dS is added to the return vector indicating the difference between
#' actual (corrected) S minus indicated S. Add dS to the current S indication of the NVU to correct it.
#'
#' @export
corrNVU_VOR <- function(plan, leg, VOR, radialTo, dist, S=NA, cols=c("fix","fixLat","fixLon", "ZPY")) {
  a <- plan[, cols]
  names(a) <- c("fix","fixLat","fixLon","ZPY")
  a <- subset(a, !is.na(ZPY)); rownames(a) <- NULL
  if (is.character(leg)) leg <- match(leg, a[,"fix"])
  leg <- leg[1]

  VORd <- findFixes(VOR, refPoint=list(lat = a[leg, "fixLat"], lon = a[leg, "fixLon"]))
  distR <- (dist/1.852) * (pi/(180*60))
  LatVOR <- VORd$lat*pi/180; LonVOR <- VORd$lon*pi/180

  radial <- ((radialTo + 180 + VORd$dev) %% 360) *pi/180

  posLat <- asin(sin(LatVOR)*cos(distR)+cos(LatVOR)*sin(distR)*cos(radial))
  dLon <- atan2(sin(radial)*sin(distR)*cos(LatVOR),cos(distR)-sin(LatVOR)*sin(posLat))
  posLon <- (LonVOR+dLon+pi %% (2*pi)) - pi

  posLat <- posLat*180/pi; posLon <- posLon*180/pi

  res <- orthoCoords(a$fixLat[leg], a$fixLon[leg], a$fixLat[leg+1], a$fixLon[leg+1], posLat, posLon)[1:2]

  if (is.na(S)) {
    return(res)
  } else {
    return(c(res, "dS" = as.numeric(res["S"])-S))
  }
}



#' Convert geographical coordinates to orthodromic coordinates.
#'
#' This function is used in NVU calculations and corrections.
#'
#' @param lat1 Latitude of departure point (in degrees)
#' @param lon1 Longitude of departure point (in degrees)
#' @param lat2 Latitude of destination point (in degrees)
#' @param lon2 Longitude of destination point (in degrees)
#' @param latP Latitude of point to be converted to orthodromic coordinates (in degrees)
#' @param lonP Longitude of point to be converted to orthodromic coordinates (in degrees)
#' @return Returns a named vector with orthodromic coordinates Z and S (in km),
#' as well as S0, the negative of the leg distance (which is equivalently
#' the S coordinate of the departure point).
#'
#' @export
orthoCoords <- function(lat1, lon1, lat2, lon2, latP, lonP) {
  crsAB <- gcb(lat1, lon1, lat2, lon2)
  crsAD <- gcb(lat1, lon1, latP, lonP)
  S0 <- -spDistsN1(cbind(lon1, lat1), cbind(lon2, lat2), longlat=TRUE)
  A <- crsAB-crsAD
  b <- (spDistsN1(cbind(lon1, lat1), cbind(lonP, latP), longlat=TRUE)/1.852) * (pi/(180*60))
  Z <- asin(sin(b)*sin(-A*pi/180))
  S <- S0 + (1.852*60*180/pi) * atan2(sin(b)*cos(A*pi/180),cos(b))
  Z <- (1.852*60*180/pi) * Z
  c("Z"=Z, "S"=S, "S0"=S0)
}

