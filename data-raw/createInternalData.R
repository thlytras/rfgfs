# This script creates the sysdata.rda file, that contains the internal flight data 
# (parsed into  R from the FlightGear datafiles) and some data required to calculate 
# magnetic declinations.

# Unzip files into temporary directory (adjust accordingly)
cat("Unzipping files...\n")
system(sprintf("gunzip -c /usr/share/games/flightgear/Airports/apt.dat.gz > %s/apt.dat", tempdir()))
system(sprintf("gunzip -c /usr/share/games/flightgear/Navaids/awy.dat.gz > %s/awy.dat", tempdir()))
system(sprintf("gunzip -c /usr/share/games/flightgear/Navaids/fix.dat.gz > %s/fix.dat", tempdir()))
system(sprintf("gunzip -c /usr/share/games/flightgear/Navaids/nav.dat.gz > %s/nav.dat", tempdir()))


library(sp)

# Great circle bearing
gcb <- function(lat1, lon1, lat2, lon2) atan2(sin((lon2-lon1)*pi/180)*cos(lat2*pi/180), 
    cos(lat1*pi/180)*sin(lat2*pi/180)-sin(lat1*pi/180)*cos(lat2*pi/180)*cos((lon2-lon1)*pi/180)) %% (2*pi)

# Difference of two angles (in radians)
angDiff <- function(a, b) {
  d <- a-b
  d[d>pi] <- d[d>pi]-(2*pi)
  d
}

fltPrepData <- function(){
  # Airway data
    cat("Loading airways data...\n")
    awy <- read.fwf(sprintf("%s/awy.dat", tempdir()), diff(c(1, 7, 18, 30, 36, 47, 59, 61, 65, 69, 140)), 
		    stringsAsFactors=FALSE, strip.white=TRUE, skip=3)
    names(awy) <- c("fix1", "fix1Lat", "fix1Lon", "fix2", "fix2Lat", "fix2Lon", 
		    "isHigh", "baseFL", "topFL", "awy")
    awy <- awy[-nrow(awy),]
    awy$id1 <- with(awy, paste(fix1, round(fix1Lat-fix1Lon,1), sep="|"))
    awy$id2 <- with(awy, paste(fix2, round(fix2Lat-fix2Lon,1), sep="|"))
    awy$dist <- apply(awy, 1, function(x) spDistsN1(
	      matrix(as.numeric(x[3:2]),ncol=2), 
	      matrix(as.numeric(x[6:5]),ncol=2), 
	    longlat=TRUE))
    awy$course <- apply(awy[,c("fix1Lat", "fix1Lon", "fix2Lat", "fix2Lon")], 1, function(x)gcb(x["fix1Lat"], x["fix1Lon"], x["fix2Lat"], x["fix2Lon"]))
  # Navigational fix data
    cat("Loading navigational fixes data...\n")
    fix <- read.fwf(sprintf("%s/fix.dat", tempdir()), diff(c(1, 12, 24, 29)), 
		    stringsAsFactors=FALSE, strip.white=TRUE, skip=3)
    names(fix) <- c("lat", "lon", "fix")
    fix <- fix[-nrow(fix),]
  # Navaid data
    cat("Loading navaids data...\n")
    navTypes.raw <- read.fwf(sprintf("%s/nav.dat", tempdir()), 2, skip=3)[,1]
    navTypes <- sapply(unique(navTypes.raw)[-length(unique(navTypes.raw))],
                    function(x)range(which(navTypes.raw %in% x)))
    colnames(navTypes) <- c("NDB", "VORDME", "ILS", "LOC", "GS", "OM", "MM", "IM", "DMEILS", "TACAN")
    navTypes.fields <- list(
	    c(1,3,16,30,37,43,47,54,59,110),
	    c(1,3,16,30,37,43,47,54,59,110),
	    c(1,3,16,30,37,43,47,59,64,69,73,110),
	    c(1,3,16,30,37,43,47,59,64,69,73,110),
	    c(1,3,16,30,37,43,47,59,64,69,73,110),
	    c(1,3,16,30,37,43,47,59,64,69,73,110),
	    c(1,3,16,30,37,43,47,59,64,69,73,110),
	    c(1,3,16,30,37,43,47,59,64,69,73,110),
	    c(1,3,16,30,37,43,47,59,64,69,73,110)+1,
	    c(1,3,16,30,37,43,47,54,59,110)+1)
    nav <- mapply(function(skip, nrows, navTypes.fields){
	    read.fwf(sprintf("%s/nav.dat", tempdir()), diff(navTypes.fields), stringsAsFactors=FALSE, strip.white=TRUE,
		    skip=skip, nrows=nrows)[,-1]
    }, skip=3+navTypes[1,]-1, nrows=navTypes[2,]-navTypes[1,]+1, navTypes.fields=navTypes.fields, SIMPLIFY=FALSE)
    names(nav) <- colnames(navTypes)
    colnames(nav$NDB) <- colnames(nav$VORDME) <- colnames(nav$TACAN) <- 
	    c("lat", "lon", "elev", "freq", "range", "dev", "id", "name")
    colnames(nav$ILS) <- colnames(nav$LOC) <- colnames(nav$GS) <- colnames(nav$DMEILS) <- 
	    colnames(nav$OM) <- colnames(nav$MM) <- colnames(nav$IM) <- 
	    c("lat", "lon", "elev", "freq", "range", "heading", "id", "apt", "awy", "type")
    # include RSBNs
    nav$RSBN <- read.fwf("rsbn.dat", diff(navTypes.fields[[1]]), stringsAsFactors=FALSE, strip.white=TRUE)[,-1]
    nav$RSBN <- nav$RSBN[seq(2,nrow(nav$RSBN),2),-6]
    colnames(nav$RSBN) <- c("lat", "lon", "elev", "freq", "range", "id", "name")
    nav$RSBN$channel <- as.integer(gsub("(.*\\sCh)|(VORTAC.*)", "", nav$RSBN$name))   
    # remove duplicate VORs
    nav$VOR <- nav$VOR[-which(duplicated(nav$VOR[,-6])),]; rownames(nav$VOR) <- NULL
    nav$VORDME <- nav$VORDME[-which(duplicated(nav$VORDME[,-6])),]; rownames(nav$VORDME) <- NULL
    nav$NDB <- nav$NDB[-which(duplicated(nav$NDB[,-6])),]; rownames(nav$NDB) <- NULL
  # All nav points data (taken from airways data)
    cat("Processing all nav points...\n")
    allPts <- rbind(
	setNames(awy[,c("fix1","fix1Lat","fix1Lon")], c("fix","fixLat","fixLon")), 
	setNames(awy[,c("fix2","fix2Lat","fix2Lon")], c("fix","fixLat","fixLon")))
    allPts <- allPts[!duplicated(allPts),]
    allPts$id <- with(allPts, paste(fix, round(fixLat-fixLon,1), sep="|"))
    allPts <- allPts[!duplicated(allPts$id),]
  # Angles and distances between all points in the airways data
    courseRev <- apply(awy[,c("fix1Lat", "fix1Lon", "fix2Lat", "fix2Lon")], 1, function(x)gcb(x["fix2Lat"], x["fix2Lon"], x["fix1Lat"], x["fix1Lon"]))
    dists <- rbind(awy[,c("awy", "id1", "id2", "dist", "baseFL", "topFL", "course")],
	cbind(setNames(awy[,c("awy", "id2", "id1", "dist", "baseFL", "topFL")], c("awy", "id1", "id2", "dist", "baseFL", "topFL")), course=courseRev))
    dists <- dists[!duplicated(dists),]
  # Load airport data
    cat("Loading airports data...\n")
    str_trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    aptList <- list()
    aptCon <- file(sprintf("%s/apt.dat", tempdir()))
    open(aptCon)
    suppressWarnings(tryCatch({
      while(length(curLine <- readLines(aptCon, n=1)) > 0) {
	if (length(grep("^1\\s.*", curLine))>0) { # Airport found
	  apt <- list(icaoCode = str_trim(substr(curLine, 16, 19)),
		      fullName = substr(curLine, 21, nchar(curLine)),
		      elevation = as.integer(substr(curLine, 3, 10)))
	  aptRwyLat <- c(); aptRwyLon <- c()
	  while(length(curLine <- readLines(aptCon, n=1)) > 0) {
	    if (curLine=="") break
	    if (length(grep("^100\\s.*", curLine))>0) { # Runway found
	      aptRwyLat <- c(aptRwyLat, as.numeric(substr(curLine, 36, 47)), as.numeric(substr(curLine, 92, 103)))
	      aptRwyLon <- c(aptRwyLon, as.numeric(substr(curLine, 49, 61)), as.numeric(substr(curLine, 105, 117)))
	    }
	  }
	  apt$lat <- mean(aptRwyLat); apt$lon <- mean(aptRwyLon)
	  aptList[[apt$icaoCode]] <- apt
	}
      }
    }, error=function(e){close(aptCon); stop(e)}))
    close(aptCon)
    apt <- as.data.frame(do.call("rbind", aptList))
    for (i in 1:ncol(apt)) apt[,i] <- unlist(apt[,i])
  # Return everything
    cat("All done!\n")
    list(awy=awy, fix=fix, nav=nav, pts=allPts, dists=dists, apt=apt)
}

fltData <- fltPrepData()

# Clean up
system(sprintf("rm -rf %s/*.dat", tempdir()))

mag <- list()
 
mag$gnm_wmm2005 <- matrix(c(
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    -29556.8, -1671.7, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    -2340.6, 3046.9, 1657.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    1335.4, -2305.1, 1246.7, 674.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    919.8, 798.1, 211.3, -379.4, 100.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    -227.4, 354.6, 208.7, -136.5, -168.3, -14.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    73.2, 69.7, 76.7, -151.2, -14.9, 14.6, -86.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    80.1, -74.5, -1.4, 38.5, 12.4, 9.5, 5.7, 1.8, 0.0, 0.0, 0.0, 0.0, 0.0,
    24.9, 7.7, -11.6, -6.9, -18.2, 10.0, 9.2, -11.6, -5.2, 0.0, 0.0, 0.0, 0.0,
    5.6, 9.9, 3.5, -7.0, 5.1, -10.8, -1.3, 8.8, -6.7, -9.1, 0.0, 0.0, 0.0,
    -2.3, -6.3, 1.6, -2.6, 0.0, 3.1, 0.4, 2.1, 3.9, -0.1, -2.3, 0.0, 0.0,
    2.8, -1.6, -1.7, 1.7, -0.1, 0.1, -0.7, 0.7, 1.8, 0.0, 1.1, 4.1, 0.0,
    -2.4, -0.4, 0.2, 0.8, -0.3, 1.1, -0.5, 0.4, -0.3, -0.3, -0.1, -0.3, -0.1), nrow=13, byrow=TRUE)

mag$hnm_wmm2005 <- matrix(c(
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 5079.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, -2594.7, -516.7, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, -199.9, 269.3, -524.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 281.5, -226.0, 145.8, -304.7, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 42.4, 179.8, -123.0, -19.5, 103.6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, -20.3, 54.7, 63.6, -63.4, -0.1, 50.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, -61.5, -22.4, 7.2, 25.4, 11.0, -26.4, -5.1, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 11.2, -21.0, 9.6, -19.8, 16.1, 7.7, -12.9, -0.2, 0.0, 0.0, 0.0, 0.0,
    0.0, -20.1, 12.9, 12.6, -6.7, -8.1, 8.0, 2.9, -7.9, 6.0, 0.0, 0.0, 0.0,
    0.0, 2.4, 0.2, 4.4, 4.8, -6.5, -1.1, -3.4, -0.8, -2.3, -7.9, 0.0, 0.0,
    0.0, 0.3, 1.2, -0.8, -2.5, 0.9, -0.6, -2.7, -0.9, -1.3, -2.0, -1.2, 0.0,
    0.0, -0.4, 0.3, 2.4, -2.6, 0.6, 0.3, 0.0, 0.0, 0.3, -0.9, -0.4, 0.8), nrow=13, byrow=TRUE)

mag$gtnm_wmm2005 <- matrix(c(
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    8.0, 10.6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    -15.1, -7.8, -0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.4, -2.6, -1.2, -6.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    -2.5, 2.8, -7.0, 6.2, -3.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    -2.8, 0.7, -3.2, -1.1, 0.1, -0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    -0.7, 0.4, -0.3, 2.3, -2.1, -0.6, 1.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.2, -0.1, -0.3, 1.1, 0.6, 0.5, -0.4, 0.6, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.1, 0.3, -0.4, 0.3, -0.3, 0.2, 0.4, -0.7, 0.4, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), nrow=13, byrow=TRUE)

mag$htnm_wmm2005 <- matrix(c(
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, -20.9, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, -23.2, -14.6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 5.0, -7.0, -0.6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 2.2, 1.6, 5.8, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 1.7, 2.1, 4.8, -1.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, -0.6, -1.9, -0.4, -0.5, -0.3, 0.7, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.6, 0.4, 0.2, 0.3, -0.8, -0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, -0.2, 0.1, 0.3, 0.4, 0.1, -0.2, 0.4, 0.4, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0), nrow=13, byrow=TRUE)

mag$nmax <- 13

mag$root <- c(NA, NA, sqrt((2 * (2:(mag$nmax-1)) - 1) / (2 * (2:(mag$nmax-1)))))

prependToLengh <- function(x, n) {
    x <- rev(x); length(x) <- n; rev(x)
}

mag$roots0 <- t(sapply(1:mag$nmax, function(m) {
    mm <- (m-1)^2
    n <- max(m, 3):mag$nmax
    suppressWarnings( prependToLengh(sqrt((n-2)^2 - mm), mag$nmax) )
}))
mag$roots0[is.nan(mag$roots0)] <- NA

mag$roots1 <- t(sapply(1:mag$nmax, function(m) {
    mm <- (m-1)^2
    n <- max(m, 3):mag$nmax
    prependToLengh(1 / sqrt((n-1)^2 - mm), mag$nmax)
}))
mag$roots1[mag$roots1==Inf] <- NA


cat("Creating sysdata.rda...\n")
devtools::use_data(fltData, mag, internal=TRUE, overwrite=TRUE)

