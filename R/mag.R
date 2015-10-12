#' Calculate magnetic declination for a given coordinates
#'
#' Calculates the magnetic declination for a given latitude, longitude and height.
#'
#' @param lat Latitude (North is positive)
#' @param lon Longitude (East is positive)
#' @param h Height, in feet
#' @param date Date (as a Date object). Defaults to current date.
#'
#' @details This function uses the NIMA WMM2005 (World Magnetic Model). This is the exact same algorithm that FlightGear (via the SimGear library) uses, and in fact \code{magvar} is a port of the corresponding SimGear C++ function: \url{https://github.com/FlightGear/simgear/blob/next/simgear/magvar/coremag.cxx}
#'
#' @return Returns the magnetic declination (in degrees) for the specified latitude, longitude and height.
#' @export
magvar <- function(lat, lon, h=0, date=Sys.Date())
{
    mag$a = 6378.137       # semi-major axis (equatorial radius) of WGS84 ellipsoid
    mag$b = 6356.7523142   # semi-minor axis referenced to the WGS84 ellipsoid
    mag$r_0 = 6371.2   # standard Earth magnetic reference radius

    # convert degrees to radians
    lat <- lat * pi / 180
    lon <- lon * pi / 180
    # convert feet to km
    h <- h * 0.3048 / 1000

    sinlat <- sin(lat)
    coslat <- cos(lat)

    # convert to geocentric coords:
    sr <- sqrt((mag$a*coslat)^2 + (mag$b*sinlat)^2)   # sr is effective radius
    theta <- atan2(coslat * (h*sr + mag$a^2), sinlat * (h*sr + mag$b^2))   #theta is geocentric co-latitude

    r <- h^2 + 2*h*sr + (mag$a^4 - (mag$a^4 - mag$b^4) * sinlat^2) / (mag$a^2 - (mag$a^2 - mag$b^2) * sinlat^2)

    r <- sqrt(r)

    # r is geocentric radial distance
    c <- cos(theta);
    s <- sin(theta);
    # protect against zero divide at geographic poles
    inv_s <-  1 / (s + (s == 0)*1.0e-8)

    P <- matrix(0, nrow=mag$nmax, ncol=mag$nmax)
    DP <- matrix(0, nrow=mag$nmax, ncol=mag$nmax)

    # diagonal elements
    P[1, 1] = 1; P[2, 2] <- s; P[2, 1] = c
    DP[1, 1] <- 0; DP[2, 2] <- c; DP[2, 1] = -s

    for (n in 3:mag$nmax) {
        P[n, n] = P[n-1, n-1] * s * mag$root[n];
        DP[n, n] = (DP[n-1, n-1] * s + P[n-1, n-1] * c) * mag$root[n];
    }

    # lower triangle
    for (m in 0:(mag$nmax-1)) {
        for (n in max(m + 1, 2):(mag$nmax-1)) {
            if (n==mag$nmax) next
            P[n+1, m+1] = (P[n, m+1] * c * (2.0*n-1) -
                P[n-1, m+1] * mag$roots0[m+1, n+1]) * mag$roots1[m+1, n+1]

            DP[n+1, m+1] = ((DP[n, m+1] * c - P[n, m+1] * s) *
                (2.0*n-1) - DP[n-1, m+1] * mag$roots0[m+1, n+1]) * mag$roots1[m+1, n+1]
        }
    }
    P[13,13] <- 0; DP[13,13] <- 0

    # compute Gauss coefficients gnm and hnm of degree n and order m for the desired time
    #   achieved by adjusting the coefficients at time t0 for linear secular variation
    # WMM2005 
    yearfrac <- c(julian(date, origin=as.Date("2005-1-1"))) / 365.25 # reference date for current model is 1 january 2005
    gnm <- mag$gnm_wmm2005 + yearfrac*mag$gtnm_wmm2005
    hnm <- mag$hnm_wmm2005 + yearfrac*mag$htnm_wmm2005

    # compute sm (sin(m lon) and cm (cos(m lon))
    sm <- sin(0:(mag$nmax-1) * lon)
    cm <- cos(0:(mag$nmax-1) * lon)

    # compute B fields
    B_r <- 0
    B_theta <- 0
    B_phi <- 0
    fn_0 <- mag$r_0/r
    fn <- fn_0

    fn = fn_0 * fn_0
    for (n in 1:(mag$nmax-1)) {
        c1_n=0
        c2_n=0
        c3_n=0
        for (m in 0:n) {
            tmp = gnm[n+1, m+1] * cm[m+1] + hnm[n+1, m+1] * sm[m+1]
            c1_n = c1_n + tmp * P[n+1, m+1];
            c2_n = c2_n + tmp * DP[n+1, m+1];
            c3_n = c3_n + m * (gnm[n+1, m+1] * sm[m+1] - hnm[n+1, m+1] * cm[m+1]) * P[n+1, m+1];
        }
        fn = fn * fn_0;
        B_r = B_r + (n + 1) * c1_n * fn;
        B_theta = B_theta - c2_n * fn;
        B_phi = B_phi + c3_n * fn * inv_s;
    }

    # Find geodetic field components:
    psi <- theta - ((pi / 2) - lat);
    sinpsi <- sin(psi);
    cospsi <- cos(psi);
    X <- -B_theta * cospsi - B_r * sinpsi;
    Y <- B_phi;
    Z <- B_theta * sinpsi - B_r * cospsi;

    # find variation in radians (and convert to degrees)
    # return zero variation at magnetic pole X=Y=0.
    # E is positive
    if (X==0 && Y==0) return(0)
    return(atan2(Y, X)*180/pi)
}


#' Calculate magnetic declination at a given airport
#'
#' Calculates magnetic declination at a given airport 
#'
#' @param x A 4-letter ICAO airport code
#' @param date Date (as a Date object). Defaults to current date.
#'
#' @details This function calls \code{\link{magvar}}, which uses the NIMA WMM2005 (World Magnetic Model). This is the exact same algorithm that FlightGear (via the SimGear library) uses, and in fact \code{magvar} is a port of the corresponding SimGear C++ function: \url{https://github.com/FlightGear/simgear/blob/next/simgear/magvar/coremag.cxx}
#'
#' @return Returns the magnetic declination (in degrees) at the specified airport.
#' @export
magvar_apt <- function(x, date=Sys.Date()) {
    if (!is.apt(x)) stop("Airport was not found in the database.")
    apt <- fltData$apt[x,]
    magvar(apt$lat, apt$lon, apt$elevation, date=date)
}

