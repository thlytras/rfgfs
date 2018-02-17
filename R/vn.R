# "Virtual Navigator" functions

#' "Virtual Navigator": Tune the Tu-154B NVU system
#'
#' Through the FlightGear props (telnet) interface, tune the NVU system
#' of the Tu-154B to the required navigational parameters (ZPY, S, Z).
#'
#' @param con A socket connection object created with \code{\link{fgfsConnect}}
#' @param ZPY ZPY angle, in degrees. This is rounded to the first decimal when
#' passing to FlightGear. If \code{NA}, the already tuned ZPY is not modified.
#' @param S Leg distance (S), in km. If \code{NA}, the already tuned S is not modified.
#' @param Z Cross track distance (Z), in km. If \code{NA}, the already tuned Z is not modified.
#' @param active If \code{TRUE}, tune the active device. If \code{FALSE}, tune the
#' inactive device (i.e. tune Sp and Zp).
#'
#' @export
vnNVUtune <- function(con, ZPY=NA, S=NA, Z=NA, active=TRUE) {
  if (active) {
    if (!is.na(ZPY))
      fgfsSet(con, "/fdm/jsbsim/instrumentation/nvu/ZPU-active", round(ZPY, 1))
    if (!is.na(S))
      fgfsSet(con, "/fdm/jsbsim/instrumentation/nvu/S-base-active", S*1000)
    if (!is.na(Z))
      fgfsSet(con, "/fdm/jsbsim/instrumentation/nvu/Z-base-active", Z*1000)
  } else {
    if (!is.na(ZPY))
      fgfsSet(con, "/fdm/jsbsim/instrumentation/nvu/ZPU-inactive", round(ZPY, 1))
    if (!is.na(S))
      fgfsSet(con, "/fdm/jsbsim/instrumentation/nvu/Spm-inactive", S*1000)
    if (!is.na(Z))
      fgfsSet(con, "/fdm/jsbsim/instrumentation/nvu/Zpm-inactive", Z*1000)
  }
}



#' "Virtual Navigator": Tune RSBN correction parameters on the Tu-154B
#'
#' Through the FlightGear props (telnet) interface, tune RSBN correction parameters
#' on the appropriate devices (B-52 panel, B-8M map angle instrument) of the Tu-154B.
#'
#' @param con A socket connection object created with \code{\link{fgfsConnect}}
#' @param Zm Cross-track distance (Zm), in km. If \code{NA}, the already tuned Zm is not modified.
#' @param Sm Along-track distance (Sm), in km. If \code{NA}, the already tuned Sm is not modified.
#' @param YK Map angle (YK), in degrees. This is rounded to the first decimal when
#' passing to FlightGear. If \code{NA}, the already tuned YK is not modified.
#'
#' @export
vnNVUtuneRSBN <- function(con, Zm=NA, Sm=NA, YK=NA) {
  if (!is.na(Zm))
    fgfsSet(con, "/fdm/jsbsim/instrumentation/nvu/Zpm-active", Zm*1000)
  if (!is.na(Sm))
    fgfsSet(con, "/fdm/jsbsim/instrumentation/nvu/Spm-active", Sm*1000)
  if (!is.na(YK)) {
    fgfsSet(con, "/tu154/instrumentation/b-8m/outer", floor(YK/10)*10)
    fgfsSet(con, "/tu154/instrumentation/b-8m/inner", round((YK %% 10) * 10))
  }
}



#' "Virtual Navigator": Tune the Tu-154B NVU to the current leg of a flight plan
#'
#' Through the FlightGear props (telnet) interface, load the NVU parameters for the
#' current leg of a flight plan to the active NVU device of the Tu-154B.
#'
#' @param con A socket connection object created with \code{\link{fgfsConnect}}
#' @param plan A flight plan, as a data.frame. Plans created with \code{\link{planRoute}}
#' are accepted as input. At a minimum, the data frame.should have three columns:
#' waypoint names, orthodromic course headings (ZPY) and leg distances (S).
#' @param leg The current leg of the flight plan. Specified as waypoint name.
#' If ZPY and S for that leg are \code{NA}, nothing happens.
#' @param cols An optional vector of column names, if these are named differently in \code{plan}
#'
#' @export
vnNVUloadLeg <- function(con, plan, leg, cols=c("fix","ZPY","S")) {
  x <- unlist(plan[plan[[cols[1]]]==leg, cols[2:3]])
  vnNVUtune(con, x[1], x[2], 0, active=TRUE)
}



#' "Virtual Navigator": Tune the Tu-154B NVU to the next leg of a flight plan
#'
#' Through the FlightGear props (telnet) interface, load the NVU parameters for the
#' next (i.e. upcoming) leg of a flight plan to the inactive NVU device of the Tu-154B.
#'
#' @param con A socket connection object created with \code{\link{fgfsConnect}}
#' @param plan A flight plan, as a data.frame. Plans created with \code{\link{planRoute}}
#' are accepted as input. At a minimum, the data frame.should have three columns:
#' waypoint names, orthodromic course headings (ZPY) and leg distances (S).
#' @param leg The next (i.e. upcoming) leg of the flight plan. Specified as waypoint name.
#' If ZPY and S for that leg are \code{NA}, nothing happens.
#' @param cols An optional vector of column names, if these are named differently in \code{plan}
#'
#' @export
vnNVUloadNextLeg <- function(con, plan, leg, cols=c("fix","ZPY","S")) {
  x <- unlist(plan[plan[[cols[1]]]==leg, cols[2:3]])
  vnNVUtune(con, x[1], x[2], 0, active=FALSE)
}



#' "Virtual Navigator": Load RSBN correction parameters on a Tu-154B
#'
#' Through the FlightGear props (telnet) interface, tune RSBN correction parameters
#' on the appropriate devices (B-52 panel, B-8M map angle instrument) of the Tu-154B,
#' taking them from a data.frame of leg-correction pairs (of the type created by
#' \code{\link{corrPlanRSBN}}.
#'
#' @param con A socket connection object created with \code{\link{fgfsConnect}}
#' @param legs A data.frame of leg-correction pairs such as those created by
#' \code{\link{corrPlanRSBN}}. At a minimum, the data frame.should have three columns:
#' cross-track distance (Zm), along-track distance (Sm) and map angle (YK).
#' @param row Which row of the data.frame to use?
#' @param cols An optional vector of column names, if these are named differently in \code{plan}
#'
#' @export
vnNVUloadRSBN <- function(con, legs, row=1, cols=c("Zm","Sm","YK")) {
  x <- unlist(legs[row, cols])
  vnNVUtuneRSBN(con, x[1], x[2], x[3])
}



#' "Virtual Navigator": Correct NVU orthodromic coordinates using VOR/DME
#'
#' Through the FlightGear props (telnet) interface and on the Tu-154B, obtain
#' radial and distance from a certain VOR/DME beacon, calculate the current
#' orthodromic coordinates S and Z, and (optionally) tune them into the active
#' NVU device.
#'
#' @param con A socket connection object created with \code{\link{fgfsConnect}}
#' @param plan A flight plan, as a data.frame. Plans created with \code{\link{planRoute}}
#' are accepted as input. At a minimum, the data frame.should have three columns:
#' waypoint names, orthodromic course headings (ZPY) and leg distances (S).
#' @param leg The next (i.e. upcoming) leg of the flight plan. Specified as waypoint name.
#' @param VOR The 3-letter identifier of the VOR beacon.
#' @param UShDB Which of the two UShDB needles shows the current VOR radial? Either 1 or 2.
#' @param DME Which DME radio to use? Either 1 or 2.
#' @param apply Tune the active NVU device to the calculated orthodromic coordinates? If \code{FALSE},
#' these are returned without adjusting the NVU device.
#' @param cols An optional vector of column names, if these are named differently in \code{plan}
#'
#' @export
vnCorrNVU_VOR <- function(con, plan, leg, VOR, UShDB=2, DME=UShDB, apply=TRUE, cols=c("fix","fixLat","fixLon", "ZPY")) {
  if (length(UShDB)!=1 || !(UShDB==1 || UShDB==2))
    stop("Argument UShDB should be 1 or 2.")
  if (length(DME)!=1 || !(DME==1 || DME==2))
    stop("Argument DME should be 1 or 2.")
  radialTo <- as.numeric(fgfsGet(con, paste("/tu154/instrumentation/ushdb/heading-deg-",UShDB, sep="")))
  dmeTxt <- if (DME==1) "" else "[1]"
  dist <- as.numeric(fgfsGet(con, paste("/tu154/instrumentation/dme", dmeTxt, "/distance", sep="")))/1000
  res <- corrNVU_VOR(plan, leg, VOR, radialTo, dist, cols=cols)
  if (apply) {
    vnNVUtune(con, Z=res[1], S=res[2])
  }
  return(res)
}



#' "Virtual Navigator": Autoload next flight plan leg to the inactive NVU device of the Tu-154B
#'
#' Through the FlightGear props (telnet) interface and on the Tu-154B, this function
#' monitors the NVU and with every waypoint change, it loads the next
#' flight plan leg to the inactive NVU device.
#'
#' @param con A socket connection object created with \code{\link{fgfsConnect}}
#' @param plan A flight plan, as a data.frame. Plans created with \code{\link{planRoute}}
#' are accepted as input. At a minimum, the data frame.should have three columns:
#' waypoint names, orthodromic course headings (ZPY) and leg distances (S).
#' @param leg The current leg of the flight plan. Specified as waypoint name.
#' @param init If \code{TRUE}, also tune the active NVU device to the start of the current leg.
#' You will only want to use this on the ground, before you takeoff.
#' @param poll Time (in seconds) between successive checks of whether a waypoint change happened.
#' In between, the function \code{\link{Sys.sleep}}s to conserve CPU cycles.
#' @param cols An optional vector of column names, if these are named differently in \code{plan}
#'
#' @details This function loads each leg of your journey to the NVU, as your navigator would do,
#' so you can focus on other things. It stops if there's no other leg to be loaded, or if you press
#' Ctrl-C. For example you can stop it to correct your position with \code{\link{vnCorrNVU_VOR}},
#' and then run it again from the current leg.
#'
#' @return None
#'
#' @export
vnNVUautoload <- function(con, plan, leg, init=FALSE, poll=10, cols=c("fix","ZPY","S")) {
  plan <- plan[,cols]
  names(plan) <- c("fix","ZPY","S")
  plan <- subset(plan, !is.na(ZPY)); rownames(plan) <- NULL
  if (!(leg %in% plan$fix)) stop(sprintf("Fix '%s' not found in flight plan.", leg))
  i <- match(leg, plan$fix)
  if (i==nrow(plan)-1) {
    cat(sprintf("Leg %s --> %s is the last leg of the flight plan. Quitting...", leg, plan$fix[i+1]))
    return()
  }
  activeNVU <- as.numeric(fgfsGet(con, "/fdm/jsbsim/instrumentation/nvu/active"))
  if (is.na(activeNVU))
    stop("The NVU device in the simulator is turned off. You have to turn it on first.")
  if (activeNVU!=1 && activeNVU!=2)
    stop("Invalid value in property /fdm/jsbsim/instrumentation/nvu/active")
  if (init) vnNVUloadLeg(con, plan, leg)
  cat(sprintf("Current leg is %s --> %s\n", plan$fix[i], plan$fix[i+1]))
  cat(sprintf("Loading next leg: %s --> %s. ", plan$fix[i+1], plan$fix[i+2]))
  vnNVUloadNextLeg(con, plan, plan$fix[i+1])
  if (i+1==nrow(plan)-1) {
    cat("This is the last leg of the flight plan. Quitting...\n\n")
  } else {
    cat("Standing by...\n")
    while (TRUE) {
      Sys.sleep(poll)
      a <- as.numeric(fgfsGet(con, "/fdm/jsbsim/instrumentation/nvu/active"))
      if (length(a)==0) {
        warning("Got no response over telnet connection. Game stuck? Will try again...")
        next
      }
      if (is.na(a)) {
        stop("The NVU device in the simulator has been turned off. Quitting...")
      }
      if (a!=1 && a!=2)
        stop("Invalid value in property /fdm/jsbsim/instrumentation/nvu/active")
      if (a != activeNVU) {
        activeNVU <- a
        i <- i + 1
        cat(sprintf("We have just passed %s. Loading next leg: %s --> %s.\n",
                    plan$fix[i], plan$fix[i+1], plan$fix[i+2]))
        vnNVUloadNextLeg(con, plan, plan$fix[i+1])
        if (i+1==nrow(plan)-1) {
          cat("This is the last leg of the flight plan. Quitting...\n\n")
          break
        }
      }
    }
  }
}
