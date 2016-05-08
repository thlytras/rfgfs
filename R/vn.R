# "Virtual Navigator" functions

#' "Virtual Navigator": Tune the Tu-154 NVU system
#'
#' Through the FlightGear props (telnet) interface, tune the NVU system
#' of the Tu-154 to the required navigational parameters (ZPY, S, Z).
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



#' "Virtual Navigator": Tune RSBN correction parameters on the Tu-154
#'
#' Through the FlightGear props (telnet) interface, tune RSBN correction parameters
#' on the appropriate devices (B-52 panel, B-8M map angle instrument) of the Tu-154.
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
    fgfsSet(con, "/tu154/instrumentation/b-8m/outer", YK %/% 10)
    fgfsSet(con, "/tu154/instrumentation/b-8m/inner", round((YK %% 10) * 10))
  }
}



#' "Virtual Navigator": Tune the Tu-154 NVU to the current leg of a flight plan
#'
#' Through the FlightGear props (telnet) interface, load the NVU parameters for the
#' current leg of a flight plan to the active NVU device of the Tu-154.
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



#' "Virtual Navigator": Tune the Tu-154 NVU to the next leg of a flight plan
#'
#' Through the FlightGear props (telnet) interface, load the NVU parameters for the
#' next (i.e. upcoming) leg of a flight plan to the inactive NVU device of the Tu-154.
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



#' "Virtual Navigator": Load RSBN correction parameters on a Tu-154
#'
#' Through the FlightGear props (telnet) interface, tune RSBN correction parameters
#' on the appropriate devices (B-52 panel, B-8M map angle instrument) of the Tu-154,
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

