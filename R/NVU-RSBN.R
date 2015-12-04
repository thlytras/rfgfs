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
#' \code{ZPY} (orthodromic HSI course heading) and \code{S} (course distance, negated) can be fed
#' to the NVU panels. \code{da} is the azimuth correction at each waypoint.
#'
#' If \code{merge = TRUE} the above columns are added to the data.frame \code{plan}.
#'
#' @export
planNVU <- function(plan, points=NULL, cols=c("fix","fixLat","fixLon"), init.mag=FALSE, merge=FALSE) {
  if (is.null(points)) points <- 1:nrow(plan)
  if (sum(points<0)) points <- (1:nrow(plan))[points]
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



