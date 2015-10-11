#' Write a flight plan to a kml file
#'
#' This function writes a flight plan to a kml file, suitable for opening and viewing with Google Earth (or other kml viewer).
#'
#' @param A flight plan data.frame, as returned from \code{\link{planRoute}}, or a list of such flight plans.
#' @return NULL
#' @export
makeKml <- function(fltplans, file, overwrite=TRUE) {
    if (!is.list(fltplans)) stop("Argument fltplans must be a data.frame or a list of data.frames")
    if (is.data.frame(fltplans)) fltplans <- list(fltplans)
    tryCatch({
    tmpfile <- tempfile()
    draw_waypoint <- function(element, style="waypoint", tab=0) {
      cat(rep(tb,0 + tab), '<Placemark>\n', sep="")
      cat(rep(tb,1 + tab), '<name>', element["fix"], '</name>\n', sep="")
      cat(rep(tb,1 + tab), '<styleUrl>#', style, '</styleUrl>\n', sep="")
      cat(rep(tb,1 + tab), '<Point>\n', sep="")
      cat(rep(tb,2 + tab), '<coordinates>\n', sep="")
      cat(rep(tb,3 + tab), '',element["fixLon"],',',element["fixLat"],'\n', sep="")
      cat(rep(tb,2 + tab), '</coordinates>\n', sep="")  
      cat(rep(tb,1 + tab), '</Point>\n', sep="")
      cat(rep(tb,0 + tab), '</Placemark>\n', sep="")  
    }
    tb <- "  "
    # Writing the header for the kml file
    sink(tmpfile)
    cat('<?xml version="1.0" encoding="utf-8"?>\n')
    cat('<kml xmlns="http://earth.google.com/kml/2.2">\n')
    cat(tb, '<Document>\n', sep="")
    cat(rep(tb,2), '<Style id="triangle1">\n', sep="")
    cat(rep(tb,3), '<IconStyle>\n', sep="")
    cat(rep(tb,4), '<scale>1</scale>\n', sep="")
    cat(rep(tb,4), '<Icon>\n', sep="")
    cat(rep(tb,5), '<href>http://maps.google.com/mapfiles/kml/shapes/triangle.png</href>\n', sep="")
    cat(rep(tb,4), '</Icon>\n', sep="")
    cat(rep(tb,3), '</IconStyle>\n', sep="")
    cat(rep(tb,2), '</Style>\n', sep="")  
    cat(rep(tb,2), '<Style id="triangle2">\n', sep="")
    cat(rep(tb,3), '<IconStyle>\n', sep="")
    cat(rep(tb,4), '<scale>1.25</scale>\n', sep="")
    cat(rep(tb,4), '<Icon>\n', sep="")
    cat(rep(tb,5), '<href>http://maps.google.com/mapfiles/kml/shapes/triangle.png</href>\n', sep="")
    cat(rep(tb,4), '</Icon>\n', sep="")
    cat(rep(tb,3), '</IconStyle>\n', sep="")
    cat(rep(tb,2), '</Style>\n', sep="")  

    cat(rep(tb,2), '<Style id="airport1">\n', sep="")
    cat(rep(tb,3), '<IconStyle>\n', sep="")
    cat(rep(tb,4), '<scale>1</scale>\n', sep="")
    cat(rep(tb,4), '<Icon>\n', sep="")
    cat(rep(tb,5), '<href>http://maps.google.com/mapfiles/kml/shapes/airports.png</href>\n', sep="")
    cat(rep(tb,4), '</Icon>\n', sep="")
    cat(rep(tb,3), '</IconStyle>\n', sep="")
    cat(rep(tb,2), '</Style>\n', sep="")  
    cat(rep(tb,2), '<Style id="airport2">\n', sep="")
    cat(rep(tb,3), '<IconStyle>\n', sep="")
    cat(rep(tb,4), '<scale>1.25</scale>\n', sep="")
    cat(rep(tb,4), '<Icon>\n', sep="")
    cat(rep(tb,5), '<href>http://maps.google.com/mapfiles/kml/shapes/airports.png</href>\n', sep="")
    cat(rep(tb,4), '</Icon>\n', sep="")
    cat(rep(tb,3), '</IconStyle>\n', sep="")
    cat(rep(tb,2), '</Style>\n', sep="")  

    cat(rep(tb,2), '<StyleMap id="waypoint">\n', sep="")
    cat(rep(tb,3), '<Pair>\n', sep="")
    cat(rep(tb,4), '<key>normal</key>\n', sep="")
    cat(rep(tb,4), '<styleUrl>#triangle1</styleUrl>\n', sep="")
    cat(rep(tb,3), '</Pair>\n', sep="")
    cat(rep(tb,3), '<Pair>\n', sep="")
    cat(rep(tb,4), '<key>highlight</key>\n', sep="")
    cat(rep(tb,4), '<styleUrl>#triangle2</styleUrl>\n', sep="")
    cat(rep(tb,3), '</Pair>\n', sep="")
    cat(rep(tb,2), '</StyleMap>\n', sep="")
    cat(rep(tb,2), '<Style id="route">\n', sep="")
    cat(rep(tb,3), '<LineStyle>\n', sep="")
    cat(rep(tb,4), '<color>red</color>\n', sep="")
    cat(rep(tb,4), '<width>5</width>\n', sep="")
    cat(rep(tb,3), '</LineStyle>\n', sep="")
    cat(rep(tb,2), '</Style>\n', sep="")  

    cat(rep(tb,2), '<StyleMap id="airport">\n', sep="")
    cat(rep(tb,3), '<Pair>\n', sep="")
    cat(rep(tb,4), '<key>normal</key>\n', sep="")
    cat(rep(tb,4), '<styleUrl>#airport1</styleUrl>\n', sep="")
    cat(rep(tb,3), '</Pair>\n', sep="")
    cat(rep(tb,3), '<Pair>\n', sep="")
    cat(rep(tb,4), '<key>highlight</key>\n', sep="")
    cat(rep(tb,4), '<styleUrl>#airport2</styleUrl>\n', sep="")
    cat(rep(tb,3), '</Pair>\n', sep="")
    cat(rep(tb,2), '</StyleMap>\n', sep="")
    
    # Iterating over all flight plans
    for (i in 1:length(fltplans)) {
        cat(rep(tb,2), '<Document>\n', sep="")
        if (is.null(names(fltplans)[i])) {
            cat(rep(tb,3), '<name>Flight plan</name>', sep="")
        } else {
            cat(rep(tb,3), '<name>', names(fltplans)[i], '</name>', sep="")
        }
        apply(fltplans[[i]][-c(1,nrow(fltplans[[i]])),c("fix","fixLon","fixLat")], 
                1, draw_waypoint, style="waypoint", tab=3)
        apply(fltplans[[i]][c(1,nrow(fltplans[[i]])),c("fix","fixLon","fixLat")], 
                1, draw_waypoint, style="airport", tab=3)
        cat(rep(tb,3), '<Placemark>\n', sep="")
        cat(rep(tb,4), '<name>Flight plan route</name>\n', sep="")
        cat(rep(tb,4), '<styleUrl>#route</styleUrl>\n', sep="")
        cat(rep(tb,4), '<LineString>\n', sep="")
        cat(rep(tb,5), '<coordinates>\n', sep="")
        cat(rep(tb,4))
        cat(with(fltplans[[i]], paste(fixLon, fixLat, sep=",", collapse=" ")))
        cat('\n')
        cat(rep(tb,5), '</coordinates>\n', sep="")
        cat(rep(tb,4), '</LineString>\n', sep="")
        cat(rep(tb,3), '</Placemark>\n', sep="")
        cat(rep(tb,2), '</Document>\n', sep="")
    }
    
    cat(tb, '</Document>\n', sep="")
    cat('</kml>\n')
    sink()
    file.copy(tmpfile, file, overwrite=overwrite)
    return(invisible())
  }, error=function(e){suppressWarnings(sink()); stop(e)})
}
