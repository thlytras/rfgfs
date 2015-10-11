### rfgfs - An R package

**A flight planner and companion R package for FlightGear flight simulator (fgfs)**

The rfgfs R package allows planning a route between two airports, using the airport, airway,
navaid and navfix data that ship with FlightGear. These data have been parsed into .RData,
are included with the package internally, and can be retrieved with the function flightData().

The main workhorse of the package is the function planRoute(); this returns a full flight plan
as a data.frame, and this can be further written to a kml file (that can be opened with 
Google Earth or other kml viewer) using function makeKml().

The package also includes a few additional functions, including magvar() to calculate magnetic
declination for any set of coordinates. Thus the flight plans that planRoute() creates include 
both true and magnetic course for every leg.

**Installation**

Open R, and give:

      devtools::install_github("rfgfs")

If you do not have package "devtools", install it from CRAN with:

      install.packages("devtools")

**Why an R package? Why not an *[insert favorite platform/language]* application?**

Well, R is what I am most familiar and spend most of my time with. I wanted an easy way to create
flight plans to use with my favorite aircraft in flightgear (Tu-154B), so R was the natural choice 
for me. Hopefully it will be useful to other people as well
