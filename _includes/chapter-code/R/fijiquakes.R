## fijiquakes.R
##     Fiji earthquakes data 'qk'
## +   function showzone() to display map area.
## 
## Copyright (C) 2015 Adrian Baddeley, Ege Rubak and Rolf Turner

require(datasets)
require(mapdata)

if(!exists("monochrome")) stop("File startup.R has not been sourced.")
qk <- ppp(quakes$long, quakes$lat, c(164, 190), c(-39,-10))

clearzone <- function() {
  map('world2Hires', xlim=c(164, 190), ylim=c(-39,-10), col=0)
}

showzone <- function(annotate=TRUE, solid=TRUE, mono=monochrome,
                     ..., col.land, col.text, do.axes=TRUE) {
  if(missing(col.land)) col.land <- if(mono) NULL else "green"
  if(missing(col.text)) col.text <- if(mono) NULL else "blue"
  map('world2Hires', xlim=c(164, 190), ylim=c(-39,-10), 
      col=col.land,  fill=solid, ...)
  if(do.axes) map.axes()
  if(annotate) annotatezone(col=col.text)
}

annotatezone <- function(...) {
    text(c(170.8, 169.3, 169.9, 177.5, 187.4),
         c(-37.7, -23.5, -14.4, -15.6, -12.7),
         c("New Zealand", "New Caledonia", "Vanuatu", "Fiji", "Samoa"),
         ...)
}





