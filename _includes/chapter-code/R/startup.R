## startup.R
## Standard incantations for the start of every .Rnw file 
##
## Copyright (C) 2015 Adrian Baddeley, Ege Rubak and Rolf Turner

require(spatstat)
require(spatstat.utils)
require(english)

## Adding all 'require' commands here
## so that it is immediately clear if any packages need to be installed.

require(maptools)
require(tiff)
require(jpeg)
require(dixon)

## Specify draft version (fast computations, low resolution) or final version
draftversion <- FALSE

## Specify printed book or e-book
ebook <- FALSE
## This flag can be used in the Rnw source to execute
## different commands for ebook and printed book.
## You can even change the text, using \Sexpr{if(ebook) ...} 

## Grey scale flag
## Set monochrome=TRUE for the printed book, FALSE for e-book
monochrome <- !ebook

monowarning <- if(!monochrome) "" else
               "Colours are rendered as greyscales in this book."

## The main effect of 'monochrome=TRUE' is to force graphics to be greyscale.
## Additionally this flag can be used in the Rnw source to execute
## different commands depending on whether colour or monochrome is required.
## You can even change the text, using \Sexpr{if(monochrome) ...}

if(monochrome) {
  ## Set the graphics colours to greyscale
  ## Standard pen colours
  palette(to.grey(palette()))
  ## standard image colour map
  spatstat.options(image.colfun=function(n) grey(seq(0,1,length=n)))
  ## spatstat internal code 
  spatstat.options(monochrome=TRUE)
  spatstat.options(par.pp3=list(box.front=list(col=1,lwd=2),
                                box.back=list(col="grey")))
  ## The following 'sledgehammer' commands will force all graphics
  ## to be greyscale, but they should not be required..
  ## ps.options does not recognise the English spelling of 'grey'
# ps.options(colormodel="gray")
  ## pdf.options DOES recognise the English spelling of 'grey'
# pdf.options(colormodel="grey")
} ## otherwise leave at default which is sRGB

## Suppress new 'semi-transparent' colours in plot.ppp
spatstat.options(transparent=FALSE)

## Simulation algorithms
## The following options ensure that simulations are reproducible
## using the chosen random seed values. They select the older, slower
## simulation algorithms that were in force when the book was written.
spatstat.options(fastthin=FALSE)
spatstat.options(fastpois=FALSE)

## default parameters for plot.fv
parfv <- list()
parfv$legendargs <- list(bty="n") # no box around legend
if(monochrome) parfv$col <- 1  # black lines
spatstat.options(par.fv=parfv)

## define commonly-used greyscale maps
blacktowhite <- function(n) { grey(seq(0, 1, length=n)) }
whitetoblack <- function(n) { grey(seq(1, 0, length=n)) }
greytoblack <- function(n) { grey(seq(0.9, 0, length=n)) }
blacktogrey <- function(n) { grey(seq(0.0, 9, length=n)) }
lighttodark <- function(n) { grey(seq(0.9, 0.2, length=n)) }
darktolight <- function(n) { grey(seq(0.2, 0.9, length=n)) }

## Set width of R output
##  (this limit is often but not always respected)
## If using krantz1 (text width 6+1/8 inch), set width to 60 characters
## If using krantz2 (text width 7 inch), set width to 72 characters
## We're using krantz2, with R output in 'small' font, allowing 88 characters
options(width=82)

## Set continuation character to blank
options(continue="  ")

## Suppress weird characters
options(useFancyQuotes=FALSE)

## Figure layout functions and parameters
source("R/figurelayout.R")
## set default graphics parameters in Sweave plots
resetpar()

## use Times font family in all graphics text 
ps.options(family="Times")
pdf.options(family="Times")

## Ensure the directory exists for saving automatically-generated graphics
if(!file.exists("pix-auto")) dir.create("pix-auto")
## Ditto for saved data 
if(!file.exists("data-auto")) dir.create("data-auto")

datafilepath <- function(fname) { paste0("data-auto/", fname) }



## .................. Code for generating text ......................

## enforce American convention for lists: "A, B, and C"
UScommasep <- function(x, join=", and ", flatten=TRUE) {
  commasep(x, join=join, flatten=flatten)
}

## Use inside \Sexpr to enclose a string in \texttt{  }

inFont <- function(f="texttt", x) {
    if(is.null(x)) return("")
    paste0("\\\\", f, paren(x, "{"))
}

## convert e.g. 'SpatialLinesDataFrame' into Spatial\-Lines\-Data\-Frame
## to avoid overrunning margins.

HyphenateMouthful <- function(x) {
  ## split into characters
  y <- strsplit(x, NULL)[[1]]
  ## identify l.c. followed by u.c.
  ends <- which(y[-length(y)] %in% letters & y[-1] %in% LETTERS)
  ends <- c(ends, nchar(x))
  nbits <- length(ends)
  starts <- c(0, ends[-nbits]) + 1
  ## 
  z <- substr(x, starts[1], ends[1])
  if(nbits > 1) {
    for(i in 2:nbits) 
      z <- paste0(z, "\\\\-", substr(x, starts[i], ends[i]))
  }
  return(z)
}

requireversion <- function(pkg, ver) {
  eval(substitute(require(p), list(p=substitute(pkg))))
  pkgname <- deparse(substitute(pkg))
  v <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                fields="Version")
  if(package_version(v) < ver)
    stop(paste("Package",
               sQuote(pkgname),
               "is out of date: version >=",
               ver,
               "is needed"))
  invisible(NULL)
}

  
requireversion(spatstat, "1.42-2.027")
