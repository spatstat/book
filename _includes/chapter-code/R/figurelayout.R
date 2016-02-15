## figurelayout.R
##
## Various R functions for controlling figure layout in Sweave and spatstat
##
## Copyright (C) 2015 Adrian Baddeley, Ege Rubak and Rolf Turner

if(!exists("monochrome")) monochrome <- TRUE

## Figure parameters are maintained in a list
MyFigurePars <- list()

## The list is interrogated at the time a new figure is initialised
options(SweaveHooks=list(fig=function() do.call(par, MyFigurePars)))

## ............................................................
## The following functions should be run BEFORE the figure is drawn
## .............................................................

## How to set a figure parameter 
setpar <- function(...) {
  newpar <- list(...)
  if(length(newpar) > 0) {
    if(any(!nzchar(names(newpar))))
      stop("All arguments must be named")
    myfigpars <- MyFigurePars
    for(nam in names(newpar))
      myfigpars[[nam]] <- newpar[[nam]]
    MyFigurePars <<- myfigpars
  }
  return(invisible(NULL))
}

## Defaults
resetpar <- function() {
  setpar(font.lab=3,       ##  italic font for axis labels and text
         mgp=c(2.25,1,0),  ##  move axis label position closer to axis
         cex.lab=1,        ## axis labels - normal size
         cex.axis=0.9,     ## axis numerals - slightly smaller
         cex.main=1.1,     ## main title - slightly smaller than usual 1.2
         xaxs='r',         ## usual 4% spacing between axis and plot
         yaxs='r')
  setmargins(1.1)
}

## Start new plot with default parameters
newplot <- function(...) {
  resetpar()
  widths(...)
}

CurrentLogicalWidth <- NA
LogicalFontSize <- NA
PhysicalFraction <- NA

## Declare figure dimensions so that text can be scaled appropriately
widths <- function(logicalinches, physicalfraction, 
                   targetsize=9.5, targetlwd=1, TextwidthCm=12.2,
                   fudge=0.75) {
  ## logicalinches: width in inches, as declared in \SweaveOpts
  CurrentLogicalWidth <<- logicalinches
  ## physicalfraction: fraction of physical \textwidth, in \setkeys{Gin}{}
  PhysicalFraction <<- physicalfraction
  ## determine physical scale
  shrinkage <- physicalfraction * TextwidthCm/((logicalinches-fudge) * 2.54)
  ## determine logical size of font needed to achieve physical 'targetsize'
  fontsize <- floor(targetsize/shrinkage)
  LogicalFontSize <<- fontsize
  ## set logical font size
  ps.options(pointsize=fontsize)
  pdf.options(pointsize=fontsize)
  ## 'lwd' is line width, logical size, multiple of 1/96 inch
  lwd <- round(targetlwd/shrinkage, 1)
  ## set logical line width
  parfv <- spatstat.options('par.fv')
  parfv$lwd <- lwd
  spatstat.options(par.fv = parfv)
}


## To set the plot margins in Sweave, the following function can be invoked
## To plot spatial objects without text in margins: 
##                   setmargins(0)
## For image plots with title and ribbon: 
##                   setmargins(0,0,2,2) 
## Sequence is bottom, left, top, right.

setmargins <- function(...) {
  z <- c(...)
  if(length(z) == 1) z <- rep(z, 4) else stopifnot(length(z) == 4)
  z <- z + 0.1
  setpar(mar=z)
  invisible(NULL)
}

## set all margins to zero and eliminate all outer spaces
zeromargins <- function() {
  setpar(
      mar=rep(0,4),
      omd=c(0,1,0,1),
      xaxs="i",
      yaxs="i"
  )
  invisible(NULL)
}


## ............... Functions that insert text .................
##
##  The following functions should be used inside an \Sexpr{}
##  They will insert the appropriate latex code
##  as well as setting the appropriate parameters.
## 

## Set widths
Initialise <- function(logicalwidth, logicalheight, physicalfraction) {
  ## reset all parameters; initialise scale
  newplot(logicalwidth, physicalfraction)
  ## tell latex
  out <- paste0("\\\\setkeys{Gin}{width=",
                if(physicalfraction == 1) "" else physicalfraction,
                "\\\\textwidth}")
  return(out)
}

## change the fraction of text width taken up by the figure
ChangeFraction <- function(physicalfraction) {
  ## reset scale
  widths(CurrentLogicalWidth, physicalfraction)
  ## tell latex
  out <- paste0("\\\\setkeys{Gin}{width=",
                if(physicalfraction == 1) "" else physicalfraction,
                "\\\\textwidth}")
  return(out)
}

renderPar <- function(z) if(is.character(z)) sQuote(z) else z

DumpPars <- function() {
  MFP <- lapply(MyFigurePars, renderPar)
  parval <- c(" ........ Graphics parameters: .............",
              paste("Postscript logical width =", CurrentLogicalWidth, "inch"),
              paste("Postscript logical font size =", LogicalFontSize, "pt"),
              paste("Latex figure width = ",
                    PhysicalFraction, "* \\\\textwidth"),
              "R graphics settings in par():",
              paste(names(MFP), "=", MFP),
              " ...........................................")
  parval <- paste("%% ", parval)
  out <- paste(parval, collapse="\\\\\\\\\n")
  return(out)
}
