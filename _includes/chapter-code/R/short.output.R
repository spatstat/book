## short.output.R
## Code to capture and truncate the output of any R command
## Copyright (C) 2015 Adrian Baddeley, Ege Rubak and Rolf Turner

short.output <- function(e, ..., excised=NULL, maxwidth=NULL){
  x <- match.call()
  x <- x[!(names(x) %in% c("excised", "maxwidth"))]
  if(length(x) > 2) {
    ## Specified output lines
    ll <- as.character(x[-(1:2)])
    ## Resolve ranges of line numbers ##
    index <- grep("-", ll)
    n <- length(index)
    if(n>0){
      r <- ll[index]
      ll <- ll[-index]
      for(i in 1:n){
        s <- as.numeric(unlist(strsplit(r[i], "-")))
        ll <- c(ll, seq(from = s[1], to = s[2]))
      }
    }
    ##
    ll <- as.numeric(ll)
    ll <- sort(ll)
  } else ll <- NULL
  ## Evaluate expression and capture output
  out <- capture.output(eval(e))
  ## retain only selected lines
  if(length(ll) > 0)
    out <- out[ll]
  ## remove NA's caused by indexing
  if(any(bad <- is.na(out))) {
    out <- out[!bad]
    ll <- ll[!bad]
  }
  ## insert a string like "[...]" where lines have been deleted
  nl <- length(ll)
  if(nl > 0 && !is.null(excised)) {
    newout <- character()
    for(i in 1:nl) {
      newout <- c(newout, out[i])
      if(i < nl && (ll[i+1] > ll[i] + 1))
        newout <- c(newout, excised)
    }
    out <- newout
  }
  if(!is.null(maxwidth))
    out <- substr(out, 1, maxwidth) 
  cat(out, sep = "\n")
}

skipblanklines <- function(e) {
  out <- capture.output(eval(e))
  for(i in seq_along(out))
    if(nchar(out[i]) > 0) spatstat:::splat(out[i])
}

## eg:
##  library(spatstat)
##  short.output(ppm(swedishpines~1,Strauss(9)), 3-5)
