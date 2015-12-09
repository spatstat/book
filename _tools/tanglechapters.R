## Script to to tangle chapters from book dir and add copyright notice.
## Shouldn't be run on a regular basis and care is needed.
## Assumes it runs from _tools dir and that it is in a specific relative path to book dir

curdir <- getwd()
setwd("..")
wwwdir <- getwd()
setwd("../book/src")
infiles <- list.files(pattern = "[0-1][0-9].*Rnw$")
infiles <- infiles[-c(9, 12, 13)]
outfiles <- paste0(file.path(wwwdir, "_includes", "R-code", sub("Rnw$", "R", infiles)))
for(i in seq_along(infiles)){
  Stangle(infiles[i], output = outfiles[i])
  content <- readLines(outfiles[i])
  writeLines(c(content[1],
               "## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner",
               content[-1]),
               outfiles[i])
}
setwd(curdir)