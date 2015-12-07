## shortestpath.R
## Code to find and display the shortest path between vertices on a network.
## For making illustrations only!
## Copyright (C) 2015 Adrian Baddeley, Ege Rubak and Rolf Turner

shortestpath <- function(L, i, j) {
  L <- as.linnet(L, sparse=FALSE)
  d <- L$dpath
  m <- L$m
  to <- L$to
  from <- L$from
  path <- i
  leader <- i
  repeat {
    k <- setdiff(which(m[leader,]), path)
    leader <- k[which.min(d[i,k] + d[k, j])]
    path <- c(path, leader)
    if(leader == j) break
  }
  return(path)
}

joinpoints <- function(X, i, ...) {
  co <- coords(X[i])
  lines(co[,1], co[,2], ...)
}
