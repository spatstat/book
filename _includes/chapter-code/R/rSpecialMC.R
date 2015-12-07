## rSpecialMC.R
## Code for generating simulated realisations of Matern Cluster process
## retaining all relevant parents and offspring
## For making illustrations only!
## Copyright (C) 2015 Adrian Baddeley, Ege Rubak and Rolf Turner

rSpecialMC <- local({
  rundisk <- function(n, radius) {
    R <- radius * sqrt(runif(n, min=0, max=1))
    Theta <- runif(n, min=0, max=2*pi)
    cbind(R * cos(Theta), R * sin(Theta))
  }
function (kappa, scale, mu, win = owin(c(0, 1), c(0, 1)), nsim = 1, 
    drop = TRUE, saveLambda = FALSE, expand = scale, allkids = FALSE, ...) 
{
    if (missing(scale)) 
        scale <- list(...)$r
    check.1.real(scale)
    stopifnot(scale > 0)
    result <- rSpecialNS(kappa, scale, list(mu, rundisk), win, 
        radius = scale, nsim = nsim, drop = FALSE,allkids=allkids)
    if (saveLambda) {
        for (i in 1:nsim) {
            parents <- attr(result[[i]], "parents")
            Lambda <- clusterfield("MatClust", parents, scale = scale, 
                mu = mu, ...)
            attr(result[[i]], "Lambda") <- Lambda[win]
        }
    }
    return(if (nsim == 1 && drop) result[[1]] else result)
}})
