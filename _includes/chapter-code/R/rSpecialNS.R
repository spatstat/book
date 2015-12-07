## rSpecialNS.R
## Code for generating simulated realisations of Neyman=Scott Cox models
## retaining all relevant parents and offspring
## For making illustrations only!
## Copyright (C) 2015 Adrian Baddeley, Ege Rubak and Rolf Turner

rSpecialNS <- function (kappa, expand, rcluster, win = owin(c(0, 1), c(0, 1)), 
    ..., lmax = NULL, nsim = 1, drop = TRUE, allkids = FALSE) 
{
    if (missing(expand) && !is.null(rmax <- list(...)$rmax)) 
        expand <- rmax
    if (is.function(rcluster)) 
        return(rPoissonCluster(kappa, expand, rcluster, win, 
            ..., lmax = lmax, nsim = nsim, drop = drop))
    if (!(is.list(rcluster) && length(rcluster) == 2)) 
        stop("rcluster should be either a function, or a list of two elements")
    win <- as.owin(win)
    mu <- rcluster[[1]]
    rdisplace <- rcluster[[2]]
    if (is.numeric(mu)) {
        if (!(length(mu) == 1 && mu >= 0)) 
            stop("rcluster[[1]] should be a single nonnegative number")
        mumax <- mu
    }
    else if (is.im(mu) || is.function(mu)) {
        if (is.function(mu)) 
            mu <- as.im(mu, W = win)
        mumax <- max(mu)
    }
    else stop("rcluster[[1]] should be a number, a function or a pixel image")
    if (!is.function(rdisplace)) 
        stop("rcluster[[2]] should be a function")
    frame <- boundingbox(win)
    dilated <- grow.rectangle(frame, expand)
    if (is.im(kappa) && !is.subset.owin(dilated, as.owin(kappa))) 
        stop(paste("The window in which the image", sQuote("kappa"), 
            "is defined\n", "is not large enough to contain the dilation of the window", 
            sQuote("win")))
    parentlist <- rpoispp(kappa, lmax = lmax, win = dilated, 
        nsim = nsim, drop = FALSE)
    resultlist <- vector(mode = "list", length = nsim)
    for (i in 1:nsim) {
        parents <- parentlist[[i]]
        np <- npoints(parents)
        if (np == 0) {
            result <- ppp(numeric(0), numeric(0), window = win)
            parentid <- integer(0)
        }
        else {
            csize <- rpois(np, mumax)
            noff <- sum(csize)
            xparent <- parents$x
            yparent <- parents$y
            x0 <- rep.int(xparent, csize)
            y0 <- rep.int(yparent, csize)
            dd <- rdisplace(noff, ...)
            mm <- if (is.ppp(dd)) 
                marks(dd)
            else NULL
            xy <- xy.coords(dd)
            dx <- xy$x
            dy <- xy$y
            if (!(length(dx) == noff)) 
                stop("rcluster returned the wrong number of points")
            xoff <- x0 + dx
            yoff <- y0 + dy
            parentid <- rep.int(1:np, csize)
            retain <- inside.owin(xoff, yoff, win)
            if (is.im(mu)) 
                retain[retain] <- inside.owin(xoff[retain], yoff[retain], 
                  as.owin(mu))
            if(allkids) {
                ok <- inside.owin(xoff,yoff,dilated)
                xallkids <- xoff[ok]
                yallkids <- yoff[ok]
                pid.all <- parentid[ok]
            }
            xoff <- xoff[retain]
            yoff <- yoff[retain]
            parentid <- parentid[retain]
            if (!is.null(mm)){ 
                mmall <- if(allkids) mm else NULL
                mm    <- marksubset(mm, retain)
            } else mmall <- NULL
            result <- ppp(xoff, yoff, window = win, check = FALSE, 
                marks = mm)
        }
        if (is.im(mu)) {
            P <- eval.im(mu/mumax)
            result <- rthin(result, P)
        }
        attr(result, "parents") <- parents
        attr(result, "parentid") <- parentid
        attr(result, "expand") <- expand
        if(allkids) {
           ak <- ppp(xallkids, yallkids, window = dilated,
                     check = FALSE, marks = mmall)
           ak <- ak[dilated]
           attr(result,"allkids") <- ak
           attr(result,"pid.all") <- pid.all
        }
        resultlist[[i]] <- result
    }
    if (nsim == 1 && drop) 
        return(resultlist[[1]])
    names(resultlist) <- paste("Simulation", 1:nsim)
    return(as.solist(resultlist))
}
