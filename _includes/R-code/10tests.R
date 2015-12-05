### R code from vignette source '10tests.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 10tests.Rnw:9-10
###################################################
source("R/startup.R")


###################################################
### code chunk number 2: 10tests.Rnw:13-16
###################################################
# library(envel) # superseded
# requireversion(envel, "1.6-4")
requireversion(spatstat, "1.42-2.023")


###################################################
### code chunk number 3: 10tests.Rnw:19-22
###################################################
spatstat.options(terse=2)
Digits <- 3
options(digits=Digits)


###################################################
### code chunk number 4: 10tests.Rnw:145-159
###################################################
xx <- seq(-3,3,length=1024)
showdens <- function(x=xx) {
  plot(x, dnorm(x), type="l", main="", lwd=2,
       ylim=c(0, dnorm(0) * 1.1),
       xlab="t", ylab="probability density", axes=FALSE, xaxs='i', yaxs='i')
  box()
}
shadeup <- function(x0, ..., x=xx) {
  xup <- x[x >= x0]
  polygon(c(xup, rev(xup)),
          c(dnorm(xup), rep(0, length(xup))),
          ...)
  segments(x0, 0, x0, dnorm(x0), lwd=2)
}


###################################################
### code chunk number 5: fvSquat.Rnw:3-5
###################################################
newplot(6, 0.65)
setmargins(0.5+c(3,3,0,0))


###################################################
### code chunk number 6: 10tests.Rnw:164-165
###################################################
setmargins(3,1.5,0,0)


###################################################
### code chunk number 7: 10tests.Rnw:169-173
###################################################
showdens()
shadeup(1.2, border=NA, col="grey")
axis(1, at=1.2, labels=expression(t[obs]))
text(1.6, dnorm(1.6)/2, expression(p))


###################################################
### code chunk number 8: 10tests.Rnw:175-180
###################################################
showdens()
shadeup(0.6, border=NA, col="grey")
axis(1, at=0.6, labels=expression(t[crit]))
axis(1, at=1.2, labels=expression(t[obs]))
text(1.25, 0.07, expression(alpha))


###################################################
### code chunk number 9: 10tests.Rnw:297-299
###################################################
mur <- lapply(murchison, rescale, s=1000, unitname="km")
mur$dfault <- with(mur, distfun(faults))


###################################################
### code chunk number 10: 10tests.Rnw:303-305
###################################################
mfit0 <- ppm(gold ~ greenstone, data=mur)
mfit1 <- ppm(gold ~ greenstone + dfault, data=mur)


###################################################
### code chunk number 11: 10tests.Rnw:308-309
###################################################
mfit1 <- update(mfit0, . ~ . + dfault)


###################################################
### code chunk number 12: 10tests.Rnw:330-333
###################################################
Dist <- with(copper, distfun(SouthLines))
cfit0 <- ppm(SouthPoints ~ 1, data=copper)
cfit1 <- ppm(SouthPoints ~ Dist, data=copper)


###################################################
### code chunk number 13: 10tests.Rnw:374-375
###################################################
anova(mfit0, mfit1, test="LRT")


###################################################
### code chunk number 14: 10tests.Rnw:396-397
###################################################
anova(cfit0, cfit1, test="LRT")


###################################################
### code chunk number 15: chorleyload
###################################################
if(draftversion) {
  reload.or.compute(datafilepath("chorleyCoarse.rda"), {
    lung <- split(chorley)$lung
    larynx <- split(chorley)$larynx
    smo <- density(lung, sigma=0.15, eps=0.1)
    smo <- eval.im(pmax(smo, 1e-10))
    Q <- quadscheme(larynx, eps=0.5)
  })
  draftmessage <- 
      "using coarse quadrature scheme, eps=0.5, in draft version"
} else {
  reload.or.compute(datafilepath("chorley.rda"), {
    lung <- split(chorley)$lung
    larynx <- split(chorley)$larynx
    smo <- density(lung, sigma=0.15, eps=0.1)
    smo <- eval.im(pmax(smo, 1e-10))
    Q <- quadscheme(larynx, eps=0.1)
  })
  draftmessage <- NULL
}


###################################################
### code chunk number 16: 10tests.Rnw:425-428 (eval = FALSE)
###################################################
##   lung <- split(chorley)$lung
##   larynx <- split(chorley)$larynx
##   Q <- quadscheme(larynx, eps=0.1)


###################################################
### code chunk number 17: chorleyDfitAgain
###################################################
d2incin <- function(x, y, xincin=354.5, yincin=413.6) {
  (x - xincin)^2 + (y - yincin)^2
}
raisin <- function(x,y, alpha, beta) {
  1 + alpha * exp( - beta * d2incin(x,y))
}
chorleyDfit <- ippm(Q ~ offset(log(smo) + log(raisin)), 
                    start=list(alpha=5, beta=1))


###################################################
### code chunk number 18: 10tests.Rnw:447-448
###################################################
chorleyDfit


###################################################
### code chunk number 19: 10tests.Rnw:452-453
###################################################
chorley0fit <- update(chorleyDfit, . ~ offset(log(smo)))


###################################################
### code chunk number 20: 10tests.Rnw:468-469
###################################################
anova(chorley0fit, chorleyDfit, test="LRT")


###################################################
### code chunk number 21: 10tests.Rnw:513-514
###################################################
options(digits=3)


###################################################
### code chunk number 22: 10tests.Rnw:520-521
###################################################
coef(summary(mfit1))


###################################################
### code chunk number 23: 10tests.Rnw:527-528
###################################################
options(digits=Digits)


###################################################
### code chunk number 24: 10tests.Rnw:539-541
###################################################
V <- coef(summary(mfit1))["dfault", "Zval"]
pnorm(V, lower.tail=TRUE)


###################################################
### code chunk number 25: 10tests.Rnw:601-602
###################################################
anova(mfit0, mfit1, test="Rao")


###################################################
### code chunk number 26: 10tests.Rnw:667-668
###################################################
anova(cfit0, cfit1, test="score")


###################################################
### code chunk number 27: 10tests.Rnw:705-707
###################################################
dcop <- distfun(copper$SouthLines)
berman.test(cfit0, dcop)


###################################################
### code chunk number 28: 10tests.Rnw:903-906
###################################################
fit2e <- ppm(bei ~ polynom(elev,2), data=bei.extra)
M <- quadrat.test(fit2e, nx=4, ny=2)
M


###################################################
### code chunk number 29: Bei.Rnw:3-5
###################################################
newplot(12,0.8)
setmargins(0)


###################################################
### code chunk number 30: 10tests.Rnw:915-917
###################################################
plot(bei, main="", cols="lightgrey", cex=0.5)
plot(M, add=TRUE, col=if(monochrome) "black" else "red")


###################################################
### code chunk number 31: 10tests.Rnw:926-927
###################################################
rr <- round(range(residuals(M)))


###################################################
### code chunk number 32: 10tests.Rnw:971-972 (eval = FALSE)
###################################################
## quadrat.test(fit2e, nx=4, ny=2, CR=0)


###################################################
### code chunk number 33: 10tests.Rnw:985-987
###################################################
V <- quantess(Window(bei), bei.extra$grad, 4)
quadrat.test(fit2e, tess=V)


###################################################
### code chunk number 34: 10tests.Rnw:1018-1020
###################################################
X <- bdspots[[2]][square(c(-200,200))]
qX <- quadratcount(X, 20, 20)


###################################################
### code chunk number 35: 10tests.Rnw:1027-1029
###################################################
tX <- table(factor(as.numeric(qX), levels=0:max(qX)))
tX


###################################################
### code chunk number 36: 10tests.Rnw:1035-1036
###################################################
options(digits=2)


###################################################
### code chunk number 37: 10tests.Rnw:1038-1040
###################################################
eX <- 400 * dpois(0:max(qX), mean(qX))
eX


###################################################
### code chunk number 38: 10tests.Rnw:1044-1051
###################################################
fX <- factor(as.numeric(qX), levels=0:max(qX))
fX <- mergeLevels(fX, ">=5"=5:7)
tX <- table(fX)
tX
p04 <- dpois(0:4, mean(qX))
eX <- 400 * c(p04, 1 - sum(p04))
eX


###################################################
### code chunk number 39: 10tests.Rnw:1053-1054
###################################################
options(digits=Digits)


###################################################
### code chunk number 40: 10tests.Rnw:1057-1059
###################################################
X2 <- sum((tX - eX)^2/eX)
(pval <- pchisq(X2, df=length(tX)-1, lower.tail=FALSE))


###################################################
### code chunk number 41: 10tests.Rnw:1262-1264
###################################################
grad <- bei.extra$grad
cdf.test(fit2e, grad, test="ks")


###################################################
### code chunk number 42: 10tests.Rnw:1351-1363
###################################################
set.seed(19141917) 
## make list of 20 point patterns.
Plist <- list()
## First entry is 'cells' data
Plist[[1]] <- cells
## other entries are randomised
for(i in 2:20) 
  Plist[[i]] <- runifpoint(npoints(cells), Window(cells))
Plist <- as.solist(Plist)
## Evaluate Clark-Evans statistic for 5 patterns
clarkP <- unlist(lapply(Plist, clarkevans, correction="none"))
clarkP <- round(clarkP, 3)


###################################################
### code chunk number 43: 10tests.Rnw:1366-1367
###################################################
zeromargins()


###################################################
### code chunk number 44: 10tests.Rnw:1371-1374
###################################################
plot(Plist, main="", main.panel="", 
     equal.scales=TRUE, mar.panel=0, 
     hsep=1/2, vsep=1/2, nrows=4, pch=16, cex=0.8)


###################################################
### code chunk number 45: 10tests.Rnw:1384-1385
###################################################
set.seed(19141917) 


###################################################
### code chunk number 46: 10tests.Rnw:1409-1412
###################################################
opa <- options(width=80)
sort(clarkP[-1])
par(opa)


###################################################
### code chunk number 47: 10tests.Rnw:1578-1579
###################################################
(Tobs <- clarkevans(cells, correction="none"))


###################################################
### code chunk number 48: 10tests.Rnw:1585-1590
###################################################
Tsim <- numeric(19)
for(i in 1:19) {
  X <- runifpoint(npoints(cells), Window(cells))
  Tsim[i] <- clarkevans(X, correction="none")
}


###################################################
### code chunk number 49: 10tests.Rnw:1600-1603
###################################################
# check this!
if(!(Tobs > max(Tsim))) 
  stop("Unusual MC test result- pick another seed")


###################################################
### code chunk number 50: 10tests.Rnw:1608-1609
###################################################
(preg <- (1 + sum(Tsim > Tobs))/(1 + length(Tsim)))


###################################################
### code chunk number 51: 10tests.Rnw:1612-1613
###################################################
(pclus <- (1 + sum(Tsim < Tobs))/(1 + length(Tsim)))


###################################################
### code chunk number 52: 10tests.Rnw:1616-1617
###################################################
(peither <- 2 * min(pclus, preg))


###################################################
### code chunk number 53: 10tests.Rnw:1623-1626 (eval = FALSE)
###################################################
## quadrat.test(redwood, nx=5, alternative="clustered",
##              method="MonteCarlo", nsim=999)
## clarkevans.test(redwood, alternative="clustered", nsim=999)


###################################################
### code chunk number 54: 10tests.Rnw:1741-1747
###################################################
xxx <- seq(0, 10, length=1001)
scal <- 4 * sqrt(2 * pi)
set.seed(103)
xobs <- qnorm(0.9, mean=5)
xsim <- rnorm(10, mean=5)
ysim <- xobs + (xsim-5)


###################################################
### code chunk number 55: 10tests.Rnw:1752-1753
###################################################
setmargins(0)


###################################################
### code chunk number 56: 10tests.Rnw:1757-1791
###################################################
topline <- FALSE
if(topline) {
  plot(c(0,22),c(-2,19),type="n", axes=FALSE, xlab="", ylab="", main="")
} else {
  plot(c(0,22),c(-2,13),type="n", axes=FALSE, xlab="", ylab="", main="")
}
lines(xxx, scal * dnorm(xxx, mean=5))
points(xobs, 0, pch=16)
points(xsim, rep(0, length(xsim)), pch="|")
segments(0,6,10,6)
points(xobs, 6, pch=16)
lines(xxx, 8 + scal * dnorm(xxx, mean=5))
if(topline) {
  for(i in 2:8) lines(xxx, 14+scal*dnorm(xxx, mean=i), lty=3)
  for(i in 2:8) lines(12+xxx, 14+scal*dnorm(xxx, mean=i), lty=3)
}
for(i in 5:9) lines(12+xxx, 8+scal*dnorm(xxx, mean=i), lty=if(i==5) 1 else 3)
segments(12,6,22,6)
points(12+xobs, 6, pch=16)
points(12+xobs, 0, pch=16)
lines(12+xxx, scal*dnorm(xxx, mean=xobs), lty=2)
points(12+ysim, rep(0,length(ysim)), pch="|")
if(topline) {
  text(5, 18.5, "Model")
  text(17, 18.5, "Model")
}
text(5, 12.5, "Simple null hypothesis")
text(17, 12.5, "Composite null hypothesis")
text(5.5, 6.7, "Data")
text(17.5, 6.7, "Data")
text(2.3,3, "Null model")
text(14,3, "Fitted null model")
text(5, -1.25, "Simulations from null model")
text(17, -1.25, "Simulations from fitted model")


###################################################
### code chunk number 57: 10tests.Rnw:1852-1861
###################################################
dotest <- function(X, nsim=19) {
   Tobs <- clarkevans(X, correction="none")
   Tsim <- numeric(nsim)
   for(i in 1:nsim) {
     Xsim <- rpoispp(intensity(X), win=Window(X))
     Tsim[i] <- clarkevans(Xsim, correction="none")
  }
  return(Tobs > max(Tsim))
}


###################################################
### code chunk number 58: 10tests.Rnw:1869-1877 (eval = FALSE)
###################################################
## N <- 10000
## lambda <- 50
## rejected <- logical(N)
## for(i in 1:N) {
##     X <- rpoispp(lambda)
##     rejected[i] <- dotest(X)
## }
## alpha <- mean(rejected)


###################################################
### code chunk number 59: 10tests.Rnw:2025-2027
###################################################
set.seed(11111918)
KE <- envelope(cells, Kest, nsim=39)


###################################################
### code chunk number 60: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 61: 10tests.Rnw:2032-2033
###################################################
plot(KE, main="", lwd=4, legend=FALSE)


###################################################
### code chunk number 62: 10tests.Rnw:2162-2164
###################################################
mad.test(cells, Lest, nsim=99, ginterval=c(0, 0.2), 
         fix.n=TRUE, verbose=FALSE)


###################################################
### code chunk number 63: 10tests.Rnw:2186-2190
###################################################
set.seed(28283)
envKglob19 <- envelope(cells, Kest, nsim=19, fix.n=TRUE, global=TRUE,
                       savepatterns=TRUE)
envLglob19 <- envelope(envKglob19, Lest, global=TRUE, savefuns=TRUE)


###################################################
### code chunk number 64: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 65: 10tests.Rnw:2195-2198
###################################################
par(mfrow=c(1,2))
plot(envKglob19, main="", lwd=c(4,1,1,1), legend=FALSE)
plot(envLglob19, main="", lwd=c(4,1,1,1), legend=FALSE)


###################################################
### code chunk number 66: 10tests.Rnw:2300-2301
###################################################
dclf.test(cells, Lest, nsim=99, fix.n=TRUE, verbose=FALSE)


###################################################
### code chunk number 67: 10tests.Rnw:2377-2380
###################################################
set.seed(4242422)
dc <- dclf.test(cells, Lest, nsim=19, fix.n=TRUE, alternative="less",
                verbose=FALSE)


###################################################
### code chunk number 68: 10tests.Rnw:2382-2383 (eval = FALSE)
###################################################
## dclf.test(cells, Lest, nsim=19, fix.n=TRUE, alternative="less")


###################################################
### code chunk number 69: 10tests.Rnw:2385-2386
###################################################
dc


###################################################
### code chunk number 70: 10tests.Rnw:2439-2441 (eval = FALSE)
###################################################
## plot(envelope(redwood, Lest, nsim=39))
## plot(envelope(redwood, Lest, nsim=39, global=TRUE))


###################################################
### code chunk number 71: 10tests.Rnw:2444-2446
###################################################
EP <- envelope(redwood, Lest, nsim=39, savefuns=TRUE)
EG <- envelope(EP, global=TRUE)


###################################################
### code chunk number 72: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 73: 10tests.Rnw:2451-2454
###################################################
par(mfrow=c(1,2))
plot(EP, main="", legend=FALSE)
plot(EG, main="", legend=FALSE)


###################################################
### code chunk number 74: 10tests.Rnw:2592-2596 (eval = FALSE)
###################################################
## numata <- residualspaper$Fig1
## fit <- ppm(numata ~ polynom(x,y,3))
## E <- envelope(fit, Lest, nsim=19, global=TRUE, correction="border")
## plot(E)


###################################################
### code chunk number 75: 10tests.Rnw:2603-2605 (eval = FALSE)
###################################################
## B   <- bdspots[[1]]
## fit <- ppm(B ~ 1, Hardcore())


###################################################
### code chunk number 76: 10tests.Rnw:2609-2610 (eval = FALSE)
###################################################
## EH <- envelope(fit, Lest, nsim=39, global=TRUE)


###################################################
### code chunk number 77: 10tests.Rnw:2629-2630 (eval = FALSE)
###################################################
## e <- expression(rpoispp(100))


###################################################
### code chunk number 78: 10tests.Rnw:2644-2649 (eval = FALSE)
###################################################
## n <- npoints(B)
## W <- Window(B)
## r <- min(nndist(B))*n/(n+1)
## e <- expression(rSSI(r, n, W))
## E <- envelope(B, Lest, nsim=39, global=TRUE, simulate=e)


###################################################
### code chunk number 79: 10tests.Rnw:2664-2667 (eval = FALSE)
###################################################
## e <- expression(rlabel(amacrine))
## E <- envelope(amacrine, Lcross, nsim=19, global=TRUE, simulate=e)
## plot(E)


###################################################
### code chunk number 80: 10tests.Rnw:2682-2684 (eval = FALSE)
###################################################
## Xlist <- runifpoint(42, nsim=99)
## envelope(cells, Kest, nsim=99, simulate=Xlist)


###################################################
### code chunk number 81: 10tests.Rnw:2689-2691 (eval = FALSE)
###################################################
## EK <- envelope(cells, Kest, nsim=99, savepatterns=TRUE)
## Ep <- envelope(cells, pcf,  nsim=99, simulate=EK)


###################################################
### code chunk number 82: 10tests.Rnw:2702-2704 (eval = FALSE)
###################################################
## EK <- envelope(cells, Kest, nsim=99, savepatterns=TRUE)
## Ep <- envelope(EK, pcf)


###################################################
### code chunk number 83: 10tests.Rnw:2711-2713 (eval = FALSE)
###################################################
## A <- mad.test(cells, Lest, nsim=99, savefuns=TRUE)
## B <- dclf.test(A)


###################################################
### code chunk number 84: 10tests.Rnw:2774-2776 (eval = FALSE)
###################################################
## D <- density(X)
## envelope(X, Kinhom, simulate=expression(rpoispp(D)))


###################################################
### code chunk number 85: 10tests.Rnw:2789-2791 (eval = FALSE)
###################################################
## D <- density(X)
## envelope(X, Kinhom, lambda=D, simulate=expression(rpoispp(D)))


###################################################
### code chunk number 86: 10tests.Rnw:2840-2841 (eval = FALSE)
###################################################
## envelope(redwood, Lest, global=TRUE)


###################################################
### code chunk number 87: 10tests.Rnw:2880-2883 (eval = FALSE)
###################################################
## fisher <- function(x) { asin(sqrt(x)) }
## envelope(redwood, Fest, global=TRUE, 
##          transform=expression(fisher(.)))


###################################################
### code chunk number 88: 10tests.Rnw:2902-2903 (eval = FALSE)
###################################################
## envelope(cells, Kest, nsim=100, VARIANCE=TRUE)


###################################################
### code chunk number 89: 10tests.Rnw:2921-2923
###################################################
a1 <- envelope(cells, Lest, nsim=19, alternative="less")
a2 <- envelope(cells, Lest, nsim=19, alternative="greater")


###################################################
### code chunk number 90: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 91: 10tests.Rnw:2928-2931
###################################################
plot(anylist(a1, a2), 
     main="", main.panel="", equal.scales=TRUE,
     mar.panel=c(4,4,1,1), hsep=1, legend=FALSE)


###################################################
### code chunk number 92: 10tests.Rnw:2962-2965 (eval = FALSE)
###################################################
## E1 <- envelope(redwood, Kest, savepatterns=TRUE)
## E2 <- envelope(E1, Gest, global=TRUE, 
##                transform=expression(fisher(.)))


###################################################
### code chunk number 93: 10tests.Rnw:2972-2975 (eval = FALSE)
###################################################
## A1 <- envelope(redwood, Kest, nsim=39, savefuns=TRUE)
## A2 <- envelope(A1, global=TRUE, nsim=19, 
##                transform=expression(sqrt(./pi)))


###################################################
### code chunk number 94: 10tests.Rnw:2991-2994 (eval = FALSE)
###################################################
## E1 <- envelope(cells, Kest, nsim=10, savefuns=TRUE)
## E2 <- envelope(cells, Kest, nsim=20, savefuns=TRUE)
## E <- pool(E1, E2)


###################################################
### code chunk number 95: 10tests.Rnw:3023-3024 (eval = FALSE)
###################################################
## dclf.sigtrace(cells, Lest, nsim=19)


###################################################
### code chunk number 96: 10tests.Rnw:3026-3027
###################################################
a <- dclf.sigtrace(cells, Lest, nsim=19)


###################################################
### code chunk number 97: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 98: 10tests.Rnw:3044-3045
###################################################
plot(a, main="")


###################################################
### code chunk number 99: 10tests.Rnw:3085-3089
###################################################
set.seed(1234567)
X <- rescale(swedishpines)
prog19   <- dclf.progress(X, Lest, nsim=19)
prog1999 <- dclf.progress(X, Lest, nsim=1999, nrank=100)


###################################################
### code chunk number 100: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 101: 10tests.Rnw:3096-3099
###################################################
par(mfrow=c(1,2))
plot(prog19, main="", legend=FALSE)
plot(prog1999, main="", legend=FALSE)


###################################################
### code chunk number 102: 10tests.Rnw:3129-3131 (eval = FALSE)
###################################################
## X <- rescale(swedishpines)
## plot(dclf.progress(X, Lest, nsim=19))


###################################################
### code chunk number 103: 10tests.Rnw:3220-3221
###################################################
D <- dg.test(cells, Lest, nsim=19)


###################################################
### code chunk number 104: 10tests.Rnw:3223-3224 (eval = FALSE)
###################################################
## dg.test(cells, Lest, nsim=19)


###################################################
### code chunk number 105: 10tests.Rnw:3226-3227
###################################################
D


###################################################
### code chunk number 106: 10tests.Rnw:3232-3233 (eval = FALSE)
###################################################
## dg.test(cells, Lest, nsim=19, exponent=Inf)


###################################################
### code chunk number 107: 10tests.Rnw:3279-3280
###################################################
set.seed(180981)


###################################################
### code chunk number 108: 10tests.Rnw:3282-3285
###################################################
swp <- rescale(swedishpines)
E <- dg.envelope(swp, Lest)
Eo <- dg.envelope(swp, Lest, alternative="less")


###################################################
### code chunk number 109: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 110: 10tests.Rnw:3291-3294
###################################################
plot(anylist(E, Eo), 
     main="", main.panel="", equal.scales=TRUE,
     mar.panel=c(4,4,1,1), hsep=1, legend=FALSE)


