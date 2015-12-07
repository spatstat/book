### R code from vignette source '11validate.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 11validate.Rnw:9-15
###################################################
source("R/startup.R")
Digits <- 4
dopt <- options(digits=Digits)
Terse <- 3
spatstat.options(terse=Terse)
requireversion(spatstat, "1.41-1.073")


###################################################
### code chunk number 2: 11validate.Rnw:263-264
###################################################
fit2e <- ppm(bei ~ polynom(elev,2), data=bei.extra)


###################################################
### code chunk number 3: 11validate.Rnw:300-302
###################################################
lam0 <- fitted(fit2e, dataonly=TRUE)
rel2e <- density(bei, weights=1/lam0)


###################################################
### code chunk number 4: BeiR.Rnw:3-5
###################################################
newplot(13,0.95)
setmargins(0,0,0,1)


###################################################
### code chunk number 5: 11validate.Rnw:309-312
###################################################
coco <- if(monochrome) grey(seq(0,1,length=128)) else NULL
plot(rel2e, main="", col=coco, log=TRUE, ribargs=list(las=1))
contour(rel2e, add=TRUE, drawlabels=FALSE)


###################################################
### code chunk number 6: 11validate.Rnw:328-330 (eval = FALSE)
###################################################
## lam0 <- fitted(fit2e, dataonly=TRUE)
## rel2e <- density(bei, weights=1/lam0)


###################################################
### code chunk number 7: 11validate.Rnw:334-335
###################################################
range(rel2e)


###################################################
### code chunk number 8: 11validate.Rnw:372-375
###################################################
lambda0 <- predict(fit2e)
grad <- bei.extra$grad
rh1 <- rhohat(bei, grad, baseline=lambda0)


###################################################
### code chunk number 9: 11validate.Rnw:380-381
###################################################
rh2 <- rhohat(fit2e, grad)


###################################################
### code chunk number 10: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 11: 11validate.Rnw:394-396
###################################################
plot(rh1, main="", legend=FALSE)
abline(1,0,lty=2)


###################################################
### code chunk number 12: 11validate.Rnw:526-527
###################################################
spatstat.options(terse=2)


###################################################
### code chunk number 13: 11validate.Rnw:532-534
###################################################
res2e <- residuals(fit2e)
res2e


###################################################
### code chunk number 14: 11validate.Rnw:536-537
###################################################
spatstat.options(terse=Terse)


###################################################
### code chunk number 15: BeiR.Rnw:3-5
###################################################
newplot(13,0.95)
setmargins(0,0,0,1)


###################################################
### code chunk number 16: 11validate.Rnw:566-568
###################################################
plot(res2e, col=lighttodark, main="", legend=FALSE, pch=".",cex=2, 
     ribscale=1000)


###################################################
### code chunk number 17: 11validate.Rnw:591-592
###################################################
integral(res2e, square(200))


###################################################
### code chunk number 18: 11validate.Rnw:598-600
###################################################
qua <- quadrats(bei, 4, 2)
resQ <- integral(res2e, qua)


###################################################
### code chunk number 19: 11validate.Rnw:604-605 (eval = FALSE)
###################################################
## plot(qua, do.labels=TRUE, labels=signif(resQ, 2))


###################################################
### code chunk number 20: Bei.Rnw:3-5
###################################################
newplot(12,0.8)
setmargins(0)


###################################################
### code chunk number 21: 11validate.Rnw:613-614
###################################################
plot(qua, do.labels=TRUE, labels=signif(resQ, 2), main="")


###################################################
### code chunk number 22: 11validate.Rnw:630-631
###################################################
smo2e <- Smooth(res2e)


###################################################
### code chunk number 23: BeiR.Rnw:3-5
###################################################
newplot(13,0.95)
setmargins(0,0,0,1)


###################################################
### code chunk number 24: 11validate.Rnw:638-640
###################################################
plot(smo2e, main="", col=darktolight, ribscale=1000)
contour(smo2e, add=TRUE, drawlabels=FALSE, col="white")


###################################################
### code chunk number 25: 11validate.Rnw:736-737 (eval = FALSE)
###################################################
## lurking(fit2e, grad, type="raw")


###################################################
### code chunk number 26: 11validate.Rnw:739-743
###################################################
set.seed(42)
lurkA <- lurking(fit2e, grad, type="raw", plot.it=FALSE)
lurkB <- lurking(fit2e, grad, type="raw", plot.it=FALSE,
                 cumulative=FALSE, envelope=TRUE)


###################################################
### code chunk number 27: lurk2.Rnw:3-5
###################################################
newplot(9, 1)
setmargins(4,5,1,1)


###################################################
### code chunk number 28: 11validate.Rnw:748-751
###################################################
par(mfrow=c(1,2), cex=0.75)
plot(lurkA)
plot(lurkB)


###################################################
### code chunk number 29: 11validate.Rnw:785-786 (eval = FALSE)
###################################################
## lurking(fit2e, grad, type="raw", cumulative=FALSE, envelope=TRUE)


###################################################
### code chunk number 30: Diagnosebei.Rnw:3-5
###################################################
newplot(9,1)
setmargins(1.5)


###################################################
### code chunk number 31: 11validate.Rnw:823-824
###################################################
setmargins(0.2)


###################################################
### code chunk number 32: 11validate.Rnw:828-830
###################################################
diagnose.ppm(fit2e, pch=".", cex=1.5,
             plot.neg="contour", legend=FALSE)


###################################################
### code chunk number 33: 11validate.Rnw:926-927
###################################################
set.seed(19310605)


###################################################
### code chunk number 34: 11validate.Rnw:929-933
###################################################
Pat <- rpoispp(function(x,y){50 * exp(2*x)})
fitpat <- ppm(Pat ~ x)
resI <- residuals(fitpat, type="inverse")
resP <- residuals(fitpat, type="Pearson")


###################################################
### code chunk number 35: Unit3R.Rnw:3-5
###################################################
newplot(22,0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 36: 11validate.Rnw:947-948
###################################################
setmargins(0,0,0,1)


###################################################
### code chunk number 37: 11validate.Rnw:952-961
###################################################
pa <- function(i) {
  switch(i, 
         list(),
         list(col=grey(0.8), lwd=2),
         list(lwd=2))
}
plot(solist(Pat, resI, resP),
     main="", main.panel="", nrows=1, 
     panel.args=pa, equal.scales=TRUE, mar.panel=0, hsep=2)


###################################################
### code chunk number 38: 11validate.Rnw:1026-1029
###################################################
B <- owin(c(0.1, 0.3), c(0.4, 0.6))
integral(resI, B)
integral(resP, B)


###################################################
### code chunk number 39: 11validate.Rnw:1033-1035
###################################################
integral(resI)
integral(resP)


###################################################
### code chunk number 40: 11validate.Rnw:1088-1089
###################################################
pres2e <- residuals(fit2e, type="pearson")


###################################################
### code chunk number 41: 11validate.Rnw:1097-1101
###################################################
psmo2e <- Smooth(pres2e)
psm <- layered(psmo2e, psmo2e)
layerplotargs(psm) <- list(list(col=darktolight, ribargs=list(las=1)),
                           list(.plot="contour", col="white", drawlabels=FALSE))


###################################################
### code chunk number 42: Bei2stackLR.Rnw:3-5
###################################################
newplot(12,0.8)
setmargins(0.2)


###################################################
### code chunk number 43: 11validate.Rnw:1107-1115
###################################################
pane <- function(i) {
  if(i > 1) list() else list(how="contour", drawlabels=FALSE, 
                             maxsize=40)
}
plot(solist(pres2e, psm), 
     ncols=1, halign=TRUE,
     main="", main.panel="", equal.scales=TRUE,
     panel.args=pane, mar.panel=0.2, pch=3)


###################################################
### code chunk number 44: 11validate.Rnw:1138-1139
###################################################
ss <- 1/(2 * sqrt(pi) * attr(psmo2e, "sigma"))


###################################################
### code chunk number 45: 11validate.Rnw:1141-1142 (eval = FALSE)
###################################################
## psmo2e <- Smooth(pres2e)


###################################################
### code chunk number 46: 11validate.Rnw:1144-1145
###################################################
1/(2 * sqrt(pi) * attr(psmo2e, "sigma"))


###################################################
### code chunk number 47: 11validate.Rnw:1158-1163
###################################################
beikm <- rescale(bei, s=1000, unitname="km")
beikm.extra <- lapply(bei.extra, rescale, 
                      s=1000, unitname="km")
fit2eKM <- ppm(beikm ~ polynom(elev, 2), data=beikm.extra)
pres2eKM <- residuals(fit2eKM, type="Pearson")


###################################################
### code chunk number 48: 11validate.Rnw:1200-1201
###################################################
resS <- residuals(fitpat, type="score")


###################################################
### code chunk number 49: Unit2LRboth.Rnw:4-6
###################################################
newplot(13, 1)
setmargins(0,2,0,2)


###################################################
### code chunk number 50: 11validate.Rnw:1204-1205
###################################################
setmargins(0)


###################################################
### code chunk number 51: 11validate.Rnw:1209-1211
###################################################
plot(resS, main="", mar.panel=c(0,0,0,1), hsep=1.5,
     ribargs=list(las=1))


###################################################
### code chunk number 52: 11validate.Rnw:1240-1244
###################################################
fit1g <- ppm(bei ~grad, data=bei.extra)
par1g <- parres(fit1g, "grad")
fit2g <- update(fit1g, ~polynom(grad,2))
par2g <- parres(fit2g, "grad")


###################################################
### code chunk number 53: 11validate.Rnw:1252-1253 (eval = FALSE)
###################################################
## fit1g <- ppm(bei ~ grad, data=bei.extra)


###################################################
### code chunk number 54: 11validate.Rnw:1270-1272 (eval = FALSE)
###################################################
## par1g <- parres(fit1g, "grad")
## plot(par1g, xlim=c(0,0.25))


###################################################
### code chunk number 55: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 56: 11validate.Rnw:1286-1289
###################################################
plot(anylist(par1g, par2g),
     main="", main.panel="",
     legend=FALSE, xlim=c(0, 0.25), lwd=2)


###################################################
### code chunk number 57: 11validate.Rnw:1304-1305 (eval = FALSE)
###################################################
## fit2g <- update(fit1g, ~polynom(grad,2))


###################################################
### code chunk number 58: 11validate.Rnw:1312-1313
###################################################
anova(fit2g, test="LRT")


###################################################
### code chunk number 59: 11validate.Rnw:1554-1556
###################################################
elev <- bei.extra$elev
addEfit2g <- addvar(fit2g, elev)


###################################################
### code chunk number 60: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 61: addvar
###################################################
plot(addEfit2g, main="", legend=FALSE, lwd=2)


###################################################
### code chunk number 62: 11validate.Rnw:1734-1736
###################################################
fitPois <- ppm(cells ~1, forcefit=TRUE)
diaPois <- diagnose.ppm(fitPois, plot.it=FALSE)


###################################################
### code chunk number 63: Diagnose.Rnw:3-5
###################################################
newplot(10, 0.8)
setmargins(1.5)


###################################################
### code chunk number 64: 11validate.Rnw:1739-1740
###################################################
setmargins(0.1)


###################################################
### code chunk number 65: 11validate.Rnw:1744-1745
###################################################
plot(diaPois, col.neg=grey(0.65), cols="white")


###################################################
### code chunk number 66: 11validate.Rnw:1801-1802
###################################################
qqp <- qqplot.ppm(fitPois,plot.it=FALSE,nsim=39)


###################################################
### code chunk number 67: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 68: 11validate.Rnw:1805-1806
###################################################
setmargins(0.5+c(4,3,0,0))


###################################################
### code chunk number 69: 11validate.Rnw:1810-1811
###################################################
plot(qqp, main="", monochrome=monochrome)


###################################################
### code chunk number 70: 11validate.Rnw:1923-1925
###################################################
cellKr <- Kres(cells, correction="best")
cellGr <- Gres(cells, correction="best")


###################################################
### code chunk number 71: 11validate.Rnw:1937-1940
###################################################
## set 'shade variables' for plots
fvnames(cellKr, ".s") <- c("ihi", "ilo")
fvnames(cellGr, ".s") <- c("hi", "lo")


###################################################
### code chunk number 72: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 73: 11validate.Rnw:1946-1949
###################################################
plot(anylist(cellKr,cellGr), main="", main.panel="", 
     legend=FALSE, lwd=2, col=1, 
     mar.panel=0.2+c(4,4,0,0), hsep=2)


###################################################
### code chunk number 74: 11validate.Rnw:1967-1969
###################################################
jfit <- ppm(residualspaper$Fig1 ~ polynom(x,y,3))
jKr <- Kres(jfit, correction="best")


###################################################
### code chunk number 75: 11validate.Rnw:1975-1976
###################################################
fvnames(jKr, ".s") <- c("ihi", "ilo")


###################################################
### code chunk number 76: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 77: 11validate.Rnw:1979-1980
###################################################
setmargins(0.2+c(4,4,0,0))


###################################################
### code chunk number 78: 11validate.Rnw:1984-1985
###################################################
plot(jKr, main="", legend=FALSE, lwd=2, col=1)


###################################################
### code chunk number 79: murchSetup
###################################################
mur <- lapply(murchison, rescale, s=1000, unitname="km")
attach(mur)
green <- greenstone
dfault <- distfun(faults)


###################################################
### code chunk number 80: murfit1x
###################################################
murfit1x <- ppm(gold ~ green * dfault, eps=1)


###################################################
### code chunk number 81: murlev1x
###################################################
if(draftversion) {
  reload.or.compute(datafilepath("murlev1xCoarse.rda"), {
    murlev1x <- leverage(murfit1x)
    murinf1x <- influence(murfit1x)
    murdfb1x <- dfbetas(murfit1x)
  })
} else {
  reload.or.compute(datafilepath("murlev1x.rda"), {
    murlev1x <- leverage(murfit1x)
    murinf1x <- influence(murfit1x)
    murdfb1x <- dfbetas(murfit1x)
  })
}  


###################################################
### code chunk number 82: 11validate.Rnw:2292-2296 (eval = FALSE)
###################################################
## murfit1x <- ppm(gold ~ green * dfault, eps=1)
## murlev1x <- leverage(murfit1x)
## murinf1x <- influence(murfit1x)
## murdfb1x <- dfbetas(murfit1x)


###################################################
### code chunk number 83: 11validate.Rnw:2302-2306
###################################################
persp(as.im(murlev1x), 
      col=if(monochrome) "yellow" else "grey", border=NA, 
      theta=-30, phi=25, shade=0.75, main="",
      zlab="leverage")


###################################################
### code chunk number 84: 11validate.Rnw:2326-2329
###################################################
## boxes surrounding highly influential points
murcorner <- owin(c(635, 682.6), c(7066, 7095))
murzoom <- owin(c(377,438), c(6890, 6926))


###################################################
### code chunk number 85: 11validate.Rnw:2331-2333
###################################################
## determine space for influence plot
B <- as.owin(plot(murinf1x, do.plot=FALSE))


###################################################
### code chunk number 86: MurchisonL.Rnw:3-5
###################################################
newplot(9, 0.5)
setmargins(0.1)


###################################################
### code chunk number 87: 11validate.Rnw:2340-2344
###################################################
plot(B, main="", type="n")
plot(murcorner, col=grey(0.7), border=NA, add=TRUE)
plot(murzoom, col=grey(0.7), border=NA, add=TRUE)
plot(murinf1x, add=TRUE, show.all=TRUE, main="")


###################################################
### code chunk number 88: 11validate.Rnw:2373-2393
###################################################
MurDetail <- function(b, k=NULL) { 
  ## plot detail of the Murchison data in rectangle 'b'
  with(mur, {
    plot(b, type="n", main="")
    plot(greenstone[b], add=TRUE,
         border=NULL, col=if(monochrome) "grey" else "lightgreen")
    plot(faults[b], add=TRUE, lwd=3)
    plot(gold[b], add=TRUE, pch=1, cex=2.5, lwd=2)
    if(!is.null(k))
      plot(gold[k], add=TRUE, pch=16, cex=2.5, lwd=2)
    plot(b, add=TRUE, lty=3, lwd=2)
  })
}
murlayered <- with(mur, 
                   layered(as.rectangle(gold), greenstone, faults, gold))
layerplotargs(murlayered) <- list(
  list(),                                
  list(border=NULL, col=if(monochrome) "grey" else "lightgreen"),
  list(lwd=3),
  list(pch=10, cex=1.2, lwd=2))


###################################################
### code chunk number 89: MurchCorner2.Rnw:3-5
###################################################
newplot(10, 1)
setmargins(0.2)


###################################################
### code chunk number 90: 11validate.Rnw:2400-2404
###################################################
plot(solist(murlayered[,murcorner], murinf1x[murcorner]),
     main="", main.panel="", equal.scales=TRUE,
     leg.side="right", mar.panel=0.3+c(0,0,0,2.5), hsep=-1.5,
     leg.args=list(sep.frac = 0.125))


###################################################
### code chunk number 91: MurchOutlier2.Rnw:3-5
###################################################
newplot(10.8, 1)
setmargins(0.2)


###################################################
### code chunk number 92: 11validate.Rnw:2424-2427
###################################################
plot(solist(murlayered[,murzoom], murinf1x[murzoom]),
     main="", main.panel="", equal.scales=TRUE,
     leg.side="right", mar.panel=0.3+c(0,0,0,2.2), hsep=-1.5)


###################################################
### code chunk number 93: augmentx
###################################################
## This code helps to accelerate the plotting 
murdfb1x <- augment.msr(murdfb1x)


###################################################
### code chunk number 94: MurchOutlier2x2.Rnw:3-5
###################################################
newplot(14, 1)
setmargins(0.2)


###################################################
### code chunk number 95: 11validate.Rnw:2464-2468
###################################################
plot(murdfb1x[murzoom], main="", mar.panel=0.2+c(0,1,1,2)/2, 
     hsep=2, vsep=1, etch=TRUE,
     cex.main=0.85, ribargs=list(las=1), ribscale=1e6,
     leg.args=list(labelmap=1000))


###################################################
### code chunk number 96: 11validate.Rnw:2479-2480
###################################################
rm(murlev1x,murinf1x,murdfb1x)


###################################################
### code chunk number 97: 11validate.Rnw:2499-2501
###################################################
DL <- as.im(dfault)
subW <- (DL <= bdist.pixels(Window(DL)))


###################################################
### code chunk number 98: chorleyload
###################################################
if(draftversion) {
  load(datafilepath("chorleyCoarse.rda"))
  draftmessage <- 
    "using coarse quadrature scheme, eps=0.5, in draft version"
} else {
  load(datafilepath("chorley.rda"))
  draftmessage <- NULL
}


###################################################
### code chunk number 99: chorleyDfitAgain
###################################################
d2incin <- function(x, y, xincin=354.5, yincin=413.6) {
  (x - xincin)^2 + (y - yincin)^2
}
raisin <- function(x,y, alpha, beta) {
  1 + alpha * exp( - beta * d2incin(x,y))
}
chorleyDfit <- ippm(Q ~offset(log(smo) + log(raisin)), 
                    start=list(alpha=5, beta=1))


###################################################
### code chunk number 100: morescore
###################################################
Zalpha <- function(x,y, alpha, beta) {
  expbit <- exp( - beta * d2incin(x,y))
  expbit/(1 + alpha * expbit)
}
Zbeta <- function(x,y, alpha, beta) {
  d2 <- d2incin(x,y)
  topbit <- alpha * exp( - beta * d2)
  - d2 * topbit/(1 + topbit)
}
Zscore <- list(alpha=Zalpha, beta=Zbeta)


###################################################
### code chunk number 101: moreHessian
###################################################
Zaa <- function(x,y, alpha, beta) {
  expbit <- exp( - beta * d2incin(x,y))
  -(expbit/(1 + alpha * expbit))^2
}
Zab <- function(x,y, alpha, beta) {
  d2 <- d2incin(x,y)
  expbit <- exp( - beta * d2)
  - d2 * expbit/(1 + alpha * expbit)^2
}
Zbb <- function(x,y, alpha, beta) {
  d2 <- d2incin(x,y)
  topbit <- alpha * exp( - beta * d2)
  (d2^2) * topbit/(1 + topbit)^2
}
Zhess <- list(aa=Zaa, ab=Zab, ba=Zab, bb=Zbb)


###################################################
### code chunk number 102: chorleyDXcalc
###################################################
reload.or.compute(datafilepath("chorleydigglev.rda"), {
  chorleyDXlev <- leverage(chorleyDfit, iScore=Zscore, iHessian=Zhess)
  chorleyDXinf <- influence(chorleyDfit, iScore=Zscore, iHessian=Zhess)
  chorleyDXdfb <- dfbetas(chorleyDfit, iScore=Zscore, iHessian=Zhess)
  chorleyDXdfbsmo <- Smooth(chorleyDXdfb, sigma=1.5)
})


###################################################
### code chunk number 103: 11validate.Rnw:2608-2615 (eval = FALSE)
###################################################
## chorleyDXlev <- leverage(chorleyDfit, 
##                          iScore=Zscore, iHessian=Zhess)
## chorleyDXinf <- influence(chorleyDfit, 
##                           iScore=Zscore, iHessian=Zhess)
## chorleyDXdfb <- dfbetas(chorleyDfit, 
##                         iScore=Zscore, iHessian=Zhess)
## chorleyDXdfbsmo <- Smooth(chorleyDXdfb, sigma=1.5)


###################################################
### code chunk number 104: 11validate.Rnw:2620-2622
###################################################
logbexpr <- expression(log(1 + 
    alpha * exp( - beta * ((x - 354.5)^2 + (y-413.6)^2))))


###################################################
### code chunk number 105: 11validate.Rnw:2625-2627
###################################################
logB <- deriv(logbexpr, c("alpha", "beta"), 
              c("x", "y", "alpha", "beta"), hessian=TRUE)


###################################################
### code chunk number 106: 11validate.Rnw:2642-2644
###################################################
chorleyDfitI <- ippm(Q ~offset(log(smo) + logB),
                     start=list(alpha=10, beta=1))


###################################################
### code chunk number 107: 11validate.Rnw:2646-2647
###################################################
chorleyDfitI


###################################################
### code chunk number 108: 11validate.Rnw:2649-2652 (eval = FALSE)
###################################################
## chorleyDXlev <- leverage(chorleyDfitI)
## chorleyDXinf <- influence(chorleyDfitI)
## chorleyDXdfb <- dfbetas(chorleyDfitI)


###################################################
### code chunk number 109: 11validate.Rnw:2682-2684
###################################################
## Box surrounding incinerator and suspect cases:
ibox <- owin(c(352.2, 358.0), c(411.5, 416.0))


###################################################
### code chunk number 110: 11validate.Rnw:2686-2687
###################################################
spatstat.options(image.colfun=greytoblack)


###################################################
### code chunk number 111: ChorleyR.Rnw:3-5
###################################################
newplot(9, 0.65)
setmargins(0, 0, 0, 2)


###################################################
### code chunk number 112: chorleyDXlev
###################################################
plot(chorleyDXlev, main="", box=FALSE)
plot(Window(chorley), add=TRUE)


###################################################
### code chunk number 113: ChorleyZoomR.Rnw:3-5
###################################################
newplot(8, 0.5)
setmargins(0,0,0,2)


###################################################
### code chunk number 114: chorleyDXlevZ
###################################################
plot(chorleyDXlev[ibox], main="", box=FALSE)
plot(edges(Window(chorley))[ibox], add=TRUE, lwd=2)
points(chorley.extra[["incin"]], pch=10, cex=2, lwd=2)


###################################################
### code chunk number 115: ChorleyL.Rnw:3-5
###################################################
newplot(9,0.65)
setmargins(0, 2, 0, 0)


###################################################
### code chunk number 116: chorleyDXinf
###################################################
sy <- plot(chorleyDXinf, main="", maxsize=0.7, lwd=3)


###################################################
### code chunk number 117: ChorleyZoomL.Rnw:3-5
###################################################
newplot(8, 0.5)
setmargins(0,2,0,0)


###################################################
### code chunk number 118: chorleyDXinfZ
###################################################
plot(chorleyDXinf[ibox], main="", box=FALSE, symap=sy)
plot(edges(as.owin(chorley))[ibox], add=TRUE, lwd=2)
points(chorley.extra[["incin"]], pch=10, cex=1.2)


###################################################
### code chunk number 119: 11validate.Rnw:2739-2740
###################################################
spatstat.options(image.colfun=darktolight)


###################################################
### code chunk number 120: ChorleyZoom2LR.Rnw:4-6
###################################################
newplot(19, 1)
setmargins(0,2,0,2)


###################################################
### code chunk number 121: 11validate.Rnw:2744-2745
###################################################
setmargins(0,1,0,1)


###################################################
### code chunk number 122: chorleyDXdfbZ
###################################################
plot(chorleyDXdfb[ibox, 2:3], 
     main="", main.panel="", mar.panel=0, hsep=3,
     cols="white", maxsize=1.75,
     leg.args=list(col="black"), box=FALSE, etch=TRUE, lwd=2,
     panel.end=edges(as.owin(chorley))[ibox])


