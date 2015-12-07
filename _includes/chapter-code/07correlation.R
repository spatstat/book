### R code from vignette source '07correlation.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 07correlation.Rnw:9-12
###################################################
source("R/startup.R")
spatstat.options(terse=2)
requireversion(spatstat, "1.41-1.021")


###################################################
### code chunk number 2: 07correlation.Rnw:43-48
###################################################
set.seed(12345)
inde <- rpoispp(100)
regu <- rSSI(0.09, 70)
clus <- rMatClust(30, 0.05, 4)
save(inde, regu, clus, file=datafilepath("trichotomy.rda"))


###################################################
### code chunk number 3: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 4: 07correlation.Rnw:54-57
###################################################
plot(solist(regu,inde,clus),
     main="", main.panel="", equal.scales=TRUE,
     mar.panel=0, hsep=0.3, pch=16)


###################################################
### code chunk number 5: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 6: 07correlation.Rnw:165-172
###################################################
par(mfrow=c(1,3))
xl <- c(0.03, 0.71)
yl <- c(1/7, 7)
miplot(regu, main="", xlim=xl, ylim=yl, log="xy", pch=3)
miplot(inde, main="", xlim=xl, ylim=yl, log="xy", pch=3)
miplot(clus, main="", xlim=xl, ylim=yl, log="xy", pch=3)
par(mfrow=c(1,1))


###################################################
### code chunk number 7: 07correlation.Rnw:268-292
###################################################
## Fry plot example
X <- cells[shift(square(0.4), c(0.6,0.6))]
W <- as.owin(X)
B0 <- square(c(-1,1)*0.3)
B <- shift(B0, X[6])
A <- boundingbox(B, W)
FB0 <- frypoints(X)[B0]
FB <- shift(FB0, X[6])
## set up a 'layered' object containing all steps in making the Fry plot.
## Then use [ ] to select intermediate stages
steps <- layered(blankspace=A, 
                 printedpage=W, 
                 datapoints=X, 
                 transparency=B, 
                 centre=X[6], 
                 copying=X[-6][B],
                 full=FB)
layerplotargs(steps) <- list(list(type="n"),
                             list(col="darkgrey"), # grey background
                             list(cols="white", pch=16), # white dots
                             list(),
                             list(pch=3, cex=1.5, lwd=2), # crosshairs
                             list(pch=1, lwd=4), # large open circles
                             list(pch=1, lwd=2, cex=0.85)) # open circles


###################################################
### code chunk number 8: 07correlation.Rnw:296-297
###################################################
zeromargins()


###################################################
### code chunk number 9: 07correlation.Rnw:301-304
###################################################
plot(solist(steps[1:3], steps[1:5], steps[1:6], steps[c(1,4,5,7)]),
     main="", main.panel=paren(letters[1:4]), equal.scales=TRUE,
     mar.panel=0.1, hsep=1, nrows=1, font.main=3, cex.main=0.7)


###################################################
### code chunk number 10: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 11: 07correlation.Rnw:325-334
###################################################
par(mfrow=c(1,3))
w <- 0.5
fryplot(regu, main="", width=w, cex=0.5)
points(0,0,pch=3,cex=2)
fryplot(inde, main="", width=w, cex=0.5)
points(0,0,pch=3,cex=2)
fryplot(clus, main="", width=w, cex=0.5)
points(0,0,pch=3,cex=2, col="white")
par(mfrow=c(1,1))


###################################################
### code chunk number 12: 07correlation.Rnw:491-502
###################################################
set.seed(99999)
XX <- runifpoint(30)
RR <- 0.2
showT <- function(i, X=XX, r=RR, adj=c(-1,-1)) {
  D <- disc(radius=r, centre=X[i])
  plot(D, add=TRUE, lty=2)
  tiX <- sum(pairdist(X)[i,] <= r) - 1
  adj <- adj * r/4
  text(X$x[i] + adj[1], X$y[i] + adj[2], tiX)
  invisible(NULL)
}


###################################################
### code chunk number 13: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 14: 07correlation.Rnw:507-508
###################################################
zeromargins()


###################################################
### code chunk number 15: 07correlation.Rnw:512-520
###################################################
plot(grow.rectangle(Window(XX), RR), type="n", main="")
plot(XX, add=TRUE, show.all=TRUE, main="", pch=16)
showT(11)
showT(7)
showT(8)
showT(5)
showT(3, adj=c(-1.5,0))
showT(16, adj=c(-1.5,0))


###################################################
### code chunk number 16: 07correlation.Rnw:591-597
###################################################
Kregu <- Kest(regu, correction="iso")
Kinde <- Kest(inde, correction="iso")
Kclus <- Kest(clus, correction="iso")
Lregu <- Lest(regu, correction="iso")
Linde <- Lest(inde, correction="iso")
Lclus <- Lest(clus, correction="iso")


###################################################
### code chunk number 17: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 18: 07correlation.Rnw:601-602
###################################################
setmargins(0.1 + c(3,4,0,0))


###################################################
### code chunk number 19: 07correlation.Rnw:606-614
###################################################
yr <- range(Kclus, Kinde, Kregu)
plot(Kregu, iso ~ r, lty=1, main="", legend=FALSE, ylim=yr, 
     ylab=expression(italic(hat(K)(r))))
plot(Kinde, iso ~ r, lty=3, add=TRUE)
plot(Kclus, iso ~ r, lty=2, add=TRUE)
legend("topleft", bty="n",
       lty=c(2,3,1), lwd=2,
       legend=c("clustered", "independent", "regular"))


###################################################
### code chunk number 20: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 21: 07correlation.Rnw:751-752
###################################################
setmargins(4,4,1,1)


###################################################
### code chunk number 22: 07correlation.Rnw:756-761
###################################################
par(mfrow=c(1,3))
plot(Kregu, main="", col=1, legend=FALSE)
plot(Kinde, main="", col=1, legend=FALSE)
plot(Kclus, main="", col=1, legend=FALSE)
par(mfrow=c(1,1))


###################################################
### code chunk number 23: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 24: 07correlation.Rnw:829-830
###################################################
setmargins(4,4,1,1)


###################################################
### code chunk number 25: 07correlation.Rnw:834-839
###################################################
par(mfrow=c(1,3))
plot(Lregu, main="", col=1, legend=FALSE)
plot(Linde, main="", col=1, legend=FALSE)
plot(Lclus, main="", col=1, legend=FALSE)
par(mfrow=c(1,1))


###################################################
### code chunk number 26: 07correlation.Rnw:909-912
###################################################
set.seed(1492)
KCI <- lohboot(clus, "Kest")
KE <- envelope(clus, nsim=39, verbose=FALSE)


###################################################
### code chunk number 27: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 28: 07correlation.Rnw:918-921
###################################################
par(mfrow=c(1,2))
plot(KCI, legend=FALSE, main="")
plot(KE,  legend=FALSE, main="")


###################################################
### code chunk number 29: 07correlation.Rnw:994-996
###################################################
K <- Kest(cells)
Ki <- Kest(cells, correction="isotropic")


###################################################
### code chunk number 30: 07correlation.Rnw:1036-1037
###################################################
Lc <- Lest(cells)


###################################################
### code chunk number 31: 07correlation.Rnw:1084-1086
###################################################
Xfool <- rpoispp(function(x,y) 500 * exp(-3*x - 2*y))
Kfool <- Kest(Xfool, correction="iso")


###################################################
### code chunk number 32: UnitFvLeft.Rnw:4-5
###################################################
zeromargins()


###################################################
### code chunk number 33: 07correlation.Rnw:1093-1094
###################################################
plot(Xfool, main="", pch=16)


###################################################
### code chunk number 34: UnitFvRight.Rnw:6-7
###################################################
setmargins(3, 3.5, 0.25, 0.1)


###################################################
### code chunk number 35: 07correlation.Rnw:1097-1099
###################################################
par(pty="s")
plot(Kfool, main="")


###################################################
### code chunk number 36: 07correlation.Rnw:1165-1166
###################################################
set.seed(19191)


###################################################
### code chunk number 37: 07correlation.Rnw:1168-1169
###################################################
Xcell <- rcell(nx=15)


###################################################
### code chunk number 38: UnitFvLeft.Rnw:4-5
###################################################
zeromargins()


###################################################
### code chunk number 39: 07correlation.Rnw:1184-1185
###################################################
plot(Xcell, main="", pch=16, cex=0.8)


###################################################
### code chunk number 40: UnitFvRight.Rnw:6-7
###################################################
setmargins(3, 3.5, 0.25, 0.1)


###################################################
### code chunk number 41: 07correlation.Rnw:1188-1189
###################################################
plot(Kest(Xcell), cbind(iso, theo) ~ r, main="", cex=0.85)


###################################################
### code chunk number 42: 07correlation.Rnw:1323-1346
###################################################
## synthetic example 
set.seed(20142014)
W <- square(1)
Wplus <- grow.rectangle(W, 1, 0.5)
P <- ppp(0.9, 0.85, window=W)
Xplus <- rSSI(0.07, 150, win=Wplus, x.init=P[Wplus])[-1]
R <- 0.4
DD <- disc(R, P)
inDD <- inside.owin(Xplus, , DD)
tir <- sum(inDD)
FieldDemo <- layered(Wplus, Xplus[!inDD], Xplus[inDD], P, P, W, DD)
layerplotargs(FieldDemo) <- list(list(type="n"),
                                 list(pch=1),
                                 list(pch=16),
                                 list(pch=1),
                                 list(pch=3, cex=1.2),
                                 list(lwd=2),
                                 list(lty=2, lwd=2))
BB <- boundingbox(W, DD)
X <- Xplus[W]
inDW <- inside.owin(X, , DD)
EdgeDemo <- layered(BB, X[!inDW], X[inDW], P, P, W, DD)
layerplotargs(EdgeDemo) <- layerplotargs(FieldDemo)


###################################################
### code chunk number 43: 07correlation.Rnw:1350-1351
###################################################
zeromargins()


###################################################
### code chunk number 44: 07correlation.Rnw:1355-1356
###################################################
plot(FieldDemo, main="")


###################################################
### code chunk number 45: 07correlation.Rnw:1409-1410
###################################################
zeromargins()


###################################################
### code chunk number 46: 07correlation.Rnw:1414-1415
###################################################
plot(EdgeDemo, main="")


###################################################
### code chunk number 47: 07correlation.Rnw:1429-1489
###################################################
## set up example data for edge corrections
## smoothed version of Australia
W <- closing(Window(austates), 2.25)
## points to be considered
P <- ppp(c(135.0, 146.7), c(-27.3, -22.4), window=W)
R <- pairdist(P)[1,2] # about 13
## fake data including these points
set.seed(1919191)
XX <- rSSI(1, 50, W, x.init=P)
## edge effect
D <- disc(radius=R, centre=P[1])
Dnew <- setminus.owin(D, union.owin(W, dilation(XX, 1)))
Xmissed <- rSSI(1, 8, Dnew)
Bplus <- grow.rectangle(boundingbox(W), 4)
Xplus <- rSSI(1, 160, Bplus, x.init=superimpose(XX, Xmissed, W=Bplus))
OtherEdgeDemo <- layered(Bplus, Xplus, W, D, P[1])
PCEX <- 1.2
layerplotargs(OtherEdgeDemo) <- list(list(type="n"),
                                  list(cex=PCEX),
                                  list(lwd=2),
                                  list(lty=2),
                                  list(pch=16, cex=PCEX))
## border correction
Rsmall <- 3
Wminus <- erosion(W, Rsmall)
Y <- XX[bdist.points(XX) >= Rsmall]
bdY <- bdist.points(Y)
Imarginal <- which.min(ifelse(bdY > 1.2 * Rsmall, bdY, Inf))
Pmarginal <- Y[Imarginal]
Dmarginal <- disc(radius=Rsmall, centre=Pmarginal)
BordDemo <- layered(W, Wminus, XX, Pmarginal, Dmarginal)
layerplotargs(BordDemo) <- list(list(),
                             list(col=grey(0.5), border=NA),
                             list(pch=1, cex=PCEX),
                             list(pch=16, cex=PCEX),
                             list(lty=2, lwd=2))
## isotropic correction
D <- disc(radius=R, centre=P[1])
Din <- edges(D)[W]
BBI <- boundingbox(W, D)
RipleyDemo <- layered(BBI, W, XX, D, Din, P)
layerplotargs(RipleyDemo) <- list(list(type="n"),
                                  list(lwd=2),
                                  list(cex=PCEX),
                                  list(lty=2),
                                  list(lwd=7, col=grey(0.4)),
                                  list(pch=16, cex=PCEX))
## translation correction
Arr <- onearrow(P)
v <- unlist(lapply(coords(Arr), diff))
Ws <- shift(W, -v)
WsW <- intersect.owin(W, Ws)
BBT <- boundingbox(W, Ws)
TransDemo <- layered(BBT, WsW, Ws, W, XX, Arr)
layerplotargs(TransDemo) <- list(list(type="n"),
                                 list(col="grey"),
                                 list(lty=2),
                                 list(lwd=2),
                                 list(cex=PCEX),
                                 list(do.points=TRUE, pch=16, cex=PCEX))


###################################################
### code chunk number 48: 07correlation.Rnw:1511-1513
###################################################
set.seed(1918)
unK <- Kest(runifpoint(100), correction="none")


###################################################
### code chunk number 49: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 50: 07correlation.Rnw:1517-1518
###################################################
setmargins(3, 3.5, 0.25, 0.1)


###################################################
### code chunk number 51: 07correlation.Rnw:1522-1523
###################################################
plot(unK, main="", legend=FALSE)


###################################################
### code chunk number 52: 07correlation.Rnw:1547-1548
###################################################
zeromargins() 


###################################################
### code chunk number 53: 07correlation.Rnw:1552-1553
###################################################
plot(BordDemo, main="")


###################################################
### code chunk number 54: 07correlation.Rnw:1655-1658
###################################################
set.seed(999)
X20 <- runifpoint(20)
K20 <- Kest(X20)


###################################################
### code chunk number 55: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 56: 07correlation.Rnw:1662-1663
###################################################
setmargins(3, 3.5, 0.25, 0.1)


###################################################
### code chunk number 57: 07correlation.Rnw:1667-1668
###################################################
plot(K20, cbind(border, theo) ~ r, main="", legend=FALSE)


###################################################
### code chunk number 58: 07correlation.Rnw:1735-1736
###################################################
zeromargins()


###################################################
### code chunk number 59: 07correlation.Rnw:1740-1741
###################################################
plot(RipleyDemo, main="")


###################################################
### code chunk number 60: 07correlation.Rnw:1891-1892
###################################################
zeromargins()


###################################################
### code chunk number 61: 07correlation.Rnw:1896-1897
###################################################
plot(TransDemo, main="")


###################################################
### code chunk number 62: 07correlation.Rnw:2032-2034
###################################################
KC <- Kest(cells)
KC


###################################################
### code chunk number 63: fv2title.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,1,1))


###################################################
### code chunk number 64: 07correlation.Rnw:2080-2085
###################################################
par(mfrow=c(1,2), pty="s", cex=0.8)
aa <- plot(Kest(swedishpines), legendargs=list(bty="o"))
if(monochrome) aa[,"col"] <- 1
aa <- subset(aa, select=-key)
plot(Kest(swedishpines), iso ~ r, main="")


###################################################
### code chunk number 65: 07correlation.Rnw:2100-2101
###################################################
aa


###################################################
### code chunk number 66: 07correlation.Rnw:2137-2139 (eval = FALSE)
###################################################
## Ks <- Kest(swedishpines)
## plot(Ks,  iso ~ r)


###################################################
### code chunk number 67: 07correlation.Rnw:2143-2144
###################################################
Ks <- Kest(swedishpines)


###################################################
### code chunk number 68: 07correlation.Rnw:2207-2208
###################################################
K <- Kest(swedishpines)


###################################################
### code chunk number 69: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 70: 07correlation.Rnw:2213-2214
###################################################
setmargins(3, 4, 0.1, 0.1)


###################################################
### code chunk number 71: 07correlation.Rnw:2218-2222
###################################################
par(mfrow=c(1,2),pty="s")
plot(K, ./theo ~ r, main="", legendargs=list(cex=0.7,bty="o"))
plot(K, . ~ theo, main="", legendargs=list(cex=0.7, bty="o"))
par(mfrow=c(1,1))


###################################################
### code chunk number 72: 07correlation.Rnw:2238-2240 (eval = FALSE)
###################################################
## lambda <- intensity(swedishpines)
## plot(Ks, lambda * . ~ r)


###################################################
### code chunk number 73: 07correlation.Rnw:2322-2323
###################################################
Ko <- subset(Ks, r < 0.1, select= -border)


###################################################
### code chunk number 74: 07correlation.Rnw:2332-2335
###################################################
Ks <- Kest(swedishpines)
K <- as.function(Ks)
K(9)


###################################################
### code chunk number 75: 07correlation.Rnw:2345-2348
###################################################
K <- as.function(Ks, value=".")
K(9)
K(9, "trans")


###################################################
### code chunk number 76: 07correlation.Rnw:2358-2361
###################################################
Kr <- Kest(redwood)
y <- with(Kr, iso - theo)
x <- with(Kr, r)


###################################################
### code chunk number 77: 07correlation.Rnw:2385-2386
###################################################
with(Kr, max(abs(iso-theo)))


###################################################
### code chunk number 78: 07correlation.Rnw:2400-2402
###################################################
K1 <- Kest(redwood) ; K2 <- Kest(cells)
DK <- eval.fv(K1-K2)


###################################################
### code chunk number 79: 07correlation.Rnw:2458-2470
###################################################
X <- swedishpines
W <- Window(X)
D2 <- disc(radius=30, centre=X[24])
D1 <- disc(radius=28, centre=X[24])
OO <- setminus.owin(D2, D1)
# disc
Pic1 <- layered(W, D1, X, X[24])
# thin ring
Pic2 <- layered(W, OO, X, X[24])
layerplotargs(Pic1) <- 
    layerplotargs(Pic2) <- 
    list(list(), list(col=grey(0.55)), list(pch=16), list(pch=3, cex=2))


###################################################
### code chunk number 80: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 81: 07correlation.Rnw:2476-2478
###################################################
plot(solist(Pic1, Pic2), main="", main.panel="",
     equal.scales=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 82: 07correlation.Rnw:2522-2529
###################################################
M <- 20
ff <- function(i, j) owin(c(i,i+1)/M, c(j, j+1)/M)
P1 <- ff(0,0)
P2 <- ff(19, 7)
X1 <- runifpoint(1, P1)
X2 <- runifpoint(1, P2)
BB <- boundingbox(P1, P2)


###################################################
### code chunk number 83: 07correlation.Rnw:2540-2541
###################################################
zeromargins()


###################################################
### code chunk number 84: 07correlation.Rnw:2545-2552
###################################################
plot(BB, type="n", main="")
plot(P1, add=TRUE, col="grey")
plot(P2, add=TRUE, col="grey")
plot(X1, add=TRUE, pch=16, cex=0.7)
plot(X2, add=TRUE, pch=16, cex=0.7)
arrows(1/20, 1/20, 19/20, 7/20, angle=10, lwd=2, code=3)
text(10/20, 4/20, "r", pos=3, srt=atan(0.4) * 180/pi, cex=1, font=3)


###################################################
### code chunk number 85: 07correlation.Rnw:2608-2611
###################################################
gregu <- pcf(regu, correction="iso", divisor="d")
ginde <- pcf(inde, correction="iso", divisor="d")
gclus <- pcf(clus, correction="iso", divisor="d")


###################################################
### code chunk number 86: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 87: 07correlation.Rnw:2615-2616
###################################################
setmargins(4,4,1,1)


###################################################
### code chunk number 88: 07correlation.Rnw:2620-2625
###################################################
par(mfrow=c(1,3))
plot(gregu, main="", legend=FALSE, ylab=expression(g(r)), ylim.covers=c(0,2))
plot(ginde, main="", legend=FALSE, ylab=expression(g(r)), ylim.covers=c(0,2))
plot(gclus, main="", legend=FALSE, ylab=expression(g(r)), ylim.covers=c(0,2))
par(mfrow=c(1,1))


###################################################
### code chunk number 89: 07correlation.Rnw:2766-2768 (eval = FALSE)
###################################################
## g <- pcf(cells)
## plot(g)


###################################################
### code chunk number 90: 07correlation.Rnw:2808-2811
###################################################
P <- copper$Points
g1 <- pcf(P, correction="isotropic")
g2 <- pcf(P, correction="isotropic", divisor="d")


###################################################
### code chunk number 91: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 92: 07correlation.Rnw:2817-2820
###################################################
par(mfrow=c(1,2))
plot(g1, main="", ylim=c(0, 10), lwd=2, legend=FALSE)
plot(g2, main="", ylim=c(0, 10), lwd=2, legend=FALSE)


###################################################
### code chunk number 93: 07correlation.Rnw:2842-2844 (eval = FALSE)
###################################################
## K <- Kest(shapley)
## g <- pcf(K, spar=0.5)


###################################################
### code chunk number 94: 07correlation.Rnw:2942-2945 (eval = FALSE)
###################################################
## swp <- rescale(swedishpines)
## Kvb <- varblock(swp, Kest, nx=3, ny=3) 
## plot(Kvb)


###################################################
### code chunk number 95: 07correlation.Rnw:3007-3009 (eval = FALSE)
###################################################
## Kloh <- lohboot(swp, Kest)
## plot(Kloh)


###################################################
### code chunk number 96: 07correlation.Rnw:3016-3023
###################################################
swp <- rescale(swedishpines)
Kvb <- varblock(swp, Kest, nx=3, ny=3)
Kloh <- lohboot(swp, Kest)
xyV <- plot(Kvb, limitsonly=TRUE)
xlim <- xyV$xlim
xyL <- plot(Kloh, limitsonly=TRUE, xlim=xlim)
ylim <- range(xyV$ylim, xyL$ylim)


###################################################
### code chunk number 97: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 98: 07correlation.Rnw:3029-3033
###################################################
par(mfrow=c(1,2))
plot(Kvb, main="", legend=FALSE, xlim=xlim, ylim=ylim,lty=c(1,2,1,1))
plot(Kloh, main="", legend=FALSE, xlim=xlim, ylim=ylim)
par(mfrow=c(1,1))


###################################################
### code chunk number 99: 07correlation.Rnw:3073-3074 (eval = FALSE)
###################################################
## Lg <- lohboot(swp, Lest, global=TRUE)


###################################################
### code chunk number 100: 07correlation.Rnw:3078-3079 (eval = FALSE)
###################################################
## Kg <- eval.fv(pi * Lg^2)


###################################################
### code chunk number 101: 07correlation.Rnw:3083-3085
###################################################
Lg <- lohboot(swp, "Lest", global=TRUE)
Kg <- eval.fv(pi * Lg^2)


###################################################
### code chunk number 102: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 103: 07correlation.Rnw:3090-3095
###################################################
par(mfrow=c(1,2))
plot(Lg, cbind(iso,theo) ~ r, shade=c("lo", "hi"), main="", legend=FALSE)
plot(Kg, cbind(iso,theo) ~ r, shade=c("lo", "hi"), main="", legend=FALSE,
     ylab=expression(K(r)))
par(mfrow=c(1,1))


###################################################
### code chunk number 104: 07correlation.Rnw:3155-3159
###################################################
set.seed(11111918)
KE <- envelope(swp, Kest, nsim=39, savefuns=TRUE)
Xlim <- c(0, 2)
Ylim <- plot(KE, xlim=Xlim, limitsonly=TRUE)$ylim


###################################################
### code chunk number 105: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 106: 07correlation.Rnw:3164-3170
###################################################
OP <- par(mfrow=c(1,2),pty="s")
plot(attr(KE, "simfuns"), 
     lty=1, lwd=0.5, main="", xlim=Xlim, ylim=Ylim, legend=FALSE)
plot(KE, add=TRUE, obs ~ r, lwd=4, shade=NULL)
plot(KE, obs ~ r, main="", lwd=4, xlim=Xlim, ylim=Ylim, legend=FALSE)
par(OP)


###################################################
### code chunk number 107: 07correlation.Rnw:3229-3230
###################################################
E <- envelope(swp, Kest, nsim=39)


###################################################
### code chunk number 108: 07correlation.Rnw:3236-3238 (eval = FALSE)
###################################################
## E <- envelope(swp, Kest, nsim=39, fix.n=TRUE)
## plot(E)


###################################################
### code chunk number 109: 07correlation.Rnw:3246-3247
###################################################
E


###################################################
### code chunk number 110: 07correlation.Rnw:3310-3314
###################################################
set.seed(4224)
E <- envelope(swp, Kest, nsim=19, rank=1, global=TRUE, 
              savepatterns=TRUE)
EL <- envelope(E, Lest, global=TRUE)


###################################################
### code chunk number 111: 07correlation.Rnw:3318-3319 (eval = FALSE)
###################################################
## E <- envelope(swp, Kest, nsim=19, rank=1, global=TRUE)


###################################################
### code chunk number 112: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 113: 07correlation.Rnw:3331-3335
###################################################
OP <- par(mfrow=c(1,2),pty="s")
plot(E, main="", legend=FALSE)
plot(EL, main="", legend=FALSE)
par(OP)


###################################################
### code chunk number 114: 07correlation.Rnw:3361-3364
###################################################
set.seed(10981)
madswede <- mad.test(swp, Lest, nsim=99, rmax=2, use.theo=TRUE)
dclfswede <- dclf.test(swp, Lest, nsim=99, rmax=2, use.theo=TRUE)


###################################################
### code chunk number 115: 07correlation.Rnw:3366-3367 (eval = FALSE)
###################################################
## mad.test(swp, Lest, nsim=99, rmax=2, use.theo=TRUE)


###################################################
### code chunk number 116: 07correlation.Rnw:3369-3370
###################################################
madswede


###################################################
### code chunk number 117: 07correlation.Rnw:3383-3384 (eval = FALSE)
###################################################
## dclf.test(swp, Lest, nsim=99, rmax=2, use.theo=TRUE)$p.value


###################################################
### code chunk number 118: 07correlation.Rnw:3386-3387
###################################################
dclfswede$p.value


###################################################
### code chunk number 119: 07correlation.Rnw:3406-3407
###################################################
set.seed(19221101)


###################################################
### code chunk number 120: 07correlation.Rnw:3413-3415
###################################################
X <- rSSI(0.05, win=owin(c(0,1), c(0, 3)))
Y <- affine(X, mat=diag(c(1, 1/3)))


###################################################
### code chunk number 121: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 122: 07correlation.Rnw:3424-3425
###################################################
zeromargins()


###################################################
### code chunk number 123: 07correlation.Rnw:3429-3433
###################################################
OP <- par(mfrow=c(1,2))
plot(Y, pch=3, main="")
fryplot(Y, width=0.2, cex=0.3, main="")
par(OP)


###################################################
### code chunk number 124: 07correlation.Rnw:3454-3479
###################################################
X <- swedishpines
W <- Window(X)
D2 <- disc(radius=30, centre=X[24])
D1 <- disc(radius=16, centre=X[24])
Tri <- convexhull.xy(35 * c(0, cos(pi/12), cos(pi/3)),
                     35 * c(0, sin(pi/12), sin(pi/3)))
Sec <- intersect.owin(D2, shift(Tri, X[24]))
## shaded sector
Pic3 <- layered(W, Sec, X, X[24])
layerplotargs(Pic3) <- 
    list(list(), list(col="grey"), list(pch=16), list(pch=3, cex=2))
## shaded ring section, arrow between points
Ar <- onearrow(X[c(24,34)])
BigRing <- setminus.owin(D2, D1)
Tri2 <- convexhull.xy(70 * c(0, 1, cos(2*pi/3)),
                      70 * c(0, 0, sin(2*pi/3)))
Section <- intersect.owin(BigRing, shift(Tri2, X[24]))
Pic4 <- layered(W, Section, X, X[24], Ar)
layerplotargs(Pic4) <- 
    list(list(), list(col="grey"), list(pch=16), list(pch=3, cex=2),
    list(do.points=TRUE, pch=16, cex=PCEX))
## CLIP THEM
halfW <- owin(c(0, 96), c(40, 100))
Pic3 <- applytolayers(Pic3, "[", i=halfW)
Pic4 <- applytolayers(Pic4, "[", i=halfW)


###################################################
### code chunk number 125: 07correlation.Rnw:3483-3484
###################################################
zeromargins()


###################################################
### code chunk number 126: 07correlation.Rnw:3489-3490
###################################################
plot(Pic3, main="")


###################################################
### code chunk number 127: 07correlation.Rnw:3520-3528
###################################################
Khoriz <- Ksector(Y, begin = -15, end = 15, units="degrees")
Kvert <- Ksector(Y,  begin = 90-15, end = 90+15, units="degrees")
dK <- function(X, ...) {
  K1 <- Ksector(X, ..., begin = -15, end = 15, units="degrees")
  K2 <- Ksector(X, ..., begin = 90-15, end = 90+15, units="degrees")
  eval.fv(1e4 * (K1-K2))
}
CIdK <- varblock(Y, dK, nx=5)


###################################################
### code chunk number 128: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 129: 07correlation.Rnw:3535-3541
###################################################
par(mfrow=c(1,2), pty="s")
plot(Khoriz, trans/theo ~ r, lty=2, main="", legend=FALSE, 
     ylab=expression(italic(K[sector](r))), ylim=c(0, 1.1))
plot(Kvert, trans/theo ~ r, add=TRUE)
plot(CIdK, trans ~ r, shade=c("hitrans", "lotrans"), legend=FALSE, 
     ylab="horizontal - vertical", main="", las=1)


###################################################
### code chunk number 130: 07correlation.Rnw:3559-3561 (eval = FALSE)
###################################################
## Khoriz <- Ksector(Y, begin = -15, end = 15, units="degrees")
## Kvert <- Ksector(Y,  begin = 90-15, end = 90+15, units="degrees")


###################################################
### code chunk number 131: 07correlation.Rnw:3569-3571 (eval = FALSE)
###################################################
## plot(Khoriz, trans/theo ~ r, lty=2)
## plot(Kvert, trans/theo ~ r, add=TRUE)


###################################################
### code chunk number 132: 07correlation.Rnw:3578-3584 (eval = FALSE)
###################################################
## dK <- function(X, ...) {
##   K1 <- Ksector(X, ..., begin = -15, end = 15, units="degrees")
##   K2 <- Ksector(X, ..., begin = 90-15, end = 90+15, units="degrees")
##   eval.fv(K1-K2)
## }
## CIdK <- varblock(Y, dK, nx=5)


###################################################
### code chunk number 133: 07correlation.Rnw:3601-3602
###################################################
zeromargins()


###################################################
### code chunk number 134: 07correlation.Rnw:3604-3605
###################################################
plot(Pic4, main="")


###################################################
### code chunk number 135: 07correlation.Rnw:3657-3658
###################################################
f <- pairorient(Y, r1=0.02, r2=0.05, sigma=5)


###################################################
### code chunk number 136: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 137: 07correlation.Rnw:3672-3673
###################################################
rose(f,  main="", col="grey")


###################################################
### code chunk number 138: 07correlation.Rnw:3703-3707
###################################################
ganiso <- Kmeasure(Y, sigma=0.02, eps=0.001)
giso <- rotmean(ganiso, result="im")
grel <- eval.im(ganiso/giso)
detail <- square(c(-0.1,0.1))


###################################################
### code chunk number 139: Unit2R.Rnw:3-5
###################################################
newplot(13, 0.9)
setmargins(0,0,0,4)


###################################################
### code chunk number 140: 07correlation.Rnw:3782-3783
###################################################
zeromargins()


###################################################
### code chunk number 141: 07correlation.Rnw:3787-3790
###################################################
plot(anylist(ganiso[detail], grel[detail]), 
     main="", main.panel="", 
     mar.panel=c(0,0,0,1), hsep=1, equal.scales=TRUE, box=TRUE)


###################################################
### code chunk number 142: 07correlation.Rnw:3843-3846 (eval = FALSE)
###################################################
## ganiso <- Kmeasure(Y, sigma=0.02, eps=0.001)
## detail <- square(c(-0.1,0.1))
## plot(ganiso[detail])


###################################################
### code chunk number 143: 07correlation.Rnw:3871-3874 (eval = FALSE)
###################################################
## giso <- rotmean(ganiso, result="im")
## grel <- ganiso/giso
## plot(grel[detail])


###################################################
### code chunk number 144: 07correlation.Rnw:3906-3907
###################################################
integral(ganiso, detail)


###################################################
### code chunk number 145: 07correlation.Rnw:4117-4119
###################################################
numata <- residualspaper$Fig1
lambda <- density(numata, bw.ppl)


###################################################
### code chunk number 146: Unit2r.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 147: 07correlation.Rnw:4125-4129
###################################################
plot(solist(layered(numata, plotargs=list(pch=16)), 
            lambda), 
     main="", main.panel="", equal.scales=TRUE, 
     mar.panel=0.2)


###################################################
### code chunk number 148: 07correlation.Rnw:4150-4152 (eval = FALSE)
###################################################
## numata <- residualspaper$Fig1
## lambda <- density(numata, bw.ppl)


###################################################
### code chunk number 149: 07correlation.Rnw:4157-4158
###################################################
numataK <- Kinhom(numata, lambda)


###################################################
### code chunk number 150: 07correlation.Rnw:4171-4172 (eval = FALSE)
###################################################
## numataK <- Kinhom(numata, sigma=bw.ppl)


###################################################
### code chunk number 151: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 152: 07correlation.Rnw:4181-4182
###################################################
plot(numataK, main="")


###################################################
### code chunk number 153: 07correlation.Rnw:4228-4229 (eval = FALSE)
###################################################
## g <- pcfinhom(bei)


###################################################
### code chunk number 154: 07correlation.Rnw:4236-4237 (eval = FALSE)
###################################################
## g <- pcf(Kinhom(bei))


###################################################
### code chunk number 155: 07correlation.Rnw:4254-4255
###################################################
set.seed(6828)


###################################################
### code chunk number 156: LinhomEnv
###################################################
lam <- density(numata, bw.ppl)
E <- envelope(numata, Linhom, sigma=bw.ppl, 
              simulate=expression(rpoispp(lam)),
              use.theory=TRUE, nsim=19, global=TRUE)


###################################################
### code chunk number 157: 07correlation.Rnw:4263-4264 (eval = FALSE)
###################################################
## plot(E, . - r ~ r)


###################################################
### code chunk number 158: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 159: 07correlation.Rnw:4273-4274
###################################################
plot(E, . - r ~ r, main="", legend=FALSE, ylim=c(-0.15,0.15))


###################################################
### code chunk number 160: 07correlation.Rnw:4371-4374
###################################################
X <- unmark(bronzefilter)
fit <- ppm(X ~ x)
lam <- predict(fit)


###################################################
### code chunk number 161: 07correlation.Rnw:4377-4378
###################################################
Kbro <- Kscaled(X, lam)


###################################################
### code chunk number 162: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 163: 07correlation.Rnw:4386-4387
###################################################
plot(Kbro, main="", legend=FALSE, xlim=c(0,2))


###################################################
### code chunk number 164: 07correlation.Rnw:4441-4442
###################################################
lK <- localK(swedishpines)


###################################################
### code chunk number 165: 07correlation.Rnw:4450-4452
###################################################
locK <- as.data.frame(lK)[, fvnames(lK, ".a")]
rr   <- with(lK, r)


###################################################
### code chunk number 166: 07correlation.Rnw:4460-4461
###################################################
locH <- hclust(dist(t(locK)))


###################################################
### code chunk number 167: 07correlation.Rnw:4511-4512
###################################################
cT <- Tstat(Xcell)


###################################################
### code chunk number 168: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 169: 07correlation.Rnw:4518-4519
###################################################
plot(cT, cbind(trans, theo) ~ r, main="", legend=FALSE)


