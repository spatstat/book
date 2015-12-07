### R code from vignette source '08distances.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 08distances.Rnw:9-13
###################################################
source("R/startup.R")
Digits <- 4
options(digits=Digits)
# library(envel) # superseded


###################################################
### code chunk number 2: examplediagrams
###################################################
## construct example diagrams for nearest distance
set.seed(104)
## parameters
R <- 0.25
Rplus <- 0.275
Rminus <- 0.2
W <- square(1)
## data point in question
XI <- ppp(0.5, 0.5, window=W)
## points outside circle
H <- disc(Rplus, XI)
OtherPoints <- runifpoint(15, setminus.owin(W, H))
Window(OtherPoints) <- W
X <- superimpose(XI, OtherPoints)
## nearest points
Nearest <- ppp(x=XI$x+R/sqrt(2), y=XI$y+R/sqrt(2), window=W)
#SecondNearest <- OtherPoints[nncross(XI, OtherPoints)$which]
## circles
ContactCircle <- disc(R, XI)
SmallerCircle <- disc(Rminus, XI)
## arrows
ArrowNearest <- onearrow(superimpose(XI, Nearest))
#ArrowSecondNearest <- onearrow(superimpose(XI, SecondNearest))
## construct figures for nearest neighbour
PCEX <- 1
nndistDef <- layered(W, XI, OtherPoints, 
                     Nearest, ContactCircle, ArrowNearest,
                     plotargs=list(list(),
                                   list(pch=16, cex=PCEX),
                                   list(pch=16, cex=PCEX),
                                   list(pch=16, cex=PCEX),
                                   list(lwd=1, lty=2),
                                   list(pch=16, cex=PCEX, col.head="grey", 
                                        do.points=FALSE)))
nndistExceeds <- nndistDef
nndistExceeds[[5]] <- SmallerCircle
layerplotargs(nndistExceeds)[[1]] <- list(type="n")
## figures for empty space
## Find a point that has a lot of empty space around it
## and doesn't have XI as its nearest neighbour
diri <- dirichlet(X)
til <- tiles(diri)
dis <- unlist(lapply(til, 
                    function(z, XX) { 
                        v <- as.ppp(vertices(z), Window(X))
                        dvX <- nncross(v, X, what="d")
                        b <- bdist.points(v)
                        max(0, dvX[dvX < b])
                    },
                    XX=X))
dis[1] <- 0
best <- which.max(dis)
besttile <- til[[best]]
v <- as.ppp(vertices(besttile), Window(X))
dvX <- nncross(v, X, what="d")
bv <- bdist.points(v)
vbest <- v[which.max(dvX[dvX < bv])]
alpha <- 0.8
U <- ppp(alpha * vbest$x + (1-alpha)* X$x[best],
         alpha * vbest$y + (1-alpha)* X$y[best],
         window=W)
## now construct diagram around U
RUX <- nncross(U, X, what="dist")
XJ <- X[nncross(U, X, what="which")]
UXContactCircle <- disc(RUX, U)
UXSmallerCircle <- disc(RUX * 0.75, U)
ArrowUX <- onearrow(superimpose(U, XJ))
emptyspaceDef <- layered(W, U, X,
                         UXContactCircle, ArrowUX,
                         plotargs=list(list(),
                                   list(pch=3, cex=PCEX),
                                   list(pch=16, cex=PCEX),
                                   list(lwd=1, lty=2),
                                   list(pch=16, cex=PCEX, 
                                        do.points=FALSE)))
Closeup <- owin(U$x + c(-1,1) * RUX * 2.5,
                U$y + c(-1,1) * RUX * 1.8)
Closeup <- grow.rectangle(boundingbox(X[Closeup]), RUX/10)
Closeup <- intersect.owin(W, Closeup)
ArrowUX <- onearrow(superimpose(U, XJ, W=Closeup))
emptyspaceExceeds <- layered(Closeup, U[Closeup], X[Closeup],
                         UXSmallerCircle, ArrowUX,
                         plotargs=list(list(type="n"),
                                   list(pch=3, cex=PCEX),
                                   list(pch=16, cex=PCEX),
                                   list(lwd=1),
                                   list(pch=16, cex=PCEX, col.head="grey",
                                        headfraction=0.35,
                                        do.points=FALSE)))


###################################################
### code chunk number 3: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 4: 08distances.Rnw:176-177
###################################################
zeromargins() ## This strips away ALL white space


###################################################
### code chunk number 5: 08distances.Rnw:181-184
###################################################
plot(solist(nndistDef[-5], emptyspaceDef[-4]), 
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=2)


###################################################
### code chunk number 6: 08distances.Rnw:195-197
###################################################
## temporary
options(digits=2)


###################################################
### code chunk number 7: 08distances.Rnw:205-207
###################################################
M <- pairdist(redwood)
M[1:3, 1:5]


###################################################
### code chunk number 8: 08distances.Rnw:212-214
###################################################
v <- nndist(redwood)
v[1:3]


###################################################
### code chunk number 9: 08distances.Rnw:222-223 (eval = FALSE)
###################################################
## Z <- distmap(redwood)


###################################################
### code chunk number 10: 08distances.Rnw:267-269
###################################################
distmapred <- distmap(redwood, dimyx=1024)
distfunred <- distfun(redwood)


###################################################
### code chunk number 11: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 12: 08distances.Rnw:275-280
###################################################
par(mfrow=c(1,2))
contour(distmapred, main="", drawlabels=FALSE, nlevels=25, lwd=2)
plot(redwood, add=TRUE, pch=16)
contour(distfunred, main="", drawlabels=FALSE, nlevels=25, lwd=2)
plot(redwood, add=TRUE, pch=16)


###################################################
### code chunk number 13: 08distances.Rnw:319-325
###################################################
U <- runifpoint(3, Window(redwood))
Z <- distmap(redwood)
Z[U]
nncross(U, redwood, what="dist")
f <- distfun(redwood)
f(U)


###################################################
### code chunk number 14: 08distances.Rnw:345-348
###################################################
nncross(U, redwood, k=2, what="dist")
f2 <- distfun(redwood, k=2)
f2(U)


###################################################
### code chunk number 15: 08distances.Rnw:379-380
###################################################
options(digits=Digits)


###################################################
### code chunk number 16: 08distances.Rnw:446-449
###################################################
clarkevans(redwood)
clarkevans.test(redwood, correction="donnelly", 
                alternative="clustered")


###################################################
### code chunk number 17: 08distances.Rnw:484-486
###################################################
hopskel(redwood)
hopskel.test(redwood, alternative="clustered")


###################################################
### code chunk number 18: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 19: 08distances.Rnw:516-517
###################################################
zeromargins() 


###################################################
### code chunk number 20: 08distances.Rnw:521-524
###################################################
opa <- par(cex=0.85)
redwoodfull.extra$plotit(main="")
par(opa)


###################################################
### code chunk number 21: 08distances.Rnw:559-560 (eval = FALSE)
###################################################
## stienen(redwoodfull)


###################################################
### code chunk number 22: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 23: 08distances.Rnw:569-570
###################################################
zeromargins()


###################################################
### code chunk number 24: 08distances.Rnw:574-578
###################################################
par(mfrow=c(1,2))
stienen(redwoodfull, main="", legend=FALSE, 
        border=list(bg=NULL, lwd=2, cols=grey(0.3)))
plot(dirichlet(redwoodfull), main="")


###################################################
### code chunk number 25: 08distances.Rnw:616-617 (eval = FALSE)
###################################################
## plot(dirichlet(redwoodfull))


###################################################
### code chunk number 26: 08distances.Rnw:763-764
###################################################
zeromargins()


###################################################
### code chunk number 27: 08distances.Rnw:768-769
###################################################
plot(emptyspaceExceeds, main="")


###################################################
### code chunk number 28: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 29: 08distances.Rnw:823-837
###################################################
curve(1 - exp(- pi * x^2), 
      from=0, to=1.5, 
      xlab=expression(italic(r)), 
      ylab=expression(italic(F(r))), main="", axes=FALSE)
xx <- (0:15)/10
yy <- (0:10)/10
segments(xx, 0, xx, 1, col=grey(0.6), lty=2)
segments(0, yy, 1.5, yy, col=grey(0.6), lty=2)
curve(1 - exp(- pi * x^2), 
      from=0, to=1.5, add=TRUE, lwd=2)
axis(2, pos=0)
axis(1, at=c(0,0.5,1,1.5), 
     labels=expression(0, 0.5/sqrt(lambda), 1/sqrt(lambda), 1.5/sqrt(lambda)),
     pos=0)


###################################################
### code chunk number 30: 08distances.Rnw:931-933
###################################################
Fs <- Fest(swedishpines)
Gs <- Gest(swedishpines)


###################################################
### code chunk number 31: 08distances.Rnw:957-958
###################################################
Fs


###################################################
### code chunk number 32: 08distances.Rnw:969-972 (eval = FALSE)
###################################################
## swp <- rescale(swedishpines)
## plot(Fest(swp))
## plot(Gest(swp))


###################################################
### code chunk number 33: fv2title.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,1,1))


###################################################
### code chunk number 34: 08distances.Rnw:983-988
###################################################
opa <- par(mfrow=c(1,2), cex=0.85)
swp <- rescale(swedishpines)
plot(Fest(swp))
plot(Gest(swp))
par(opa)


###################################################
### code chunk number 35: 08distances.Rnw:1012-1016
###################################################
load(datafilepath("trichotomy.rda"))
Fregu <- Fest(regu)
Finde <- Fest(inde)
Fclus <- Fest(clus)


###################################################
### code chunk number 36: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 37: 08distances.Rnw:1022-1033
###################################################
opa <- par(mfrow=c(1,3), pty="s", cex=0.91)
## note: par(mfrow) sets cex=0.83. We increase it by 10%
xl <- c(0, 0.12)
yl <- c(0, 1)
plot(Fregu, 
     cbind(km, theo) ~ r, main="", col=1, legend=FALSE, xlim=xl, ylim=yl)
plot(Finde, 
     cbind(km, theo) ~ r, main="", col=1, legend=FALSE, xlim=xl, ylim=yl)
plot(Fclus, 
     cbind(km, theo) ~ r, main="", col=1, legend=FALSE, xlim=xl, ylim=yl)
par(opa)


###################################################
### code chunk number 38: 08distances.Rnw:1063-1078
###################################################
## synthetic example for dilated set
set.seed(987654)
X <- runifpoint(50)
W <- Window(X)
Xr <- dilation(X, 0.08)
XrW <- intersect.owin(Xr, W)
bXrW <- edges(Xr)[W]
DilationExample <- layered(W, XrW, bXrW, X,
                           plotargs=list(list(type="n"),
                                         list(col="grey", border=NA),
                                         list(lwd=2),
                                         list(pch=16)))
## trim it
Wtrim <- owin(c(0,1), c(0, 2/3))
DilationExample <- DilationExample[, Wtrim]


###################################################
### code chunk number 39: 08distances.Rnw:1082-1083
###################################################
zeromargins()


###################################################
### code chunk number 40: 08distances.Rnw:1087-1088
###################################################
plot(DilationExample, main="")


###################################################
### code chunk number 41: 08distances.Rnw:1122-1125
###################################################
Gregu <- Gest(regu)
Ginde <- Gest(inde)
Gclus <- Gest(clus)


###################################################
### code chunk number 42: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 43: 08distances.Rnw:1131-1142
###################################################
par(mfrow=c(1,3), pty="s", cex=0.91)
## note: par(mfrow) sets cex=0.83. We increase it by 10%
xl <- c(0, 0.12)
yl <- c(0, 1)
plot(Gregu, 
     cbind(km, theo) ~ r, main="", col=1, legend=FALSE, xlim=xl, ylim=yl)
plot(Ginde,
     cbind(km, theo) ~ r, main="", col=1, legend=FALSE, xlim=xl, ylim=yl)
plot(Gclus,
     cbind(km, theo) ~ r, main="", col=1, legend=FALSE, xlim=xl, ylim=yl)
par(opa)


###################################################
### code chunk number 44: 08distances.Rnw:1244-1247
###################################################
swp <- rescale(swedishpines)
Fci <- varblock(swp, Fest, nx=5, correction="best")
Gci <- varblock(swp, Gest, nx=5, correction="best")


###################################################
### code chunk number 45: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 46: 08distances.Rnw:1253-1256
###################################################
par(mfrow=c(1,2))
plot(Fci, main="", legend=FALSE, xlim=c(0, 1))
plot(Gci, main="", legend=FALSE, xlim=c(0, 1))


###################################################
### code chunk number 47: 08distances.Rnw:1269-1271
###################################################
Fenv <- envelope(swp, Fest, nsim=39, fix.n=TRUE)
Genv <- envelope(swp, Gest, nsim=39, fix.n=TRUE)


###################################################
### code chunk number 48: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 49: 08distances.Rnw:1277-1280
###################################################
par(mfrow=c(1,2))
plot(Fenv, main="", legend=FALSE, xlim=c(0, 1))
plot(Genv, main="", legend=FALSE, xlim=c(0, 1))


###################################################
### code chunk number 50: 08distances.Rnw:1298-1299 (eval = FALSE)
###################################################
## plot(Fenv, hi - lo ~ r, shade=NULL, xlim=c(0, 2))


###################################################
### code chunk number 51: 08distances.Rnw:1339-1343
###################################################
Phi <- function(x) asin(sqrt(x))
Fglob <- envelope(swp, Fest, nsim=19, fix.n=TRUE,
                  global=TRUE, ginterval=c(0,1),
                  transform=expression(Phi(.)))


###################################################
### code chunk number 52: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 53: 08distances.Rnw:1354-1355
###################################################
plot(Fglob, main="", xlim=c(0, 1), legend=FALSE)


###################################################
### code chunk number 54: 08distances.Rnw:1423-1424 (eval = FALSE)
###################################################
## plot(Fest(swp), . ~ theo)


###################################################
### code chunk number 55: 08distances.Rnw:1438-1440
###################################################
FS <- Fest(swp)
QG <- QQversion(Gest(swp))


###################################################
### code chunk number 56: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 57: 08distances.Rnw:1446-1449
###################################################
par(mfrow=c(1,2))
plot(FS, . ~ theo, main="", legend=FALSE)
plot(QG, main="", legend=FALSE)


###################################################
### code chunk number 58: 08distances.Rnw:1471-1472 (eval = FALSE)
###################################################
## plot(QQversion(Gest(swp)))


###################################################
### code chunk number 59: 08distances.Rnw:1505-1509
###################################################
r0 <- sqrt(area(swp)/2)/npoints(swp)
Gglob <- envelope(swp, Gest, nsim=19, fix.n=TRUE,
                  global=TRUE, ginterval=c(3*r0,1),
                  transform=expression(Phi(.)))


###################################################
### code chunk number 60: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 61: 08distances.Rnw:1518-1521
###################################################
par(mfrow=c(1,2))
plot(Fglob, . ~ theo, main="", legend=FALSE)
plot(Gglob, . ~ theo, main="", legend=FALSE)


###################################################
### code chunk number 62: 08distances.Rnw:1557-1580
###################################################
## example for hazard rate of point process
X <- swedishpines
W <- Window(X)
P <- X[24]
D2 <- disc(radius=30.7, centre=P)
D1 <- disc(radius=28.5, centre=P)
OO <- setminus.owin(D2, D1)
## remove points from D1
Y <- X[!inside.owin(X, w=D1)]
## arrow from P to nearest neighbour
j <- nncross(P,Y,what="which")
A <- onearrow(superimpose(P, Y[j]))
## clean diagram by removing points close to nearest neighbour
Y <- superimpose(Y[j], Y[pairdist(Y)[j,] > 4])
D3 <- disc(radius=32.2, centre=P)
Y <- Y[!inside.owin(Y, w=setminus.owin(D3, D2))]
# point falling in thin ring with no points in disc
PicHaz <- layered(W, OO, Y, P, A,
                  plotargs=list(list(type="n"), 
                                list(col="grey"), 
                                list(pch=16), 
                                list(pch=3, cex=1),
                                list(do.points=FALSE, col.head="grey")))


###################################################
### code chunk number 63: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 64: 08distances.Rnw:1585-1586
###################################################
zeromargins()


###################################################
### code chunk number 65: 08distances.Rnw:1590-1591
###################################################
plot(PicHaz, main="")


###################################################
### code chunk number 66: 08distances.Rnw:1677-1678 (eval = FALSE)
###################################################
## plot(Fest(cells), cbind(hazard, theohaz) ~ r)


###################################################
### code chunk number 67: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 68: 08distances.Rnw:1684-1685
###################################################
setmargins(4,4,1,1)


###################################################
### code chunk number 69: 08distances.Rnw:1689-1697
###################################################
par(mfrow=c(1,3), cex=0.91)
plot(Fregu, cbind(hazard, theohaz) ~ r, 
     main="", col=1, legend=FALSE, ylab=expression(italic(h(r))))
plot(Finde, cbind(hazard, theohaz) ~ r, 
     main="", col=1, legend=FALSE, ylab=expression(italic(h(r))))
plot(Fclus, cbind(hazard, theohaz) ~ r, 
     main="", col=1, legend=FALSE, ylab=expression(italic(h(r))))
par(mfrow=c(1,1))


###################################################
### code chunk number 70: 08distances.Rnw:1716-1718
###################################################
hazenv <- envelope(swp, Fhazard, nsim=39, fix.n=TRUE,
                   transform=expression(./(2*pi*r)))


###################################################
### code chunk number 71: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 72: 08distances.Rnw:1730-1731
###################################################
plot(hazenv, legend=FALSE, main="")


###################################################
### code chunk number 73: 08distances.Rnw:1821-1824 (eval = FALSE)
###################################################
## FS <- Fest(swp)
## GS <- Gest(swp)
## DD <- eval.fv(GS-FS)


###################################################
### code chunk number 74: 08distances.Rnw:1830-1835 (eval = FALSE)
###################################################
## DigDif <- function(X, ..., r=NULL) {
##   FX <- Fest(X, ..., r=r)
##   GX <- Gest(X, ..., r=FX$r)
##   eval.fv(GX-FX)
## }


###################################################
### code chunk number 75: 08distances.Rnw:1840-1841 (eval = FALSE)
###################################################
## DE <- envelope(swp, DigDif, nsim=19, fix.n=TRUE, global=TRUE)


###################################################
### code chunk number 76: 08distances.Rnw:1887-1890
###################################################
Jregu <- Jest(regu)
Jinde <- Jest(inde)
Jclus <- Jest(clus)


###################################################
### code chunk number 77: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 78: 08distances.Rnw:1894-1895
###################################################
setmargins(4,4,1,1)


###################################################
### code chunk number 79: 08distances.Rnw:1899-1907
###################################################
par(mfrow=c(1,3), cex=0.91)
plot(Jregu, 
     cbind(km, theo) ~ r, main="", col=1, legend=FALSE, ylim.covers=c(0,2))
plot(Jinde, 
     cbind(km, theo) ~ r, main="", col=1, legend=FALSE, ylim.covers=c(0,2))
plot(Jclus, 
     cbind(km, theo) ~ r, main="", col=1, legend=FALSE, ylim.covers=c(0,2))
par(mfrow=c(1,1))


###################################################
### code chunk number 80: fasp2x2.Rnw:3-5
###################################################
newplot(12, 0.75)
setmargins(4, 4, 0, 0.5)


###################################################
### code chunk number 81: 08distances.Rnw:1987-1988
###################################################
plot(allstats(swp), main="", legend=FALSE)


###################################################
### code chunk number 82: 08distances.Rnw:2089-2092
###################################################
swp <- rescale(swedishpines)
JS <- Jest(swp)
JiS <- Jinhom(swp, sigma=bw.scott)


###################################################
### code chunk number 83: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 84: 08distances.Rnw:2097-2100
###################################################
par(mfrow=c(1,2))
plot(JS, main="", ylim.covers=0, legend=FALSE)
plot(JiS, main="", ylim.covers=0, legend=FALSE)


###################################################
### code chunk number 85: 08distances.Rnw:2116-2120
###################################################
set.seed(19221101)
X <- rSSI(0.05, win=owin(c(0,1), c(0, 3)))
Y <- affine(X, mat=diag(c(1, 1/3)))
fn <- nnorient(Y)


###################################################
### code chunk number 86: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 87: 08distances.Rnw:2149-2150
###################################################
rose(fn,  main="", col="grey")


###################################################
### code chunk number 88: 08distances.Rnw:2184-2206
###################################################
## synthetic data for diagram
set.seed(1944)
X <- runifpoint(25)
W <- levelset(density(X, dimyx=512), 25)
Y <- runifpoint(1, erosion(complement.owin(W), 0.15))
# clip
B <- shift(shift(square(0.6), origin="midpoint"), coords(Y))
W <- intersect.owin(W, B)
B <- intersect.owin(Frame(W), B)
Y <- Y[B]
X <- X[B]
## project Y onto W 
Z <- project2set(Y, W)
## make arrow
YZ <- onearrow(superimpose(Y, Z, W=Frame(X)))
## 
PCEX <- 1
Hdemo <- layered(W, Y, Z, YZ,
                 plotargs=list(list(col="grey"),
                     list(pch=16, cex=PCEX),
                     list(pch=1, cex=PCEX), 
                     list(headangle=20, lwd=2, col.head="black")))


###################################################
### code chunk number 89: 08distances.Rnw:2210-2211
###################################################
zeromargins()


###################################################
### code chunk number 90: 08distances.Rnw:2215-2216
###################################################
plot(Hdemo, main="")


###################################################
### code chunk number 91: Anemones.Rnw:3-5
###################################################
newplot(7,0.8)
setmargins(0)


###################################################
### code chunk number 92: 08distances.Rnw:2276-2277
###################################################
plot(anemones, main="", markscale=1, bg="grey", legend=FALSE)


###################################################
### code chunk number 93: 08distances.Rnw:2304-2307
###################################################
Anemones <- anemones
width <- sidelengths(Window(Anemones))[1]
unitname(Anemones) <- list("cm", "cm", 145/width)


###################################################
### code chunk number 94: 08distances.Rnw:2316-2318
###################################################
Anemones <- rescale(Anemones)
marks(Anemones) <- marks(Anemones) * 145/width


###################################################
### code chunk number 95: 08distances.Rnw:2324-2327
###################################################
AnemoneSet <- discs(Anemones, eps=0.1, mask=TRUE)
Hanem <- Hest(AnemoneSet, conditional=TRUE)
Hsmooth <- Smooth(Hanem, which="hazard")


###################################################
### code chunk number 96: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 97: 08distances.Rnw:2332-2339
###################################################
par(mfrow=c(1,2))
plot(Hanem, main="", 
     ylab=expression(italic(hat(H)(r))),
     legend=FALSE)
plot(Hsmooth, hazard ~ r, 
     ylab=expression(italic(hat(h)(r))), 
     main="")


###################################################
### code chunk number 98: 08distances.Rnw:2354-2355 (eval = FALSE)
###################################################
## AnemoneSet <- discs(Anemones, eps=0.1, mask=TRUE)


###################################################
### code chunk number 99: 08distances.Rnw:2364-2365 (eval = FALSE)
###################################################
## Hanem <- Hest(AnemoneSet, conditional=TRUE)


###################################################
### code chunk number 100: 08distances.Rnw:2370-2371 (eval = FALSE)
###################################################
## Hsmooth <- Smooth(Hanem, which="hazard")


###################################################
### code chunk number 101: 08distances.Rnw:2374-2383
###################################################
if(draftversion) {
  hh <- heather$coarse
  resolutionmessage <- 
      inFont("textbf", "(Coarse resolution image used for draft)")
} else {
  hh <- heather$fine
  resolutionmessage <- ""
}
hh <- flipxy(hh)


###################################################
### code chunk number 102: HeatherHoriz.Rnw:3-5
###################################################
newplot(6, 0.5)
zeromargins()


###################################################
### code chunk number 103: 08distances.Rnw:2390-2391
###################################################
plot(hh, main="")


###################################################
### code chunk number 104: 08distances.Rnw:2425-2427
###################################################
HH <- with(heather, Hest(fine))
SH <- Smooth(HH)


###################################################
### code chunk number 105: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 106: 08distances.Rnw:2434-2437
###################################################
par(mfrow=c(1,2))
plot(HH, main="")
plot(SH, hazard ~ r, main="", ylab=expression(italic(h(r))))


###################################################
### code chunk number 107: 08distances.Rnw:2544-2546
###################################################
GC <- with(copper, Gfox(SouthPoints, SouthLines))
JC <- with(copper, Jfox(SouthPoints, SouthLines, eps=0.1))


###################################################
### code chunk number 108: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 109: 08distances.Rnw:2557-2560
###################################################
par(mfrow=c(1,2))
plot(GC, main="", legend=FALSE)
plot(JC, main="", legend=FALSE, ylim.covers=0)


###################################################
### code chunk number 110: 08distances.Rnw:2623-2697
###################################################
## synthetic example for edge corrections
set.seed(1492192)
W <- square(1)
Wplus <- grow.rectangle(W, 1, 0.5)
## reference point
P <- ppp(0.9, 0.85, window=W)
R <- 0.4
DD <- disc(R, P)
Dlarge <- disc(R * 1.3, P)
Dsmall <- disc(R * 0.7, P)
## nearest neighbour (falls outside window)
Y <- runifpoint(2, setminus.owin(Dsmall, grow.rectangle(W, 0.04)))
## other, non-relevant, points
Z <- runifpoint(50, setminus.owin(Wplus, Dlarge))
## all data points
X <- superimpose(Y, Z, W=Wplus)
## 
FedgeEffect <- layered(Wplus, W, P, DD, X)
PCEX <- 1
layerplotargs(FedgeEffect) <- list(list(type="n"),
                                 list(lwd=2),
                                 list(pch=3, cex=PCEX),
                                 list(lty=2, lwd=2),
                                 list(pch=16, cex=PCEX))
## clip to smaller window
FedgeEffect <- FedgeEffect[ , trim.rectangle(Wplus, 0.5, 0.25)]
## 
Rtiny <- 0.2
Xplusr <- dilation(X, Rtiny)
bdryXplusr <- edges(Xplusr)[Wplus]
XplusrW <- intersect.owin(W, Xplusr)
FaverageArea <- layered(Wplus, bdryXplusr, XplusrW, W, P, X)
layerplotargs(FaverageArea) <- list(list(type="n"),
                                  list(),
                                  list(col="grey"),
                                  list(lwd=2),
                                  list(pch=3, cex=PCEX),
                                  list(pch=16, cex=PCEX))
## clip to smaller window
FaverageArea <- FaverageArea[ , trim.rectangle(Wplus, 0.5, 0.25)]
##
rr <- 0.155
Xplusrr <- dilation(X, rr)
Wminus <- erosion(W, rr)
bdryXplusrrW <- edges(Xplusrr)[setminus.owin(W, Wminus)]
WW <- grow.rectangle(W, rr)
XX <- X[WW]
XplusrrWminus <- intersect.owin(Wminus, Xplusrr)
Yard <- yardstick(mean(Wminus$xrange), Wminus$yrange[2],
                  mean(Wminus$xrange), W$yrange[2],
                  expression(italic(r)))
Fborder <- layered(WW, bdryXplusrrW, XplusrrWminus, W, Wminus, XX, Yard,
                   plotargs=list(list(type="n"),
                                 list(),
                                 list(col="grey", border=NA),
                                 list(lwd=2),
                                 list(lty=2, lwd=2),
                                 list(pch=16, cex=PCEX),
                                 list(cex=1.2)
                       ))
notXplusrrWminus <- setminus.owin(Wminus, Xplusrr)
bdryXplusrrWminus <- edges(Xplusrr)[Wminus]
FhazardGraph <- layered(WW, bdryXplusrrW, 
                   notXplusrrWminus, bdryXplusrrWminus, 
                   W, Wminus, XX, Yard, 
                   plotargs=list(list(type="n"),
                                 list(),
                                 list(col="grey", border=NA),
                                 list(lwd=5),
                                 list(lwd=2),
                                 list(lty=2, lwd=2),
                                 list(pch=16, cex=PCEX),
                                 list(cex=1.2, split=FALSE, length=0.15)
                       ))


###################################################
### code chunk number 111: 08distances.Rnw:2767-2768
###################################################
zeromargins()


###################################################
### code chunk number 112: 08distances.Rnw:2772-2773
###################################################
plot(FaverageArea[-5], main="")


###################################################
### code chunk number 113: 08distances.Rnw:2840-2841
###################################################
zeromargins()


###################################################
### code chunk number 114: 08distances.Rnw:2845-2846
###################################################
plot(FedgeEffect, main="")


###################################################
### code chunk number 115: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 116: 08distances.Rnw:2967-2968
###################################################
zeromargins()


###################################################
### code chunk number 117: 08distances.Rnw:2972-2974
###################################################
plot(solist(Fborder, FhazardGraph), 
     main="", main.panel="", equal.scales=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 118: 08distances.Rnw:3756-3758
###################################################
(1 - 0.9) == 0.1
1 - 10 * (1 - 0.9)


