### R code from vignette source '05pointproc.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 05pointproc.Rnw:9-10
###################################################
source("R/startup.R")


###################################################
### code chunk number 2: 05pointproc.Rnw:76-79
###################################################
set.seed(19171026)
Xlist <- rMatClust(20, 0.2, 5, nsim=10)
nX <- length(Xlist)


###################################################
### code chunk number 3: Unit5x2.Rnw:4-5
###################################################
zeromargins()


###################################################
### code chunk number 4: 05pointproc.Rnw:99-102
###################################################
plot(Xlist, main="", main.panel="", equal.scales=TRUE, 
     pch=16, cex=0.75, 
     nrows=2, mar.panel=0, hsep=1, vsep=1)


###################################################
### code chunk number 5: potatoes
###################################################
load("data/potatoes.rda") # provides potato-shaped windows A and B 


###################################################
### code chunk number 6: matclex
###################################################
set.seed(13042024)
X <- rpoispp(50)
W <- as.owin(X)


###################################################
### code chunk number 7: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 8: 05pointproc.Rnw:205-209
###################################################
PCEX <- 1.0
plot(as.owin(X), main="", type="n")
plot(A, add=TRUE, lwd=2, col="grey")
plot(X, add=TRUE, pch=16, cex=PCEX)


###################################################
### code chunk number 9: 05pointproc.Rnw:273-275
###################################################
set.seed(1234560)
X1 <- runifpoint(1, A) ## cheating: ensures X falls in A.


###################################################
### code chunk number 10: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 11: 05pointproc.Rnw:279-280
###################################################
setmargins(0)


###################################################
### code chunk number 12: 05pointproc.Rnw:284-294
###################################################
par(mfrow=c(1,2))
PCEX <- 1.0
plot(square(1), main="")
plot(X1, add=TRUE, pch=16, cex=PCEX)
segments(X1$x,    0, X1$x, 1,    lty=2, lwd=3)
segments(   0, X1$y,    1, X1$y, lty=2, lwd=3)
plot(square(1), main="")
plot(A, add=TRUE, lwd=2, col="grey")
plot(X1, add=TRUE, pch=16, cex=PCEX)
par(mfrow=c(1,1))


###################################################
### code chunk number 13: 05pointproc.Rnw:391-392
###################################################
Bin8list <- runifpoint(8, nsim=10)


###################################################
### code chunk number 14: Unit5x2.Rnw:4-5
###################################################
zeromargins()


###################################################
### code chunk number 15: 05pointproc.Rnw:397-400
###################################################
plot(Bin8list, main="", main.panel="", equal.scales=TRUE, 
     pch=16, cex=0.75, 
     nrows=2, mar.panel=0, hsep=1, vsep=1)


###################################################
### code chunk number 16: 05pointproc.Rnw:444-448
###################################################
set.seed(10982178)
PoisList <- rpoispp(50, nsim=10)
Pois8List <- rpoispp(8, nsim=10)
Pois8List[[7]] <- runifpoint(8) # cheat for purposes of illustration.


###################################################
### code chunk number 17: Unit5x2.Rnw:4-5
###################################################
zeromargins()


###################################################
### code chunk number 18: 05pointproc.Rnw:454-457
###################################################
plot(PoisList, main="", main.panel="", equal.scales=TRUE, 
     pch=16, cex=0.75, 
     nrows=2, mar.panel=0, hsep=1, vsep=1)


###################################################
### code chunk number 19: Unit5x2.Rnw:4-5
###################################################
zeromargins()


###################################################
### code chunk number 20: 05pointproc.Rnw:510-513
###################################################
plot(Pois8List, main="", main.panel="", equal.scales=TRUE, 
     pch=16, cex=0.75, 
     nrows=2, mar.panel=0, hsep=1, vsep=1)


###################################################
### code chunk number 21: 05pointproc.Rnw:541-558
###################################################
pb <- function(i, y, ..., main) {
  plot(as.owin(y), main=main, add=TRUE, show.all=TRUE, col=grey(0.4))
  R <- as.rectangle(y)
  vec <- unlist(centroid.owin(R))-0.5
  AA <- shift(A, vec)
  BB <- shift(B, vec)
  plot(AA, add=TRUE, col=grey(0.7), border=NA)
  plot(BB, add=TRUE, col=grey(0.7), border=NA)
}
pe <- function(i, y, ...) {
  R <- as.rectangle(y)
  vec <- unlist(centroid.owin(R))-0.5
  AA <- shift(A, vec)
  BB <- shift(B, vec)
  text(centroid.owin(AA), labels=npoints(y[AA]), cex=1.25)
  text(centroid.owin(BB), labels=npoints(y[BB]), cex=1.25)
}


###################################################
### code chunk number 22: 05pointproc.Rnw:561-563
###################################################
set.seed(26011788)
idsim <- solapply(1:3, function(z) { rpoispp(100) })


###################################################
### code chunk number 23: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 24: 05pointproc.Rnw:569-573
###################################################
plot(idsim, panel.begin=pb, panel.end=pe, equal.scales=TRUE, 
     main="", main.panel="", 
     mar.panel=c(0, 0.2, 0.2, 0), hsep=1,
     pch=21, bg="white", cols="white")


###################################################
### code chunk number 25: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 26: 05pointproc.Rnw:604-607
###################################################
plot(W, col="grey", main="")
plot(X, add=TRUE, pch=16, cols="white", cex=1.5)
plot(quadratcount(X, 5), add=TRUE, cex=2.5)


###################################################
### code chunk number 27: 05pointproc.Rnw:609-612
###################################################
plot(W, col="grey", main="")
plot(X, add=TRUE, pch=16, cols="white", cex=1.5)
plot(quadratcount(X, 15), add=TRUE, cex=1.2)


###################################################
### code chunk number 28: 05pointproc.Rnw:656-667
###################################################
N <- 20
QN <- quadrats(X, N)
TN <- tiles(QN)
ZN <- quadratcount(X, N)
AN <- pixellate(rebound.owin(A, W), dimyx=N)
AN <- t(as.matrix(AN)[N:1,])
hit <- as.vector(AN > 0.2/N^2)
cen <- lapply(TN, centroid.owin)
x0hit <- unlist(lapply(cen, getElement, "x"))[hit]
y0hit <- unlist(lapply(cen, getElement, "y"))[hit]
ZNhit <- t(as.matrix(ZN))[t(matrix(hit, N, N, byrow=TRUE))]


###################################################
### code chunk number 29: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 30: 05pointproc.Rnw:674-678
###################################################
plot(W, col="grey", main="")
plot(X, add=TRUE, pch=16, cols="white", cex=1.1)
text(x0hit, y0hit, pmin(1, ZNhit))
plot(A, add=TRUE)


###################################################
### code chunk number 31: 05pointproc.Rnw:721-722
###################################################
setmargins(4,4,1,1)


###################################################
### code chunk number 32: 05pointproc.Rnw:726-734
###################################################
par(mfrow=c(1,2))
kk <- 0:6
d1 <- dpois(kk, 0.6)
d2 <- dpois(kk, 1.7)
yr <- range(d1, d2)
barplot(d1, names.arg=kk, ylab="Probability", ylim=yr, xlab="Count")
barplot(d2, names.arg=kk, ylab="Probability", ylim=yr, xlab="Count")
par(mfrow=c(1,1))


###################################################
### code chunk number 33: 05pointproc.Rnw:801-809
###################################################
set.seed(1848)
X <- rpoispp(100)
retain <- (runif(npoints(X)) < 0.4)
Y <- X[retain]
ThinDemo <- solist(layered(X, plotargs=list(pch=16)),
                   layered(X[retain], X[!retain],
                           plotargs=list(list(pch=16), list(pch=1))),
                   layered(Y, plotargs=list(pch=16)))


###################################################
### code chunk number 34: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 35: 05pointproc.Rnw:814-816
###################################################
plot(ThinDemo, main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 36: 05pointproc.Rnw:838-842
###################################################
set.seed(1967)
X <- rpoispp(40)
Y <- rpoispp(60)
Z <- superimpose(X,Y, W=square(1))


###################################################
### code chunk number 37: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 38: 05pointproc.Rnw:847-850
###################################################
plot(solist(X, Y, Z), 
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 39: 05pointproc.Rnw:879-880 (eval = FALSE)
###################################################
## plot(rpoispp(50))


###################################################
### code chunk number 40: 05pointproc.Rnw:889-890 (eval = FALSE)
###################################################
## plot(rpoispp(100, nsim=9))


###################################################
### code chunk number 41: 05pointproc.Rnw:910-911 (eval = FALSE)
###################################################
## plot(rpoispp(100, win=letterR))


###################################################
### code chunk number 42: Rletter.Rnw:3-5
###################################################
newplot(4, 0.3)
setmargins(0)


###################################################
### code chunk number 43: 05pointproc.Rnw:917-918
###################################################
plot(rpoispp(100, win=letterR), main="", pch=16)


###################################################
### code chunk number 44: 05pointproc.Rnw:1056-1058
###################################################
lambda <- function(x,y) { 100 * (x^2+y) }
X <- rpoispp(lambda, win=square(1))


###################################################
### code chunk number 45: 05pointproc.Rnw:1060-1064
###################################################
lamim <- as.im(lambda, square(1))
L <- layered(lamim, lamim,
             plotargs=list(list(),
                           list(.plot="contour", col="white")))


###################################################
### code chunk number 46: Unit2r.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 47: 05pointproc.Rnw:1071-1074
###################################################
plot(solist(L, X),
     main="", main.panel="", equal.scales=TRUE, 
     pch=16, hsep=1, mar.panel=0.2)


###################################################
### code chunk number 48: 05pointproc.Rnw:1119-1121
###################################################
set.seed(3332)
Rad <- 0.15


###################################################
### code chunk number 49: 05pointproc.Rnw:1123-1124
###################################################
Xmc <- rMatClust(kappa=8, r=0.15, mu=5)


###################################################
### code chunk number 50: 05pointproc.Rnw:1126-1129
###################################################
Ymc <- attr(Xmc, "parents")
WB <- as.owin(Ymc)
W <- as.owin(Xmc)


###################################################
### code chunk number 51: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 52: 05pointproc.Rnw:1134-1135
###################################################
setmargins(0.5)


###################################################
### code chunk number 53: 05pointproc.Rnw:1139-1154
###################################################
CEX <- 1.2
opa <- par(mfrow=c(1,3), mar=rep(0.5,4))
plot(WB, type="n", main="")
plot(W, add=TRUE)
plot(Ymc, add=TRUE, pch=16, cex=CEX)

plot(WB, type="n", main="")
plot(W, add=TRUE)
plot(Ymc, pch=16,add=TRUE, cex=CEX)
for(i in 1:npoints(Ymc)) plot(disc(Rad, Ymc[i]), add=TRUE, lty=3)
plot(Xmc, add=TRUE, cex=CEX)

plot(WB, type="n", main="")
plot(W, add=TRUE)
plot(Xmc, add=TRUE, cex=CEX)


###################################################
### code chunk number 54: 05pointproc.Rnw:1189-1190
###################################################
XmI <- rMaternI(30, 0.1)


###################################################
### code chunk number 55: 05pointproc.Rnw:1192-1195
###################################################
R <- 0.1
P <- rpoispp(30)
kill <- (nndist(P) < R)


###################################################
### code chunk number 56: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 57: 05pointproc.Rnw:1201-1210
###################################################
par(mfrow=c(1,3))
# left
plot(P, main="")
# middle
plot(P[kill], pch=4, main="")
points(P[!kill], pch=16)
# right
plot(P[!kill], main="", pch=16)
par(mfrow=c(1,1))


###################################################
### code chunk number 58: 05pointproc.Rnw:1231-1232
###################################################
XmII <- rMaternII(30, 0.1)


###################################################
### code chunk number 59: 05pointproc.Rnw:1234-1239
###################################################
age <- runif(npoints(P))
D <- pairdist(P)
close <- (D < R)
earlier <- outer(age, age, "<")
killyoung <- apply(close & earlier, 1, any)


###################################################
### code chunk number 60: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 61: 05pointproc.Rnw:1245-1254
###################################################
par(mfrow=c(1,3))
# left
plot(P, main="")
# middle
plot(P[killyoung], pch=4, main="")
points(P[!killyoung], pch=16)
# right
plot(P[!killyoung], main="", pch=16)
par(mfrow=c(1,1))


###################################################
### code chunk number 62: 05pointproc.Rnw:1288-1289
###################################################
Xs <- rSSI(0.05, n = 200)


###################################################
### code chunk number 63: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 64: 05pointproc.Rnw:1296-1297
###################################################
plot(Xs, pch=16, main="")


###################################################
### code chunk number 65: 05pointproc.Rnw:1481-1484
###################################################
W <- square(1)
R2 <- grow.rectangle(W, 2, 0.5)
X <- rpoispp(50,win=R2)


###################################################
### code chunk number 66: 05pointproc.Rnw:1490-1494
###################################################
plot(R2, type="n", main="")
plot(X[W], pch=16,add=TRUE)
plot(X[complement.owin(W, R2)], add=TRUE)
plot(W, add=TRUE)


###################################################
### code chunk number 67: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 68: 05pointproc.Rnw:1530-1541
###################################################
set.seed(7891095)
ncell <- 16
X <- rSSI(sqrt(2)/ncell,12)
XX <- layered(X, plotargs=list(pch=16))
Y <- quadratcount(X, ncell)
is0 <- (as.vector(t(as.table(Y))) == 0)
cols <- ifelse(is0, "grey", "black")
YY <- layered(as.owin(Y), Y, 
              plotargs=list(list(), list(show.tiles=FALSE, col=cols)))
plot(solist(XX, YY), main="", main.panel="", equal.scales=TRUE,
     mar.panel=rep(0.2,4), hsep=1)


###################################################
### code chunk number 69: 05pointproc.Rnw:1587-1588
###################################################
GR <- rotate(gordon, 0.95)


###################################################
### code chunk number 70: GordonRot.Rnw:3-5
###################################################
newplot(6.6, 0.85)
setmargins(0)


###################################################
### code chunk number 71: 05pointproc.Rnw:1594-1596
###################################################
plot(Window(GR), col="grey", main="")
plot(GR, add=TRUE, pch=16)


###################################################
### code chunk number 72: 05pointproc.Rnw:1825-1826
###################################################
setmargins(0.1)


###################################################
### code chunk number 73: 05pointproc.Rnw:1830-1836
###################################################
Y <- bei.extra$elev
par(mfrow=c(1,2))
plot(Y, main="", axes=FALSE)
contour(Y, add=TRUE, nlevels=6)
persp(Y, main="", theta=20, phi=30, border=NA, shade=0.6, zlab="Elevation")
par(mfrow=c(1,1))


###################################################
### code chunk number 74: 05pointproc.Rnw:1881-1887
###################################################
Y <- rotate(copper[["SouthLines"]], pi/2)
Z <- distmap(Y, eps=0.25)
ZY <- layered(Z, Y, Window(Y), 
              plotargs=list(list(ribbon=FALSE, col=lighttodark),
                            list(lwd=2, col="white"),
                            list()))


###################################################
### code chunk number 75: CopperS2.Rnw:3-5
###################################################
newplot(6, 0.95)
setmargins(0)


###################################################
### code chunk number 76: 05pointproc.Rnw:1893-1896
###################################################
plot(solist(Y, ZY), ncols=1,
     main="", main.panel="", equal.scales=TRUE, halign=TRUE,
     mar.panel=0, vsep=0.5)


###################################################
### code chunk number 77: 05pointproc.Rnw:2021-2027
###################################################
swp <- rescale(swedishpines)
lam <- predict(ppm(swp ~ polynom(x,y,2)))
K <- Kest(swp)
Ki <- Kinhom(swp, lam)
coco <- layered(swp, lam,
                plotargs=list(list(cols="grey"), list(.plot="contour")))


###################################################
### code chunk number 78: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 79: 05pointproc.Rnw:2033-2035
###################################################
plot(solist(swp, coco), pch=16, main="", main.panel="", 
     mar.panel=0, hsep=1)


###################################################
### code chunk number 80: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 81: 05pointproc.Rnw:2047-2048
###################################################
plot(anylist(K, Ki), main="", main.panel="", legend=FALSE, xlim=c(0, 2))


