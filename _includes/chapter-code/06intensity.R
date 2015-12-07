### R code from vignette source '06intensity.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 06intensity.Rnw:9-14
###################################################
source("R/startup.R")
require(maptools)
requireversion(spatstat, "1.41-1.052")
Digits <- 4
options(digits=Digits)


###################################################
### code chunk number 2: 06intensity.Rnw:53-54
###################################################
setmargins(0)


###################################################
### code chunk number 3: 06intensity.Rnw:59-63
###################################################
nzt <- nztrees[owin(c(0,148),c(0,95))]
plot(solist(cells, nzt, swedishpines), 
     main="", main.panel="", mar.panel=rep(0.1,4),
     pch=16)


###################################################
### code chunk number 4: 06intensity.Rnw:123-124
###################################################
source("R/fijiquakes.R")


###################################################
### code chunk number 5: 06intensity.Rnw:128-129
###################################################
setmargins(0)


###################################################
### code chunk number 6: 06intensity.Rnw:134-142
###################################################
par(mfrow=c(1,3), mar=rep(0,4))
plot(split(mucosa)[[1]], main="", pch=16)
plot(unmark(gorillas), main="", pch=16, cex=0.9)
showzone(annotate=FALSE, lwd=2)
box()
plot(qk, pch=3, cex=0.5, col=if(monochrome) "grey" else "red", add=TRUE)
annotatezone()
par(mfrow=c(1,1))


###################################################
### code chunk number 7: 06intensity.Rnw:242-244
###################################################
set.seed(333444)
HomoDemo <- solist(runifpoispp(50), runifpoispp(500))


###################################################
### code chunk number 8: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 9: 06intensity.Rnw:251-253
###################################################
plot(HomoDemo,
     main="", main.panel="", mar.panel=0, hsep=1)


###################################################
### code chunk number 10: 06intensity.Rnw:348-351
###################################################
X <- rescale(swedishpines)
lam <- intensity(X)
(sdX <- sqrt(lam/area(Window(X))))


###################################################
### code chunk number 11: 06intensity.Rnw:367-370
###################################################
unitname(amacrine)
X <- rescale(amacrine, 1000/662, "mm")
intensity(X)


###################################################
### code chunk number 12: 06intensity.Rnw:382-383
###################################################
finpines


###################################################
### code chunk number 13: 06intensity.Rnw:389-392
###################################################
height <- marks(finpines)$height
diameter <- marks(finpines)$diameter
volume <- (pi/12) * height * (diameter/100)^2


###################################################
### code chunk number 14: 06intensity.Rnw:395-396
###################################################
volume <- with(marks(finpines), (pi/12) * height * (diameter/100)^2)


###################################################
### code chunk number 15: 06intensity.Rnw:399-400
###################################################
intensity(finpines, weights=volume)


###################################################
### code chunk number 16: 06intensity.Rnw:420-433
###################################################
set.seed(672672)
v <- edges(letterR)
Wplus <- grow.rectangle(as.rectangle(as.owin(v)), 0.5)
v <- v[Wplus]
v <- rescale(v, sidelengths(as.rectangle(v))[2])
fun1 <- function(x,y){ 700* exp(-10*((x-0.5)^2+(y-0.5)^2)) }
fun2 <- function(x,y){ 300 * x^2 }
im1 <- as.im(fun1, square(1))
im2 <- as.im(fun2, square(1))
HeteroDemo <- solist(
  rpoispp(fun1),
  rpoispp(fun2),
  runifpointOnLines(100,v))


###################################################
### code chunk number 17: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 18: 06intensity.Rnw:439-441
###################################################
plot(HeteroDemo,
     main="", main.panel="", mar.panel=0, hsep=1, pch=3)


###################################################
### code chunk number 19: persp2.Rnw:3-5
###################################################
newplot(12.5, 0.85)
setmargins(0.5+c(1,1,0,1))


###################################################
### code chunk number 20: 06intensity.Rnw:486-490
###################################################
par(mfrow=c(1,2), mar=rep(0.2, 4))
persp(im1, main="", shade=0.7, border=NA, theta=-20, phi=20, zlab="intensity")
persp(im2, main="", shade=0.7, border=NA, theta=-20, phi=20, zlab="intensity")
par(mfrow=c(1,1))


###################################################
### code chunk number 21: 06intensity.Rnw:602-605
###################################################
swp <- rescale(swedishpines)
Q3 <- quadratcount(swp, nx=3, ny=3)
Q3


###################################################
### code chunk number 22: 06intensity.Rnw:614-617
###################################################
Q3plus <- layered(Q3, swp, 
                  plotargs=list(list(cex=2), list(pch="+")))
L3 <- intensity(Q3, image=TRUE)


###################################################
### code chunk number 23: Unit2r.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 24: 06intensity.Rnw:621-622
###################################################
setmargins(0,0,0,2)


###################################################
### code chunk number 25: 06intensity.Rnw:627-630
###################################################
pa <- function(i) { if(i == 1) list() else list(box=TRUE, ribargs=list(las=1)) }
plot(solist(Q3plus, L3), main="", main.panel="", mar.panel=0, hsep=1,
     panel.args=pa, equal.scales=TRUE)


###################################################
### code chunk number 26: 06intensity.Rnw:662-663
###################################################
intensity(Q3)


###################################################
### code chunk number 27: 06intensity.Rnw:678-681
###################################################
l3 <- as.numeric(intensity(Q3))
sem <- sqrt(var(l3)/(length(l3)-1))
sem


###################################################
### code chunk number 28: hQ
###################################################
H <- hextess(swp, 1)
hQ <- quadratcount(swp, tess=H)


###################################################
### code chunk number 29: 06intensity.Rnw:708-709
###################################################
hL <- intensity(hQ, image=TRUE, dimyx=256)


###################################################
### code chunk number 30: Unit2r.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 31: 06intensity.Rnw:714-715
###################################################
setmargins(0,0,0,2)


###################################################
### code chunk number 32: 06intensity.Rnw:720-722
###################################################
plot(solist(hQ, hL), main="", main.panel="", equal.scales=TRUE,
     mar.panel=c(0,0,0,1))


###################################################
### code chunk number 33: 06intensity.Rnw:814-816
###################################################
tS <- quadrat.test(swp, 3,3)
tS


###################################################
### code chunk number 34: 06intensity.Rnw:824-825
###################################################
tS$p.value


###################################################
### code chunk number 35: 06intensity.Rnw:841-842
###################################################
setmargins(0)


###################################################
### code chunk number 36: 06intensity.Rnw:846-848
###################################################
plot(swp, pch=16, cols="grey", main="")
plot(tS, add=TRUE,cex=1.1)


###################################################
### code chunk number 37: 06intensity.Rnw:859-860
###################################################
quadrat.test(swp, 5, alternative="regular", method="MonteCarlo")


###################################################
### code chunk number 38: 06intensity.Rnw:867-868
###################################################
quadrat.test(Q3)


###################################################
### code chunk number 39: 06intensity.Rnw:1028-1036
###################################################
denUni <- density(swp, sigma=1)
denDig <- density(swp, sigma=1, diggle=TRUE)
denRaw <- density(swp, sigma=1, edge=FALSE)
den <- denUni
ra <- range(sapply(list(denUni, denDig, denRaw), range))
contourafter <- function(i,y, ...){
  contour(y, ..., col="white", drawlabels=FALSE)
}


###################################################
### code chunk number 40: 06intensity.Rnw:1041-1042
###################################################
setmargins(0)


###################################################
### code chunk number 41: 06intensity.Rnw:1047-1050
###################################################
plot(solist(denRaw, denUni, denDig),
     main="", main.panel="", equal.ribbon=TRUE,
     panel.end = contourafter, zlim=ra)


###################################################
### code chunk number 42: 06intensity.Rnw:1155-1156 (eval = FALSE)
###################################################
## den <- density(swp, sigma=1)


###################################################
### code chunk number 43: 06intensity.Rnw:1190-1196
###################################################
layout(matrix(1:2, nrow=1), widths=c(6,4), heights=4, respect=TRUE)
persp(den, 
      col=if(ebook) "blue" else NULL, 
      border=NA, theta=-30, shade=0.75, main="")
contour(den, axes=FALSE, main="")
layout(1)


###################################################
### code chunk number 44: 06intensity.Rnw:1217-1221
###################################################
sig <- c(0.5, 1.0, 1.5)
ds <- solapply(sig, function(x) { density(swp, sigma=x) })
names(ds) <- parse(text=paste("sigma = ", sig))
dra <- range(sapply(ds, range))


###################################################
### code chunk number 45: 06intensity.Rnw:1229-1231
###################################################
plot(ds, main="", nrows=1, equal.ribbon=TRUE, ribscale=100,
     panel.end=contourafter, zlim=dra)


###################################################
### code chunk number 46: 06intensity.Rnw:1265-1266
###################################################
b <- bw.ppl(swp, ns=128)


###################################################
### code chunk number 47: 06intensity.Rnw:1268-1270 (eval = FALSE)
###################################################
## b <- bw.ppl(swp)
## b


###################################################
### code chunk number 48: 06intensity.Rnw:1272-1273
###################################################
b


###################################################
### code chunk number 49: 06intensity.Rnw:1289-1290
###################################################
setmargins(3,3.5,0.2,0.8)


###################################################
### code chunk number 50: 06intensity.Rnw:1295-1300
###################################################
par(mfrow=c(1,2), pty="s")
oa <- list(lty=2, lwd=2, col=if(monochrome) 1 else 4)
plot(b, main="", optargs=oa)
plot(b, main="", xlim=c(3,6), optargs=oa)
par(mfrow=c(1,1))


###################################################
### code chunk number 51: 06intensity.Rnw:1328-1330 (eval = FALSE)
###################################################
## D <- density(swp, sigma=bw.diggle(swp))
## D <- density(swp, sigma=bw.diggle)


###################################################
### code chunk number 52: 06intensity.Rnw:1332-1335
###################################################
bwd <- bw.diggle(swp)
bws <- bw.scott(swp)
bwp <- bw.ppl(swp)


###################################################
### code chunk number 53: 06intensity.Rnw:1357-1358
###################################################
D <- density(swp, sigma=bw.diggle, adjust=2)


###################################################
### code chunk number 54: 06intensity.Rnw:1398-1400
###################################################
dX <- density(swp, sigma=1, at="points")
dX[1:5]


###################################################
### code chunk number 55: 06intensity.Rnw:1421-1426
###################################################
den <- density(swp, sigma=1)
denXpixel <- den[swp]
denXpixel[1:5]
denXexact <- density(swp, sigma=1, at="points", leaveoneout=FALSE)
denXexact[1:5]


###################################################
### code chunk number 56: 06intensity.Rnw:1465-1466
###################################################
dse <- density(swp, 1, se=TRUE)$SE


###################################################
### code chunk number 57: UnitR.Rnw:3-6
###################################################
newplot(9, 0.7)
zeromargins() # strip all margins
setmargins(0.1 + c(0,0,0,2)) # back off


###################################################
### code chunk number 58: 06intensity.Rnw:1478-1480
###################################################
plot(dse, main="", col=lighttodark)
contour(dse, add=TRUE, drawlabels=FALSE)


###################################################
### code chunk number 59: 06intensity.Rnw:1499-1501
###################################################
vols <- with(marks(finpines), (pi/12) * height * (diameter/100)^2)
fwd <- density(finpines, weights=vols, sigma=bw.ppl)


###################################################
### code chunk number 60: 06intensity.Rnw:1515-1516
###################################################
setmargins(0,0,0,2)


###################################################
### code chunk number 61: 06intensity.Rnw:1521-1522
###################################################
plot(fwd, main="", ribargs=list(las=1))


###################################################
### code chunk number 62: 06intensity.Rnw:1547-1550 (eval = FALSE)
###################################################
## vols <- with(marks(finpines), 
##              (pi/12) * height * (diameter/100)^2)
## Dvol <- density(finpines, weights=vols, sigma=bw.ppl)


###################################################
### code chunk number 63: 06intensity.Rnw:1554-1555
###################################################
intensity(finpines, weights=vols)


###################################################
### code chunk number 64: 06intensity.Rnw:1602-1603
###################################################
vden <- adaptive.density(swp, f=1)


###################################################
### code chunk number 65: 06intensity.Rnw:1614-1615
###################################################
aden <- adaptive.density(swp, f=0.1, nrep=30)


###################################################
### code chunk number 66: 06intensity.Rnw:1635-1636
###################################################
setmargins(0, 0, 0, 2)


###################################################
### code chunk number 67: 06intensity.Rnw:1638-1644
###################################################
nden <- nndensity(swp, k=10)
## rainbow with increasing saturation 
rainsat <- function(n) {
  grade <- sqrt(seq(0.1, 1, length=n))
  rainbow(n=n, start=1/2, s=grade)
}


###################################################
### code chunk number 68: 06intensity.Rnw:1649-1654
###################################################
par(mfrow=c(1,2))
plot(aden, main="", ribscale=1000, col=rainsat)
plot(swp, add=TRUE, pch=3)
plot(nden, main="", ribscale=1000, col=rainsat)
plot(swp, add=TRUE, pch=3)


###################################################
### code chunk number 69: 06intensity.Rnw:1672-1673
###################################################
setmargins(0)


###################################################
### code chunk number 70: 06intensity.Rnw:1770-1773
###################################################
grad <- bei.extra$grad
dens.map <- density(bei, W=grad)
dens.ter <- dens.map * sqrt(1+grad^2)


###################################################
### code chunk number 71: 06intensity.Rnw:1784-1785 (eval = FALSE)
###################################################
## persp(bei.extra$elev, colin=dens.ter)


###################################################
### code chunk number 72: 06intensity.Rnw:1789-1790
###################################################
zeromargins()


###################################################
### code chunk number 73: 06intensity.Rnw:1795-1801
###################################################
mapmessage <- 
  if(monochrome) "Lighter shades represent higher predicted densities." else ""
col.lam <- if(monochrome) grey(seq(0,1,length=128)) else topo.colors
persp(bei.extra$elev, colin=dens.ter,
      colmap=col.lam, shade=0.4, theta=-55, phi=25, expand=6, 
      box=FALSE, apron=TRUE, main="")


###################################################
### code chunk number 74: 06intensity.Rnw:1820-1821
###################################################
dens.ter2 <- density(bei, weights=sqrt(1+grad[bei]^2))


###################################################
### code chunk number 75: Bei2r.Rnw:3-5
###################################################
newplot(8.5,1)
setmargins(0.1, 0.1, 0.1, 1.6)


###################################################
### code chunk number 76: 06intensity.Rnw:1854-1857
###################################################
plot(solist(bei, bei.extra[["elev"]]), 
     equal.scales=TRUE, main="", main.panel="", mar.panel=0, hsep=0.5,
     pch=".", ribargs=list(las=1))


###################################################
### code chunk number 77: 06intensity.Rnw:1886-1890
###################################################
elev <- bei.extra$elev
b <- quantile(elev, probs=(0:4)/4, type=2)
Zcut <- cut(elev, breaks=b, labels=1:4)
V <- tess(image=Zcut)


###################################################
### code chunk number 78: BeiR.Rnw:3-5
###################################################
newplot(13,0.95)
setmargins(0,0,0,1)


###################################################
### code chunk number 79: 06intensity.Rnw:1909-1911
###################################################
## plot(V, col=grey((0:3)/4), main="")
textureplot(V, main="", spacing=20)


###################################################
### code chunk number 80: 06intensity.Rnw:1927-1929
###################################################
qb <- quadratcount(bei, tess=V)
qb


###################################################
### code chunk number 81: 06intensity.Rnw:1940-1944
###################################################
b5 <- seq(0, 5 * ceiling(max(elev)/5), by=5)
Zcut5 <- cut(elev, breaks=b5, include.lowest=TRUE)
Q5 <- quadratcount(bei, tess=tess(image=Zcut5))
lam5 <- intensity(Q5)


###################################################
### code chunk number 82: 06intensity.Rnw:1950-1951
###################################################
setmargins(3.5,3.5,0.1,0.1)


###################################################
### code chunk number 83: 06intensity.Rnw:1955-1957
###################################################
par(font.lab=1)
barplot(lam5, main="", ylab="Intensity", xlab="Elevation")


###################################################
### code chunk number 84: 06intensity.Rnw:2067-2068
###################################################
rh <- rhohat(bei, elev)


###################################################
### code chunk number 85: 06intensity.Rnw:2096-2097
###################################################
prh <- predict(rh)


###################################################
### code chunk number 86: 06intensity.Rnw:2102-2103
###################################################
zeromargins()


###################################################
### code chunk number 87: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 88: 06intensity.Rnw:2110-2112
###################################################
par(pty="s")
plot(rh, main="", legend=FALSE, do.rug=FALSE, ribscale=1000)


###################################################
### code chunk number 89: BeiR.Rnw:3-5
###################################################
newplot(13,0.95)
setmargins(0,0,0,1)


###################################################
### code chunk number 90: 06intensity.Rnw:2117-2118
###################################################
plot(prh, main="", ribscale=1000, ribargs=list(las=1))


###################################################
### code chunk number 91: 06intensity.Rnw:2139-2141
###################################################
rhf <- as.function(rh)
rhf(130)


###################################################
### code chunk number 92: 06intensity.Rnw:2159-2161 (eval = FALSE)
###################################################
## pred <- predict(rh)
## kden <- density(bei, 50)


###################################################
### code chunk number 93: 06intensity.Rnw:2231-2232 (eval = FALSE)
###################################################
## with(bei.extra, rho2hat(bei, grad, elev))


###################################################
### code chunk number 94: 06intensity.Rnw:2247-2249
###################################################
X <- rotate(copper$SouthPoints, pi/2)
L <- rotate(copper$SouthLines, pi/2)


###################################################
### code chunk number 95: CopperSouth.Rnw:3-5
###################################################
newplot(16, 0.9)
setmargins(0)


###################################################
### code chunk number 96: 06intensity.Rnw:2253-2254
###################################################
setmargins(0)


###################################################
### code chunk number 97: 06intensity.Rnw:2258-2260
###################################################
plot(X, pch=16, main="")
plot(L, add=TRUE)


###################################################
### code chunk number 98: CopperSouth.Rnw:3-5
###################################################
newplot(16, 0.9)
setmargins(0)


###################################################
### code chunk number 99: 06intensity.Rnw:2284-2285
###################################################
setmargins(0)


###################################################
### code chunk number 100: 06intensity.Rnw:2289-2292
###################################################
Z <- distmap(L)
plot(L, lwd=2, main="")
contour(Z, add=TRUE, drawlabels=FALSE)


###################################################
### code chunk number 101: fvSquat.Rnw:3-5
###################################################
newplot(6, 0.65)
setmargins(0.5+c(3,3,0,0))


###################################################
### code chunk number 102: 06intensity.Rnw:2306-2307
###################################################
setmargins(3,4,0.2,0.2)


###################################################
### code chunk number 103: 06intensity.Rnw:2311-2312
###################################################
plot(rhohat(X,Z), xlab="Distance to nearest fault", main="", legend=FALSE)


###################################################
### code chunk number 104: 06intensity.Rnw:2327-2330
###################################################
f <- distfun(L)
f
f(-42, 10)


###################################################
### code chunk number 105: 06intensity.Rnw:2385-2386
###################################################
V <- quantess(Window(bei), elev, 4)


###################################################
### code chunk number 106: 06intensity.Rnw:2391-2392
###################################################
quadrat.test(bei, tess=V)


###################################################
### code chunk number 107: 06intensity.Rnw:2463-2464
###################################################
cdf.test(bei, elev)


###################################################
### code chunk number 108: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 109: 06intensity.Rnw:2476-2477
###################################################
setmargins(3,3,0.2,0.2)


###################################################
### code chunk number 110: 06intensity.Rnw:2481-2482
###################################################
plot(cdf.test(bei, elev), main="", lty0="dotted", lwd=2)


###################################################
### code chunk number 111: 06intensity.Rnw:2520-2521
###################################################
cdf.test(swp, "x")


###################################################
### code chunk number 112: 06intensity.Rnw:2550-2553
###################################################
elev <- bei.extra$elev
B <- berman.test(bei, elev)
B


###################################################
### code chunk number 113: 06intensity.Rnw:2578-2579 (eval = FALSE)
###################################################
## berman.test(bei, elev,"Z2")


###################################################
### code chunk number 114: 06intensity.Rnw:2649-2651
###################################################
coproc <- with(copper, 
               roc(SouthPoints, distfun(SouthLines), high=FALSE))


###################################################
### code chunk number 115: 06intensity.Rnw:2653-2654 (eval = FALSE)
###################################################
## plot(coproc)


###################################################
### code chunk number 116: 06intensity.Rnw:2663-2664
###################################################
murroc <- with(murchison, roc(gold, distfun(faults), high=FALSE))


###################################################
### code chunk number 117: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 118: 06intensity.Rnw:2671-2673
###################################################
plot(anylist(coproc, murroc),
     main="", main.panel="", legend=FALSE)


###################################################
### code chunk number 119: 06intensity.Rnw:2692-2693 (eval = FALSE)
###################################################
## murroc <- with(murchison, roc(gold, distfun(faults), high=FALSE))


###################################################
### code chunk number 120: 06intensity.Rnw:2717-2719
###################################################
with(copper, auc(SouthPoints, distfun(SouthLines), high=FALSE))
with(murchison, auc(gold, distfun(faults), high=FALSE))


###################################################
### code chunk number 121: Unit2r.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 122: 06intensity.Rnw:2789-2790
###################################################
denRed <- density(redwood, bw.ppl, ns=16)


###################################################
### code chunk number 123: 06intensity.Rnw:2792-2794
###################################################
plot(solist(redwood, denRed), main="", main.panel="", 
     mar.panel=0.2, equal.scales=TRUE)


###################################################
### code chunk number 124: 06intensity.Rnw:2815-2816 (eval = FALSE)
###################################################
## denRed <- density(redwood, bw.ppl, ns=16)


###################################################
### code chunk number 125: 06intensity.Rnw:2831-2841 (eval = FALSE)
###################################################
## obsmax <- max(denRed)
## simmax <- numeric(99)
## lamRed <- intensity(redwood)
## winRed <- as.owin(redwood)
## for(i in 1:99) {
##   Xsim <- rpoispp(lamRed, win=winRed)
##   denXsim <- density(Xsim, bw.ppl, ns=16)
##   simmax[i] <- max(denXsim)
## }
## (pval <- (1+sum(simmax > obsmax))/100)


###################################################
### code chunk number 126: 06intensity.Rnw:2843-2860
###################################################
reload.or.compute(datafilepath("densitypval.rda"),
                  {
                    ## compute and save to file
                    set.seed(1985198)
                    obsmax <- max(denRed)
                    simmax <- numeric(99)
                    lamRed <- intensity(redwood)
                    winRed <- as.owin(redwood)
                    for(i in 1:99) {
                      Xsim <- rpoispp(lamRed, win=winRed)
                      denXsim <- density(Xsim, bw.ppl, ns=16)
                      simmax[i] <- max(denXsim)
                    }
                    pval <- (1+sum(simmax > obsmax))/100
                  },
                  c("obsmax", "simmax", "pval"))
pval


###################################################
### code chunk number 127: 06intensity.Rnw:2893-2894
###################################################
LR <- scanLRTS(redwood, r = 2 * bw.ppl(redwood))


###################################################
### code chunk number 128: UnitR.Rnw:3-6
###################################################
newplot(9, 0.7)
zeromargins() # strip all margins
setmargins(0.1 + c(0,0,0,2)) # back off


###################################################
### code chunk number 129: 06intensity.Rnw:2901-2902
###################################################
plot(LR, main="", col=greytoblack)


###################################################
### code chunk number 130: 06intensity.Rnw:2932-2933
###################################################
pvals <- eval.im(pchisq(LR, df=1, lower.tail=FALSE))


###################################################
### code chunk number 131: 06intensity.Rnw:2938-2939
###################################################
psig <- levelset(pvals, 0.01)


###################################################
### code chunk number 132: Unit2r.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 133: 06intensity.Rnw:2942-2943
###################################################
setmargins(0)


###################################################
### code chunk number 134: 06intensity.Rnw:2948-2953
###################################################
plot(solist(pvals, 
            layered(psig, Frame(psig))),
     panel.end=as.owin(redwood),
     log=TRUE, equal.scales=TRUE, nrows=1,
     main="", main.panel="", mar.panel=0.1, hsep=2)


###################################################
### code chunk number 135: 06intensity.Rnw:3024-3033
###################################################
set.seed(76987)
lam0 <- 20
lam1 <- 100
W <- grow.rectangle(as.rectangle(letterR), 1)
lam <- function(x,y) { ifelse(inside.owin(x,y,letterR), lam1, lam0) }
X <- rpoispp(lam, lmax=100, win=W)
XR <- layered(X, letterR, plotargs=list(list(pch=16), list(lty=3)))
cX <- clusterset(X, what="domain")
cXG <- layered(cX, plotargs=list(list(col="green", border="green")))


###################################################
### code chunk number 136: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 137: 06intensity.Rnw:3040-3042
###################################################
plot(solist(XR, cXG), equal.scales=TRUE, main="", main.panel="", 
     mar.panel=0.2, hsep=1)


###################################################
### code chunk number 138: 06intensity.Rnw:3105-3106 (eval = FALSE)
###################################################
## Z <- nnclean(X, k=10, plothist=TRUE)


###################################################
### code chunk number 139: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 140: 06intensity.Rnw:3129-3140
###################################################
layout(matrix(1:3, 1, 3))
opa <- par(pty="s", mar=0.4+c(3,4,0,0))
curvecolour <- if(monochrome) 1 else 3
Z <- nnclean(X, k=10, plothist=TRUE, lineargs=list(col=curvecolour))
par(opa)
opa <- par(mar=c(0,1,0,0))
plot(Z, which.marks=1, chars=c(".", "+"), cex=c(3,1), main="")
opa <- par(mar=c(0,3,0,0))
plot(Z, which.marks=2, main="")
par(opa)
layout(1)


###################################################
### code chunk number 141: 06intensity.Rnw:3162-3164 (eval = FALSE)
###################################################
## require(datasets)
## qk <- ppp(quakes$long, quakes$lat, c(164, 190), c(-39,-10))


###################################################
### code chunk number 142: 06intensity.Rnw:3169-3172
###################################################
qkD <- signif(bw.diggle(qk), 3)
qkP <- signif(bw.ppl(qk), 3)
qkS <- signif(bw.scott(qk), 3)


###################################################
### code chunk number 143: 06intensity.Rnw:3188-3189
###################################################
dq.5 <- density(qk, 0.5)


###################################################
### code chunk number 144: 06intensity.Rnw:3197-3201
###################################################
sig <- 0.5
(s <- sqrt(24/5) * sig)
ht.5 <- hextess(as.owin(qk), s)
hq.5 <- intensity(quadratcount(qk, tess=ht.5), image=TRUE)


###################################################
### code chunk number 145: 06intensity.Rnw:3207-3208
###################################################
setmargins(0.8, 0.8, 0.1, 1.8)


###################################################
### code chunk number 146: 06intensity.Rnw:3213-3221
###################################################
par(mfrow=c(1,3))
colo <- if(monochrome) grey(seq(1,0,length=128)) else NULL
plot(dq.5, main="", col=colo)
persp(dq.5, col=if(monochrome) NULL else "gold", border=NA, 
      phi=45, shade=0.75, ltheta=15, 
      xlab="longitude", ylab="latitude", zlab="intensity", main="")
plot(hq.5, main="", col=colo)
par(mfrow=c(1,1))


###################################################
### code chunk number 147: 06intensity.Rnw:3235-3236
###################################################
setmargins(0)


###################################################
### code chunk number 148: 06intensity.Rnw:3247-3249
###################################################
reload.or.compute(datafilepath("quakecluster.rda"),
                  { qc <- clusterset(qk, what="domain") })


###################################################
### code chunk number 149: 06intensity.Rnw:3251-3253
###################################################
nnq <- nnclean(qk, k=5, d=c(1,2), convergence=1e-5, 
               plothist=FALSE)


###################################################
### code chunk number 150: Fiji.Rnw:3-5
###################################################
newplot(4, 0.5)
setmargins(2,2,0,0)


###################################################
### code chunk number 151: 06intensity.Rnw:3261-3265
###################################################
showzone(solid=FALSE, annotate=FALSE, col.land=1)
plot(qc, add=TRUE, col="grey", border="grey")
if(!monochrome) plot(qk, add=TRUE, pch=3, cols="red")
annotatezone()


###################################################
### code chunk number 152: 06intensity.Rnw:3267-3272
###################################################
showzone(solid=FALSE, annotate=FALSE, col.land=1)
plot(nnq, which.marks=1, add=TRUE, 
     chars=c(".", "+"), cex=c(3, 0.7), col=grey(c(0, 0.4)), 
     legend=FALSE)
annotatezone()


###################################################
### code chunk number 153: 06intensity.Rnw:3304-3306
###################################################
X <- unmark(shapley)
Y <- sharpen(X, sigma=0.5, edgecorrect=TRUE)


###################################################
### code chunk number 154: Shapley2vert.Rnw:3-5
###################################################
newplot(8, 0.7)
setmargins(0.5)


###################################################
### code chunk number 155: 06intensity.Rnw:3311-3313
###################################################
plot(solist(X,Y), pch=".", main="", main.panel="", mar.panel=0, vsep=1,
     ncols=1)


###################################################
### code chunk number 156: 06intensity.Rnw:3331-3332 (eval = FALSE)
###################################################
## Y <- sharpen(unmark(shapley), sigma=0.5, edgecorrect=TRUE)


###################################################
### code chunk number 157: 06intensity.Rnw:3358-3359
###################################################
smo <- Smooth(longleaf, sigma=bw.smoothppp)


###################################################
### code chunk number 158: Unit2LR.Rnw:3-5
###################################################
newplot(13, 0.9)
setmargins(0,2,0,2)


###################################################
### code chunk number 159: 06intensity.Rnw:3363-3364
###################################################
setmargins(0,0,0,1.5)


###################################################
### code chunk number 160: 06intensity.Rnw:3369-3372
###################################################
plot(solist(longleaf, smo), 
     main="", main.panel="", 
     equal.scales=TRUE, mar.panel=0, hsep=1)


