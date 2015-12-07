### R code from vignette source '13gibbs'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 13gibbs.Rnw:9-11
###################################################
source("R/startup.R")
requireversion(spatstat, "1.41-1.073")


###################################################
### code chunk number 2: HybridSvensk
###################################################
set.seed(191711)
fitH <- ppm(swedishpines~polynom(x,y,2), 
            Hybrid(DiggleGatesStibbard(10), AreaInter(8)))
simH <- simulate(fitH, nsim=1)[[1]]


###################################################
### code chunk number 3: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 4: 13gibbs.Rnw:135-138
###################################################
plot(solist(swedishpines, simH),
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=1, pch=16)


###################################################
### code chunk number 5: Xhard
###################################################
## generate realisation of hard core process here
## so we can use it for various diagrams
set.seed(424242)
Xhard <- rHardcore(100, 0.1)


###################################################
### code chunk number 6: XhardDemo
###################################################
## Make a diagram showing the simulated pattern (left panel)
##   and the corresponding non-overlapping discs (right panel)
XhardX <- layered(Xhard, plotargs=list(pch=16))
XhardDiscs <- layered(Xhard %mark% 0.1, 
                      plotargs=list(markscale=1, legend=FALSE))
XhardDemo <- solist(XhardX, XhardDiscs)


###################################################
### code chunk number 7: BirthDeathDemos
###################################################
## make illustrations for birth-death transitions
## using previous simulated pattern
XX <- Xhard
WW <- Window(XX)
CC <- as.ppp(centroid.owin(WW), WW)
II <- which.min(crossdist(XX, CC))
YY <- XX[-II]
ZZ <- XX[II]
hh <- 0.1
WW <- intersect.owin(Window(XX), 
                     shift(shift(square(0.6), origin="midpoint"), XX[II]))
Window(XX) <- Window(YY) <- Window(ZZ) <- WW
XXr <- intersect.owin(dilation(XX, hh), WW)
YYr <- intersect.owin(dilation(YY, hh), WW)
Less <- layered(YYr, WW, YY, ZZ)
More <- layered(XXr, WW, YY, ZZ)
layerplotargs(Less) <- 
    list(list(border="grey", hatch=TRUE, hatchargs=list(col=grey(0.3))),
         list(),
         list(pch=16),
         list(pch=1, cex=1.5, col=grey(0.3)))
layerplotargs(More) <-
    list(list(border="grey", hatch=TRUE, hatchargs=list(col=grey(0.3))),
         list(),
         list(pch=16),
         list(pch=16))
M <- matrix(c(4,1,1,4)/5, 2, 2)
xx <- M %*% (WW$xrange)
yy <- M %*% (WW$yrange)
Right <- ppp(x=xx, y=rep(yy[2],2), window=WW)
Left  <- ppp(x=rev(xx), y=rep(yy[1],2), window=WW)
Arrows <- layered(WW, 
                  onearrow(Right, txt="birth"), 
                  onearrow(Left, txt="death"),
                  plotargs=list(list(type="n"), list(), list()))


###################################################
### code chunk number 8: 13gibbs.Rnw:211-225
###################################################
## make figure demonstrating 'forbidden zone' for hard core
# First make a yardstick, positioned so that it can be compared with the discs
xr <- WW$xrange
yr <- WW$yrange
xleft <- xr[1] - diff(xr)/6
ytarget <- mean(yr) + hh/2
ybest <- XX$y[which.min(abs(XX$y - ytarget))]
ys.h <- yardstick(xleft, ybest, 
                 xleft, ybest - hh, expression(italic(h)))
# make figure
L3 <- Less[-4]
layerplotargs(L3) <- list(list(col="grey"), list(), list(pch=3))
HardForbid <- layered(L3, ys.h, 
                      plotargs=list(list(), list(angle=90)))


###################################################
### code chunk number 9: StraussDemo
###################################################
Z <- YY[WW]
rr <- 0.11
ZS <- scanmeasure(Z, rr, dimyx=512)[WW]
m <- max(ZS)
ZS <- m - ZS
ZF <- eval.im(factor(as.integer(ZS), levels=0:m))
levels(ZF) <- LZF <- rev(c("beta", paste0("beta/", 2^(1:m))))
EZF <- parse(text=LZF)
xr <- WW$xrange
yr <- WW$yrange
xleft <- xr[1] - diff(xr)/6
ytarget <- mean(yr) + rr/2
ybest <- Z$y[which.min(abs(Z$y - ytarget))]
ys2 <- yardstick(xleft, ybest, 
                 xleft, ybest - rr, expression(italic(R)))
Str <- layered(ZF, Z, ys2,
               plotargs=list(
                   list(col=grey(sqrt((0:m)/m)), 
                        ribargs=list(las=1, labels=EZF),
                        box=TRUE),
                   list(pch=3),
                   list(angle=90, pos=2)))


###################################################
### code chunk number 10: 13gibbs.Rnw:281-288
###################################################
set.seed(424242)
halfnpix <- 10
npix <- 2 * halfnpix + 1
A <- quadrats(owin(), npix, npix)
U <- tiles(A)[[halfnpix * npix + halfnpix + 1]]
Uplus <- grow.rectangle(U, 1/npix)
X <- runifpoint(20, setminus.owin(owin(), Uplus))


###################################################
### code chunk number 11: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 12: 13gibbs.Rnw:293-294
###################################################
zeromargins()


###################################################
### code chunk number 13: 13gibbs.Rnw:298-301
###################################################
plot(Window(A), main="")
plot(X, add=TRUE, pch=16)
plot(U, add=TRUE, lwd=3)


###################################################
### code chunk number 14: Unit2LR.Rnw:3-5
###################################################
newplot(13, 0.9)
setmargins(0,2,0,2)


###################################################
### code chunk number 15: 13gibbs.Rnw:359-362
###################################################
plot(solist(HardForbid,Str),
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 16: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 17: 13gibbs.Rnw:510-511
###################################################
zeromargins()


###################################################
### code chunk number 18: 13gibbs.Rnw:515-518
###################################################
plot(XhardDemo,
     main="", main.panel="", equal.scales=TRUE, 
     mar.panel=0, hsep=1.5)


###################################################
### code chunk number 19: Plist
###################################################
set.seed(191710)
Hlist <- Plist <- rpoispp(8, nsim=12)
Rinter <- 0.1
naughty <- (sapply(Hlist, minnndist) <= Rinter)
crossout <- psp(c(0,0), c(0,1), c(1,1), c(1,0), window=square(1), check=FALSE)
Hlist[naughty] <- lapply(Hlist[naughty],
                         function(z) layered(crossout, z,
                                             plotargs=list(
                                                 list(lwd=4, col="grey"),
                                                 list(pch=16, cex=0.75))))
Hlist[!naughty] <- lapply(Hlist[!naughty], 
                          function(z) layered(z, 
                                              plotargs=list(pch=16, cex=0.75)))
## add yardstick to first panel
ysv <- yardstick(-0.1, 0.5-Rinter/2, -0.1, 0.5+Rinter/2, expression(italic(h)))
Hlist[[1]] <- layered(Hlist[[1]], ysv, 
                      plotargs=list(list(), list(pos=2, angle=90)))


###################################################
### code chunk number 20: Unit3x4y.Rnw:4-5
###################################################
setmargins(0)


###################################################
### code chunk number 21: 13gibbs.Rnw:596-600
###################################################
plot(Hlist, main="", main.panel="", 
     equal.scales=TRUE, halign=TRUE,
     pch=16, cex=0.75, 
     nrows=3, mar.panel=0.1, hsep=1, vsep=1)


###################################################
### code chunk number 22: 13gibbs.Rnw:626-633
###################################################
intensityStrauss <- function(beta, gamma, R=0.1) {
  G <- (1-gamma) * pi * R^2
  lam <- LambertW(beta * G)/G
  round(lam, 1)
}
lam8 <- intensityStrauss(8, 0)
lam100 <- intensityStrauss(100, 0)


###################################################
### code chunk number 23: birthdeathseq
###################################################
## generate spatial birth-death sequence
Hc <- 0.07
Beta <- 200
mod <- list(cif="hardcore",par=list(beta=Beta,hc=Hc), w=square(1))
X <- rmh(model=mod,
         start=list(n.start=0),
         control=list(p=0, q=0.35, nrep=1e4, nsave=10, nburn=0, track=TRUE))
Y <- attr(X, "saved")
ntran <- cumsum(attr(X, "history")$accepted)
Z <- list()
Z[[1]] <- ppp(, window=Window(Y[[1]]))
for(i in 1:9) 
  Z[[i+1]] <- Y[[min(which(ntran == (i * 50)))  %/% 10]]
names(Z) <- (0:9) * 50
Z <- as.solist(Z)


###################################################
### code chunk number 24: Unit2x5t.Rnw:4-5
###################################################
zeromargins()


###################################################
### code chunk number 25: 13gibbs.Rnw:872-876
###################################################
plot(Z, main="", 
     pch=16, cex=0.6, cex.main=0.9, font.main=1, 
     nrows=2, 
     equal.scales=TRUE, mar.panel=c(0,0,1,0), hsep=1, vsep=0.75)


###################################################
### code chunk number 26: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 27: 13gibbs.Rnw:930-931
###################################################
zeromargins()


###################################################
### code chunk number 28: 13gibbs.Rnw:935-938
###################################################
plot(solist(Less[-1], Arrows, More[-1]),
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=0.1)


###################################################
### code chunk number 29: 13gibbs.Rnw:1018-1026
###################################################
set.seed(1917)
modIH <- rmhmodel(cif="hardcore", 
                  par=list(beta=1, hc=0.05),
                  trend = function(x,y) { 500 * exp(-2*(x + y)) },
                  w=square(1))
XIH <- rmh(modIH, nrep=1e6)
DIH <- density(XIH, bw.scott)
YIH <- layered(XIH, plotargs=list(pch=16))


###################################################
### code chunk number 30: Unit2r.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 31: 13gibbs.Rnw:1033-1035
###################################################
plot(solist(YIH, DIH), main="", main.panel="", 
     equal.scales=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 32: 13gibbs.Rnw:1110-1112
###################################################
set.seed(424242)
SS <- rStrauss(100, 0.5, 0.1)


###################################################
### code chunk number 33: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 34: 13gibbs.Rnw:1117-1118
###################################################
zeromargins()


###################################################
### code chunk number 35: 13gibbs.Rnw:1122-1127
###################################################
SSS <- layered(SS, plotargs=list(pch=16))
SSM <- layered(SS %mark% 0.1, plotargs=list(markscale=1, legend=FALSE))
plot(solist(SSS, SSM), 
     main="", main.panel="", equal.scales=TRUE, 
     mar.panel=0, hsep=1.5)


###################################################
### code chunk number 36: 13gibbs.Rnw:1176-1198
###################################################
sx <- sapply(Plist, function(z, R=Rinter) { d <- pairdist(z)
                                            sum(d[row(d) > col(d)] <= R) })
gamma <- 0.5
accept <- (runif(length(Plist)) < gamma^sx)
Slist <- Plist
Slist[!accept] <- lapply(Slist[!accept],
                         function(z) layered(owin(),
                                             crossout, z,
                                             plotargs=list(
                                                 list(),
                                                 list(lwd=4, col="grey"),
                                                 list(pch=16, cex=0.75))))
Slist[accept] <- lapply(Slist[accept], 
                          function(z) layered(z, 
                                              plotargs=list(pch=16, cex=0.75)))
## add yardstick to first panel
ysvR <- yardstick(-0.1, 0.5-Rinter/2, -0.1, 0.5+Rinter/2, expression(italic(R)))
Slist[[1]] <- layered(Slist[[1]], ysvR, 
                      plotargs=list(list(), list(pos=2, angle=90)))
## main titles
px <- ifelse(sx == 0, "1", paste0("1/", 2^sx))
mains <- parse(text=paste("list(italic(s) == ", sx, ", italic(p) == ", px, ")"))


###################################################
### code chunk number 37: Unit3x4uy.Rnw:5-6
###################################################
setmargins(0, 1, 0, 0)


###################################################
### code chunk number 38: 13gibbs.Rnw:1205-1209
###################################################
plot(Slist, main="", main.panel=mains, 
     equal.scales=TRUE, halign=TRUE, 
     pch=16, cex=0.75,  panel.args=function(...) list(cex.main=0.9),
     nrows=3, mar.panel=0.1 + c(0, 1.5, 0, 0), hsep=1, vsep=1.5)


###################################################
### code chunk number 39: 13gibbs.Rnw:1231-1233
###################################################
lam8s <- intensityStrauss(8, 0.5)
lam100s <- intensityStrauss(100, 0.5)


###################################################
### code chunk number 40: 13gibbs.Rnw:1243-1257
###################################################
set.seed(111222)
rr <- 0.08
lambda <- 50
gamma <- (0:3)/3
beta <- lambda * exp(lambda * pi * rr^2 * (1 - gamma))
HSP <- list()
for(i in seq_along(gamma)) HSP[[i]] <- rStrauss(beta[i], gamma[i], rr)
HSP <- as.solist(HSP)
#ys <- yardstick(0.5-rr/2, -0.1, 0.5+rr/2, -0.1, expression(italic(h)))
HSP[[1]] <- layered(HSP[[1]], #ys, 
                    plotargs=list(list(pch=16, cex=0.5)#,
#                                  list(angle=90, pos=1)))
                                  ))
blurb <- parse(text=paste("gamma ==", sapply(gamma, simplenumber)))


###################################################
### code chunk number 41: Unit4u.Rnw:3-5
###################################################
newplot(25, 1)
zeromargins()


###################################################
### code chunk number 42: 13gibbs.Rnw:1269-1272
###################################################
plot(HSP, main="", main.panel=blurb, 
     equal.scales=TRUE, mar.panel=c(1,0,1,0), hsep=1, nrows=1, valign=TRUE,
     pch=16, size=0.5)


###################################################
### code chunk number 43: 13gibbs.Rnw:1541-1595
###################################################
set.seed(67812)
HC <- 0.15
Beta <- 50
mod <- rmhmodel(cif='hardcore', par=list(beta=Beta, hc=HC), w=square(1))
u <- data.frame(x=0.5, y=0.5)
emp <- ppp(window=square(1))
Xu <- rmh(mod, x.cond=u, start=list(x.start=emp), expand=1, nrep=1e6)
CondDemo1 <- layered(owin(),
                     Xu[1],
                     Xu[-1],
                     disc(HC, Xu[1]),
                     textstring(0.55, 0.55, "u"),
                     plotargs=list(list(lwd=2),
                                   list(pch=3, cex=1.5),
                                   list(pch=16),
                                   list(lty=2),
                                   list(font=3)))
B <- owin(c(0,1/3), c(0,1))
set.seed(424242)
zz <- rHardcore(Beta, HC, B)
Xz <- rmh(mod, x.cond=zz, start=list(x.start=emp), expand=1, nrep=1e6)
Yz <- Xz[-(1:npoints(zz))]
CondDemo2 <- layered(owin(),
                     B,
                     zz,
                     Yz,
                     dilation(zz, HC),
                     textstring(0.1, 0.5, "B"),
                     textstring(0.75, 0.15, "A"),
                     plotargs=list(list(lwd=2),
                                   list(lwd=2, lty=3),
                                   list(pch=3),
                                   list(pch=16),
                                   list(lty=2),
                                   list(font=3), list(font=3)))
CW <- owin(c(1/3, 1/3 + HC), c(0,1))
set.seed(999)
cz <- rHardcore(Beta, HC, CW)
Xc <- rmh(mod, x.cond=cz, start=list(x.start=emp), expand=1, nrep=1e6)
Yc <- Xc[-(1:npoints(cz))]
MarkovDemo <- layered(owin(),
                     CW,
                     cz,
                     Yc,
                     dilation(cz, HC),
                     textstring(0.1, 0.5, "B"),
                     textstring(0.4, 0.12, "C"),
                     textstring(0.75, 0.15, "A"),
                     plotargs=list(list(lwd=2),
                                   list(lwd=2, lty=3),
                                   list(pch=3),
                                   list(pch=16),
                                   list(lty=2),
                                   list(font=3), list(font=3), list(font=3)))


###################################################
### code chunk number 44: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 45: 13gibbs.Rnw:1600-1603
###################################################
plot(solist(CondDemo1, CondDemo2),
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=1, valign=TRUE)


###################################################
### code chunk number 46: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 47: 13gibbs.Rnw:1667-1668
###################################################
plot(MarkovDemo, main="")


###################################################
### code chunk number 48: 13gibbs.Rnw:1729-1734
###################################################
## set digits to 4 for narrower output
Digits <- 4
dopt <- options(digits=Digits)
Terse <- 3
spatstat.options(terse=Terse)


###################################################
### code chunk number 49: fitStrauss-swedishpines5
###################################################
fithom <- ppm(swedishpines~1, Strauss(r=5))
co <- round(exp(coef(fithom)), 3)


###################################################
### code chunk number 50: 13gibbs.Rnw:1767-1768
###################################################
ppm(swedishpines ~ 1, Strauss(r=5))


###################################################
### code chunk number 51: 13gibbs.Rnw:1803-1804
###################################################
ppm(swedishpines ~ polynom(x,y,2), Strauss(5))


###################################################
### code chunk number 52: 13gibbs.Rnw:1833-1835
###################################################
fit0 <- ppm(swedishpines ~ 1, Strauss(5))
coef(fit0)


###################################################
### code chunk number 53: 13gibbs.Rnw:1845-1846
###################################################
unlist(parameters(fit0))


###################################################
### code chunk number 54: fitStrauss2swp
###################################################
fit2 <- ppm(swedishpines ~ polynom(x,y,2), Strauss(5))


###################################################
### code chunk number 55: 13gibbs.Rnw:1880-1882
###################################################
Dactive <- distfun(vesicles.extra$activezone)
ppm(vesicles ~ Dactive, Strauss(40))


###################################################
### code chunk number 56: 13gibbs.Rnw:2048-2049
###################################################
ppm(redwood ~ 1, Strauss(0.1))


###################################################
### code chunk number 57: 13gibbs.Rnw:2055-2056
###################################################
ppm(redwood ~ 1, Strauss(0.1), project=TRUE)


###################################################
### code chunk number 58: 13gibbs.Rnw:2075-2076
###################################################
ppm(cells ~ 1, Strauss(0.00001))


###################################################
### code chunk number 59: 13gibbs.Rnw:2164-2168
###################################################
fit0 <- ppm(swedishpines ~ 1, Strauss(r=5))
fit2  <- update(fit0, . ~ polynom(x,y,2))
fit9 <- update(fit0, Strauss(9))
fit0fine <- update(fit0, nd=256)


###################################################
### code chunk number 60: 13gibbs.Rnw:2184-2186
###################################################
sqrt(diag(vcov(fit0)))
confint(fit0)


###################################################
### code chunk number 61: 13gibbs.Rnw:2189-2190
###################################################
coef(summary(fit0))


###################################################
### code chunk number 62: 13gibbs.Rnw:2198-2199
###################################################
vcov(fit0)


###################################################
### code chunk number 63: 13gibbs.Rnw:2240-2245
###################################################
fit2 <- ppm(swedishpines ~ polynom(x,y,2), Strauss(5))
Z <- solist(predict(fit2, type="trend"),
            predict(fit2, type="cif"),
            predict(fit2, type="intensity"))
ZZ <- as.anylist(lapply(Z, transect.im))


###################################################
### code chunk number 64: Unit3r.Rnw:3-5
###################################################
newplot(22,0.9)
setmargins(0)


###################################################
### code chunk number 65: 13gibbs.Rnw:2251-2252
###################################################
plot(Z, equal.ribbon=TRUE, main="", main.panel="", mar.panel=0, hsep=1)


###################################################
### code chunk number 66: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 67: 13gibbs.Rnw:2283-2284
###################################################
setmargins(0.5+c(2,2,0,0))


###################################################
### code chunk number 68: 13gibbs.Rnw:2288-2289
###################################################
plot(ZZ, equal.scales=TRUE, main="", main.panel="")


###################################################
### code chunk number 69: 13gibbs.Rnw:2301-2303
###################################################
scrambled <- runifpoint(ex=swedishpines)
ll <- predict(fit2, type="cif", X=scrambled)


###################################################
### code chunk number 70: 13gibbs.Rnw:2314-2315
###################################################
inten <- intensity(fit0)


###################################################
### code chunk number 71: 13gibbs.Rnw:2325-2326
###################################################
intensity(fit0)


###################################################
### code chunk number 72: 13gibbs.Rnw:2353-2354
###################################################
fitin(fit2)


###################################################
### code chunk number 73: 13gibbs.Rnw:2368-2369
###################################################
as.interact(fit2)


###################################################
### code chunk number 74: simStrauss2
###################################################
s <- simulate(fit2, nsim=100)


###################################################
### code chunk number 75: Unit4.Rnw:3-5
###################################################
newplot(25, 1)
zeromargins()


###################################################
### code chunk number 76: 13gibbs.Rnw:2392-2394
###################################################
plot(s[1:4], nrows=1, equal.scales=TRUE,
     main = "", main.panel="", pch=16, hsep=1, mar.panel=0)


###################################################
### code chunk number 77: 13gibbs.Rnw:2419-2423
###################################################
npoints(swedishpines)
np <- sapply(s, npoints)
mean(np)
sd(np)


###################################################
### code chunk number 78: 13gibbs.Rnw:2442-2443 (eval = FALSE)
###################################################
## E0 <- envelope(fit0, Lest, nsim=39)


###################################################
### code chunk number 79: 13gibbs.Rnw:2485-2486
###################################################
step(fit2, trace=0)


###################################################
### code chunk number 80: 13gibbs.Rnw:2517-2519
###################################################
fit2P <- update(fit2, Poisson())
anova(fit2P, fit2, test="LR")


###################################################
### code chunk number 81: 13gibbs.Rnw:2522-2523
###################################################
anova(fit0, fit2, test="LR")


###################################################
### code chunk number 82: fv4x2.Rnw:3-5
###################################################
newplot(5, 1)
setmargins(0.5+c(3,3,2,1))


###################################################
### code chunk number 83: 13gibbs.Rnw:2637-2638
###################################################
setmargins(0.5+c(2,2,1,0))


###################################################
### code chunk number 84: 13gibbs.Rnw:2642-2714
###################################################
doit <- function(nama, expr, ...) {
    height <- 1.3
    width <- 1
    plot(c(-0.25, width), c(-0.15,height), type="n", main="",
         xlab="", ylab="", axes=FALSE)
    title(main=nama, cex=1.1, font=2, line=0.5)
    AXEX <- 1.2
    text(0.9*width, 0.15, expression(bolditalic(d)), cex=AXEX)
    ## remember to reconcile the next line with \pairpot
    text(-0.15, height, expression(bolditalic(c(d))), cex=AXEX)
    plot(onearrow(ppp(c(0,0), c(0, height), window=square(height))), 
         retract=0, headfraction=0.15, col.head=1, add=TRUE)
    axis(2, at=c(0,1), tick=TRUE, pos=0, las=1, cex.axis=AXEX)
    plot(onearrow(ppp(c(0,width), c(0, 0), window=square(width))), 
         retract=0, headfraction=0.15, col.head=1, add=TRUE)
    axis(1, ..., tick=TRUE, pos=0, cex.axis=AXEX)
    segments(0,1,1,1, lty=3, lwd=2)
    eval(substitute(curve(E, lwd=3, col=1, add=TRUE, n=513, from=0),
                    list(E=substitute(expr))))
}
par(mfrow=c(2, 4))
doit("Hard Core",
     as.integer(x > 0.5),
     at=c(0, 0.5), labels=expression(0, italic(h)))
doit("Strauss",
     ifelse(x > 0.5, 1, 0.5),
     at=c(0, 0.5), labels=expression(0, italic(R)))
# strauss-hardcore
doit("Strauss-Hard Core",
     ifelse(x <= 0.3, 0, ifelse(x <= 0.6, 0.5, 1)),
     at=c(0, 0.3, 0.6), labels=expression(0, italic(h), italic(R)))
# pairpiece
doit("Piecewise constant",
     c(0.15, 0.4, 0.2, 0.6, 1)[findInterval(x, (0:5)/5, TRUE, TRUE)],
     at=(0:4)/5,
     labels=expression(0, italic(r[1]), italic(r[2]), italic(r[3]),
                       italic(r[4])))
# diggle-gratton
delta <- 0.3
rho <- 0.7
kappa <- 1.8
doit("Diggle-Gratton",
     ifelse(x <= delta, 0,
            ifelse(x >= rho, 1,
                   ((x-delta)/(rho-delta))^kappa)),
     at=c(0, delta, rho),
     labels=expression(0, delta, rho))
# diggle-gates-stibbard
rho <- 0.7
doit("Diggle-Gates-Stibbard",
     ifelse(x >= rho, 1,
            sin((pi/2) * x/rho)^2),
     at=c(0, rho),
     labels=expression(0, rho))
# softcore
sigma <- 0.3
kappa <- 0.5
doit("Soft Core",
     exp(-(sigma/x)^(2/kappa)),
     at=c(0, sigma),
     labels=expression(0, sigma))
# Fiksel
aa <- -3
kappa <- 4
h <- 0.25
r <- 0.75
doit("Fiksel",
     ifelse(x <= h, 0,
            ifelse(x >= r, 1,
                   exp( aa * exp(-kappa * x)))),
     at=c(0, h, r),
     labels=expression(0, h, r))


###################################################
### code chunk number 85: 13gibbs.Rnw:2724-2732
###################################################
set.seed(1000)
Xdgs <- rStraussHard(200, 0.5, 0.08, 0.04)
fakefit <- ppm(Xdgs ~ 1, StraussHard(0.08, 0.04))
cifdgs <- predict(fakefit, type="cif", ngrid=1024,
                  new.coef=log(c(200, 0.5)))
Xdgs <- layered(Xdgs, plotargs=list(list(pch=16)))
cifXdgs <- layered(cifdgs, plotargs=list(list(col=grey(seq(0.2,1,length=128)))))
Zdgs <- solist(Xdgs, cifXdgs)


###################################################
### code chunk number 86: Unit2r.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 87: 13gibbs.Rnw:2738-2739
###################################################
plot(Zdgs, main="", main.panel="", mar.panel=0, hsep=1, equal.scales=TRUE)


###################################################
### code chunk number 88: 13gibbs.Rnw:2983-2985
###################################################
mod <- ppm(swedishpines ~ polynom(x,y,2), PairPiece(1:15))
f <- fitin(mod)


###################################################
### code chunk number 89: 13gibbs.Rnw:3014-3016
###################################################
rr <- data.frame(r=seq(3,14,by=0.05))
p <- profilepl(rr, Strauss, swedishpines~polynom(x,y,2), aic=TRUE)


###################################################
### code chunk number 90: 13gibbs.Rnw:3018-3019
###################################################
p


###################################################
### code chunk number 91: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 92: 13gibbs.Rnw:3032-3034
###################################################
plot(anylist(f, p), main="", main.panel="", 
     mar.panel=0.2+c(4,4,0,1))


###################################################
### code chunk number 93: 13gibbs.Rnw:3053-3054
###################################################
as.ppm(p)


###################################################
### code chunk number 94: 13gibbs.Rnw:3124-3129
###################################################
set.seed(191919)
ai <- function(beta, eta, r=0.07) 
  rmhmodel(cif='areaint', par=list(beta=beta, eta=eta, r=r), w=square(1))
XA.02 <- rmh(ai(250, 0.02))
XA50  <- rmh(ai(5, 50))


###################################################
### code chunk number 95: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 96: 13gibbs.Rnw:3135-3139
###################################################
plot(solist(XA.02, XA50), 
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=1,
     pch=16)


###################################################
### code chunk number 97: 13gibbs.Rnw:3184-3207
###################################################
## Make synthetic example for explaining Area Interaction 
W <- as.owin(swedishpines)
x <- c(28,29,55,60,66)
y <- c(70,38,32,72,59)
X <- ppp(x=x,y=y, window = W)
u <- list(x=48,y=50)
u <- as.ppp(u, W)
rad <- 14
Xplusr <- dilation(X, rad)
uplusr <- disc(rad, u)
ovlap <- intersect.owin(uplusr, Xplusr)
AIdemo <- layered(W, 
                  ovlap,
                  Xplusr,
                  uplusr,
                  X,
                  u)
layerplotargs(AIdemo) <- list(list(),
                              list(col="darkgrey", border=NA),
                              list(lwd=2),
                              list(lwd=2, lty=2),
                              list(pch=16),
                              list(pch=3))


###################################################
### code chunk number 98: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 99: 13gibbs.Rnw:3214-3215
###################################################
plot(AIdemo, main="")


###################################################
### code chunk number 100: 13gibbs.Rnw:3275-3277 (eval = FALSE)
###################################################
## fitSA <- ppm(swedishpines ~ polynom(x,y,2), AreaInter(4))
## cifSA <- predict(fitSA, type = "cif", ngrid=512)


###################################################
### code chunk number 101: cifSA
###################################################
fitSA <- ppm(swedishpines~polynom(x,y,2), AreaInter(4))
cifSA <- predict(fitSA, type = "cif", ngrid=if(draftversion) 128 else 512)
cifSAdemo <- layered(cifSA, swedishpines,
                     plotargs=list(list(log=TRUE, ribargs=list(las=1)), 
                       list(col="white", pch=3)))
intSA <- fitin(fitSA)


###################################################
### code chunk number 102: 13gibbs.Rnw:3287-3288
###################################################
fitSA


###################################################
### code chunk number 103: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 104: 13gibbs.Rnw:3306-3307
###################################################
plot(intSA, main="", legend=FALSE)


###################################################
### code chunk number 105: UnitR.Rnw:3-6
###################################################
newplot(9, 0.7)
zeromargins() # strip all margins
setmargins(0.1 + c(0,0,0,2)) # back off


###################################################
### code chunk number 106: 13gibbs.Rnw:3311-3312
###################################################
setmargins(0,0,0,3)


###################################################
### code chunk number 107: 13gibbs.Rnw:3314-3315
###################################################
plot(cifSAdemo, main="")


###################################################
### code chunk number 108: 13gibbs.Rnw:3377-3384
###################################################
set.seed(422442)
gs <- function(beta, gamma, r=0.07, sat=2) {
  rmhmodel(cif='geyer', par=list(beta=beta, gamma=gamma, sat=sat, r=r), 
           w=square(1))
}
XG.5 <- rmh(gs(300, 0.5))
XG2  <- rmh(gs(20, 2))


###################################################
### code chunk number 109: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 110: 13gibbs.Rnw:3391-3395
###################################################
plot(solist(XG.5, XG2), 
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=1,
     pch=16)


###################################################
### code chunk number 111: 13gibbs.Rnw:3436-3437
###################################################
ppm(redwood ~ 1, Geyer(r=0.05, sat=2))


###################################################
### code chunk number 112: 13gibbs.Rnw:3441-3444
###################################################
df <- expand.grid(r=seq(0.02, 0.1, by=0.001), sat=c(1,2))
pG <- profilepl(df, Geyer, redwood ~ 1, correction="translate", 
                aic=TRUE)


###################################################
### code chunk number 113: 13gibbs.Rnw:3446-3447
###################################################
as.ppm(pG)


###################################################
### code chunk number 114: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 115: 13gibbs.Rnw:3453-3458
###################################################
plot(pG, main="", 
     lwd=2, 
     lty=ifelse(sat == 2, 1, 2), ## undocumented feature of plot.profilepl
     pos=2, # text to left of peaks
     lwd.opt=2, col.opt=1, tag=FALSE)


###################################################
### code chunk number 116: TripletsCalc
###################################################
set.seed(19501973)
tripmod <- function(gamma) {
  rmhmodel(cif="triplets",par=list(beta=200,gamma=gamma,r=0.1), w=square(1))
}
X0 <- rmh(tripmod(0))
X.5 <- rmh(tripmod(0.5))
fitT0 <- ppm(X0 ~ 1, Triplets(0.1))
cifT0 <- predict(fitT0, type="cif", ngrid=256, new.coef=log(c(200, 1e-16)))
facT0 <- eval.im(factor(cifT0 > 100))
faclab <- expression(0, beta)
CifT0 <- layered(facT0, plotargs=list(list(col=grey(c(0.2, 0.9)),
                                           box=TRUE,
                                           ribargs=list(las=1, labels=faclab))))


###################################################
### code chunk number 117: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 118: 13gibbs.Rnw:3551-3552
###################################################
setmargins(0,0,0,1.5)


###################################################
### code chunk number 119: 13gibbs.Rnw:3556-3559
###################################################
plot(solist(X0, X.5, CifT0), 
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 120: 13gibbs.Rnw:3588-3590
###################################################
clo <- closepairs(redwoodfull, 0.1)
crx <- with(clo, psp(xi, yi, xj, yj, window=Window(redwoodfull)))


###################################################
### code chunk number 121: 13gibbs.Rnw:3596-3597
###################################################
redcon <- connected(redwoodfull, 0.1)


###################################################
### code chunk number 122: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 123: 13gibbs.Rnw:3604-3609
###################################################
redcross <- layered(redwoodfull, crx)
layerplotargs(redcon) <- list(legend=FALSE)
plot(solist(redcross, redcon),
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 124: 13gibbs.Rnw:3646-3647
###################################################
hcg <- hclust(dist(coords(gordon)), "single")


###################################################
### code chunk number 125: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 126: 13gibbs.Rnw:3652-3653
###################################################
setmargins(0.2,3,0,0)


###################################################
### code chunk number 127: 13gibbs.Rnw:3657-3658
###################################################
plot(hcg, main="", xlab="", sub="", labels=FALSE)


###################################################
### code chunk number 128: 13gibbs.Rnw:3667-3672
###################################################
set.seed(191919)
f <- function(X) { coef(ppm(X ~ 1, Concom(2), rbord=0))[["Interaction"]] }
Xsims <- rpoispp(intensity(gordon), win=Window(gordon), nsim=99)
fXsims <- sapply(Xsims, f)
pv <- (1+sum(f(gordon) <= fXsims))/100


###################################################
### code chunk number 129: 13gibbs.Rnw:3685-3686 (eval = FALSE)
###################################################
## plot(hclust(dist(coords(gordon)), "single"))


###################################################
### code chunk number 130: 13gibbs.Rnw:3699-3700
###################################################
ppm(gordon ~ 1, Concom(2), rbord=0)


###################################################
### code chunk number 131: 13gibbs.Rnw:3706-3711
###################################################
etahat <- function(X) { 
  mod <- ppm(X ~ 1, Concom(2), rbord=0)
  return(parameters(mod)$eta)
}
(etaobs <- etahat(gordon))


###################################################
### code chunk number 132: 13gibbs.Rnw:3713-3714
###################################################
set.seed(898292)


###################################################
### code chunk number 133: ConcomMCtest
###################################################
Xsims <- rpoispp(ex=gordon, nsim=99)
etasim <- sapply(Xsims, etahat)


###################################################
### code chunk number 134: 13gibbs.Rnw:3720-3721
###################################################
(pval <- mean(etaobs <= c(etaobs, etasim)))


###################################################
### code chunk number 135: 13gibbs.Rnw:3871-3872 (eval = FALSE)
###################################################
## Hybrid(Strauss(0.1), Geyer(0.2, 3))


###################################################
### code chunk number 136: 13gibbs.Rnw:3883-3886
###################################################
M <- Hybrid(H=Hardcore(), G=Geyer(0.2, 3))
fit <- ppm(redwood ~ 1, M, correction="translate")
fit 


###################################################
### code chunk number 137: 13gibbs.Rnw:3927-3929
###################################################
Mplus <- Hybrid(fit, g=Geyer(0.1,1))
update(fit, Mplus)


###################################################
### code chunk number 138: 13gibbs.Rnw:3935-3939 (eval = FALSE)
###################################################
## fit2 <- ppm(swedishpines ~ 1, 
##             Hybrid(DG=DiggleGratton(2,10), S=Strauss(5)))
## plot(fitin(fit2))
## plot(fitin(fit2), separate=TRUE, mar.panel=rep(4,4))


###################################################
### code chunk number 139: 13gibbs.Rnw:4029-4031
###################################################
fit <- ppm(swedishpines ~ 1, Strauss(8))
Y   <- rmh(fit)


###################################################
### code chunk number 140: 13gibbs.Rnw:4072-4076
###################################################
mo <- list(cif="strauss", 
           par=list(beta=2,gamma=0.2,r=0.7), w=square(10))
X <- rmh(model=mo, 
         start=list(n.start=42), control=list(nrep=1e6))


###################################################
### code chunk number 141: 13gibbs.Rnw:4096-4098 (eval = FALSE)
###################################################
## mo <- rmhmodel(cif="strauss", 
##                par=list(beta=2,gamma=0.2,r=0.7), w=square(10))


###################################################
### code chunk number 142: 13gibbs.Rnw:4112-4116 (eval = FALSE)
###################################################
## rmhmodel(cif = c('hardcore', 'strauss'),
##          par = list(list(beta = 10, hc = 0.03), 
##                     list(beta = 1, gamma = 0.5, r = 0.07)),
##          w   = square(1))


###################################################
### code chunk number 143: 13gibbs.Rnw:4164-4165
###################################################
X4 <- rmh(model=mo, nrep=1e4)


###################################################
### code chunk number 144: 13gibbs.Rnw:4269-4272
###################################################
fit <- ppm(swedishpines ~ 1, Strauss(8))
Y <- rmh(fit, track=TRUE)
h <- attr(Y, "history")


###################################################
### code chunk number 145: 13gibbs.Rnw:4274-4276
###################################################
with(h, mean(accepted))
with(h, prop.table(table(proposaltype, accepted), 1))


###################################################
### code chunk number 146: CopperFitS
###################################################
dlin <- distfun(copper$SouthLines)
copfit <- ppm(copper$SouthPoints ~ dlin, Geyer(1, 1))


###################################################
### code chunk number 147: cdftestMCMC
###################################################
coptest <- cdf.test(copfit, dlin, nsim=39)


###################################################
### code chunk number 148: 13gibbs.Rnw:4608-4609
###################################################
coptest


###################################################
### code chunk number 149: SwedFit
###################################################
swedfit <- ppm(swedishpines ~ polynom(x,y,2), Strauss(9))


###################################################
### code chunk number 150: SwedEnv
###################################################
swedenv <- envelope(swedfit, Lest, nsim=39, global=TRUE, 
                    savepatterns=TRUE)


###################################################
### code chunk number 151: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 152: 13gibbs.Rnw:4645-4646
###################################################
plot(swedenv, . - r ~ r, main="", legend=FALSE)


###################################################
### code chunk number 153: 13gibbs.Rnw:4763-4764
###################################################
swedR <- residuals(swedfit)


###################################################
### code chunk number 154: Unit2LRboth.Rnw:4-6
###################################################
newplot(13, 1)
setmargins(0,2,0,2)


###################################################
### code chunk number 155: 13gibbs.Rnw:4774-4777
###################################################
plot(solist(swedR, Smooth(swedR)),
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=c(0,1,0,1), hsep=1)


###################################################
### code chunk number 156: 13gibbs.Rnw:4852-4853
###################################################
swedI <- residuals(swedfit, type="inverse")


###################################################
### code chunk number 157: 13gibbs.Rnw:4863-4864
###################################################
swedE <- eem(swedfit)


###################################################
### code chunk number 158: 13gibbs.Rnw:4872-4873
###################################################
swedP <- residuals(swedfit, type="pearson")


###################################################
### code chunk number 159: Unit2LRboth.Rnw:4-6
###################################################
newplot(13, 1)
setmargins(0,2,0,2)


###################################################
### code chunk number 160: 13gibbs.Rnw:4878-4881
###################################################
plot(solist(swedI, swedP),
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=c(0,1,0,1), hsep=1)


###################################################
### code chunk number 161: 13gibbs.Rnw:4981-4982 (eval = FALSE)
###################################################
## diagnose.ppm(swedfit, type="Pearson", envelope=TRUE, nsim=39)


###################################################
### code chunk number 162: 13gibbs.Rnw:4985-4986 (eval = FALSE)
###################################################
## diagnose.ppm(swedfit, type="Pearson", envelope=swedenv, nsim=39)


###################################################
### code chunk number 163: 13gibbs.Rnw:4989-4991
###################################################
DS <- diagnose.ppm(swedfit, type="Pearson", 
                   plot.it=FALSE, envelope=swedenv, nsim=39)


###################################################
### code chunk number 164: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 165: 13gibbs.Rnw:4996-4997
###################################################
setmargins(1)


###################################################
### code chunk number 166: 13gibbs.Rnw:5001-5002
###################################################
plot(DS, main="")


###################################################
### code chunk number 167: 13gibbs.Rnw:5037-5039
###################################################
SWP <- Smooth(residuals(swedfit, type="Pearson"))
2/(2 * sqrt(pi) * attr(SWP, "sigma"))


###################################################
### code chunk number 168: 13gibbs.Rnw:5052-5053
###################################################
coppar <- parres(copfit, dlin)


###################################################
### code chunk number 169: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 170: 13gibbs.Rnw:5075-5076
###################################################
plot(coppar, main="", legend=FALSE)


###################################################
### code chunk number 171: 13gibbs.Rnw:5131-5133
###################################################
set.seed(191919)
q9 <- qqplot.ppm(swedfit, nsim=39, plot.it=FALSE)


###################################################
### code chunk number 172: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 173: 13gibbs.Rnw:5137-5138
###################################################
setmargins(0.5+c(4,3,0,0))


###################################################
### code chunk number 174: 13gibbs.Rnw:5142-5143
###################################################
plot(q9, main="", monochrome=monochrome, pch=3)


###################################################
### code chunk number 175: 13gibbs.Rnw:5188-5189
###################################################
KresSwed <- Kres(swedfit, correction="iso")


###################################################
### code chunk number 176: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 177: 13gibbs.Rnw:5196-5197
###################################################
plot(KresSwed, main="",  shade=c("ihi", "ilo"), legend=FALSE)


###################################################
### code chunk number 178: chorleyDfun
###################################################
## Code block repeated from Chapter 11
## squared distance to incinerator
d2incin <- function(x, y, xincin=354.5, yincin=413.6) {
  (x - xincin)^2 + (y - yincin)^2
}
## Diggle's model of elevated risk as function of distance
fincin <- function(d2, alpha, beta) {
  1 + alpha * exp( - beta * d2)
}
## elevated risk as spatial covariate
raisin <- function(x,y, alpha, beta) {
  fincin(d2incin(x,y), alpha, beta)
}


###################################################
### code chunk number 179: morescore
###################################################
## Code block repeated from Chapter 11
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
### code chunk number 180: moreHessian
###################################################
## Code block repeated from Chapter 11
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
### code chunk number 181: chorley
###################################################
lung <- split(chorley)$lung
larynx <- split(chorley)$larynx
smo <- density(lung, sigma=0.15, eps=0.1)
smo <- eval.im(pmax(smo, 1e-10))


###################################################
### code chunk number 182: chorleyDGfit
###################################################
rGey <- 0.25
chorleyDGfit <- ippm(larynx ~ offset(log(smo) + log(raisin)) , 
                     Geyer(rGey, 1), eps = 0.5, iScore=Zscore, 
                     start=list(alpha=20, beta=1), 
                     nlm.args=list(steptol=0.001))


###################################################
### code chunk number 183: 13gibbs.Rnw:5360-5362
###################################################
unlist(parameters(chorleyDGfit))
chorleyDGopt <- parameters(chorleyDGfit)[c("alpha", "beta")]


###################################################
### code chunk number 184: chorleyDGcalc
###################################################
chorleyDGlev <- leverage(chorleyDGfit, 
                iScore=Zscore, iHessian=Zhess, iArgs=chorleyDGopt)
chorleyDGinf <- influence(chorleyDGfit, 
                iScore=Zscore, iHessian=Zhess, iArgs=chorleyDGopt)
chorleyDGdfb <- dfbetas(chorleyDGfit, 
                iScore=Zscore, iHessian=Zhess, iArgs=chorleyDGopt)


###################################################
### code chunk number 185: 13gibbs.Rnw:5374-5388
###################################################
spatstat.options(image.colfun=greytoblack)
incin <- as.ppp(chorley.extra[["incin"]], W = Window(chorley))
chorleyDGlevDemo <- 
  layered(chorleyDGlev,
          Window(chorley),
          incin,
          plotargs=list(list(show.all=FALSE, ribbon=TRUE, ribside="bottom"),
                        list(),
                        list(pch=10, col="white", etch=TRUE)))
chorleyDGinfDemo <- 
  layered(chorleyDGinf,
          incin,
          plotargs=list(list(maxsize=3.0, lwd=3, leg.side="bottom"),
                        list(pch=10, col="white", etch=TRUE)))


###################################################
### code chunk number 186: Chorley2b.Rnw:3-5
###################################################
newplot(18, 0.8)
setmargins(3,0,0,0)


###################################################
### code chunk number 187: 13gibbs.Rnw:5394-5397
###################################################
plot(solist(chorleyDGlevDemo, chorleyDGinfDemo),
     main="", main.panel="", mar.panel=c(1.2,0,0,0), hsep=1, 
     nrows=1, equal.scales=TRUE)


###################################################
### code chunk number 188: 13gibbs.Rnw:5417-5418
###################################################
spatstat.options(image.colfun=blacktowhite)


###################################################
### code chunk number 189: augmentchorleyDG
###################################################
## This code helps to accelerate the plotting 
chorleyDGdfb <- augment.msr(chorleyDGdfb)


###################################################
### code chunk number 190: Chorley2x2LR.Rnw:3-5
###################################################
newplot(20,1)
setmargins(0.5)


###################################################
### code chunk number 191: chorleyDGdfb
###################################################
white134 <- function(i) { 
  if(i==2) list(cols="black") else
  list(cols="white", leg.args=list(cols="black")) 
}
plot(chorleyDGdfb, 
     panel.end=as.owin(chorley), 
     panel.args=white134, lwd=2, box=FALSE,
     mar.panel=0, hsep=2, vsep=1, maxsize=2.5,
     main="")


###################################################
### code chunk number 192: chorleyDGsmooth
###################################################
chorleyDGdfbsmo <- Smooth(chorleyDGdfb, sigma=0.8)


###################################################
### code chunk number 193: Chorley2x2R.Rnw:3-5
###################################################
newplot(20,0.95)
setmargins(0, 0.5, 0, 4)


###################################################
### code chunk number 194: chorleyDGdfbsmo
###################################################
plot(chorleyDGdfbsmo, 
     panel.end=as.owin(chorley),
     main="", box=FALSE, 
     mar.panel=0, hsep=2, vsep=1)


###################################################
### code chunk number 195: 13gibbs.Rnw:5492-5493
###################################################


###################################################
### code chunk number 196: 13gibbs.Rnw:5495-5511
###################################################
## if(!file.exists("data/RedGey.rda")) {
##   # get bandwidth value 
##   if(!file.exists("data/RedBW.rda")) {
##    NOT YET RELEASED IN SPATSTAT
##     RedBW <- bw.locppm(redwoodfull, ~1, Geyer(0.05, 2), 
##                       correction="isotropic", 
##                       srange=c(0.06, 0.4), ns=32,
##                       verbose=FALSE)
##    save(RedBW, file="data/RedBW.rda", compress=TRUE)
##  } else load("data/RedBW.rda")
##  # fit model
##    NOT YET RELEASED IN SPATSTAT
##  RedGey <- locppm(redwoodfull, ~1, Geyer(0.05, 2), sigma=RedBW,
##                   correction="isotropic", 
##                   vcalc="full", locations="fine", 
##                   verbose=FALSE)
##  save(RedGey, file="data/RedGey.rda", compress=TRUE)
## } else load("data/RedGey.rda")


###################################################
### code chunk number 197: 13gibbs.Rnw:5536-5538 (eval = FALSE)
###################################################
##    NOT YET RELEASED IN SPATSTAT
## RedGey <- locppm(redwoodfull ~ 1, Geyer(0.05, 2), sigma=bw.locppm,
##                  correction="isotropic")


###################################################
### code chunk number 198: UnitR.Rnw:3-6
###################################################
newplot(9, 0.7)
zeromargins() # strip all margins
setmargins(0.1 + c(0,0,0,2)) # back off


###################################################
### code chunk number 199: 13gibbs.Rnw:5550-5552
###################################################
##    NOT YET RELEASED IN SPATSTAT
## plot(RedGey, main="", which=2, 
##     style="imagecontour", contourargs=list(col="white"))


###################################################
### code chunk number 200: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 201: 13gibbs.Rnw:6320-6321 (eval = FALSE)
###################################################
## ppm(X ~ terms, interaction)


###################################################
### code chunk number 202: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 203: 13gibbs.Rnw:6398-6402
###################################################
## reload.or.compute(datafilepath("ppmStraussSimdatHO.rda"), 
##                   { fit <- ppm(simdat, ~1, Strauss(r=0.275), method="ho") })
set.seed(424242)
fit <- ppm(simdat~1, Strauss(r=0.275), method="ho")


###################################################
### code chunk number 204: 13gibbs.Rnw:6404-6405 (eval = FALSE)
###################################################
## fit <- ppm(simdat ~ 1, Strauss(r=0.275), method="ho")


###################################################
### code chunk number 205: 13gibbs.Rnw:6407-6409
###################################################
fit
vcov(fit)


###################################################
### code chunk number 206: SwedFit
###################################################
p.mean <- rep(0,7)
p.var <- diag(c(rep(10000,6), 0.001))
swedfitVB <- ppm(swedishpines ~ polynom(x,y,2), Strauss(9), 
                 method = "VBlogi", 
                 prior.mean = p.mean, prior.var = p.var)


###################################################
### code chunk number 207: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 208: 13gibbs.Rnw:6650-6651 (eval = FALSE)
###################################################
## model <- dppGauss(lambda=100, alpha=0.05, d=2)


###################################################
### code chunk number 209: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 210: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 211: 13gibbs.Rnw:6663-6664 (eval = FALSE)
###################################################
## simulate(model)


###################################################
### code chunk number 212: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 213: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 214: 13gibbs.Rnw:6671-6673 (eval = FALSE)
###################################################
## X <- residualspaper[["Fig1"]]
## jfit <- dppm(X ~ polynom(x,y,3), dppGauss())


###################################################
### code chunk number 215: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


