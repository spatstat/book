### R code from vignette source '12clustercox'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 12clustercox.Rnw:9-18
###################################################
source("R/startup.R")
source("R/rSpecialMC.R")
source("R/rSpecialNS.R")
requireversion(spatstat, "1.41-1.016")
require(RandomFields)
Digits <- 4
options(digits=Digits)
Terse <- 3
spatstat.options(terse=Terse)


###################################################
### code chunk number 2: Unit4.Rnw:3-5
###################################################
newplot(25, 1)
zeromargins()


###################################################
### code chunk number 3: 12clustercox.Rnw:46-53
###################################################
pa <- function(i) { 
  if(i == 3) list(pch=".", cex=3) else list(pch=16, cex=0.65)
}
plot(solist(redwood, split(bramblecanes)[[1]], rescale(bei, 500)),
     main="", main.panel="",
     mar.panel=0, hsep=1, equal.scales=TRUE,
     panel.args=pa)


###################################################
### code chunk number 4: 12clustercox.Rnw:195-196 (eval = FALSE)
###################################################
## kppm(redwood ~ 1, "Thomas")


###################################################
### code chunk number 5: 12clustercox.Rnw:203-204 (eval = FALSE)
###################################################
## kppm(bei ~ grad, "LGCP", model="exp", data=bei.extra)


###################################################
### code chunk number 6: 12clustercox.Rnw:225-230
###################################################
set.seed(1492)
X0 <- rLGCP("exp", 4, var=0.5, scale=.2, saveLambda=TRUE)
Lambda <- blur(attr(X0, "Lambda"), 0.06)
X <- rpoispp(Lambda)
Lmax <- max(Lambda)


###################################################
### code chunk number 7: 12clustercox.Rnw:233-238
###################################################
CoxParts <- layered(Lambda=Lambda, X=X,
                    plotargs=list(list(.plot="contour",
                                       drawlabels=FALSE),
                                  list(pch=16)))
CoxDemo <- solist(CoxParts[1], CoxParts, CoxParts[2])


###################################################
### code chunk number 8: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 9: 12clustercox.Rnw:243-245
###################################################
plot(CoxDemo, main="", main.panel="", 
     equal.scales=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 10: 12clustercox.Rnw:266-271
###################################################
set.seed(192)
Xc <- replicate(8,
                rpoispp(rexp(1,1/100)),
                simplify=FALSE)
Xc <- as.solist(Xc)


###################################################
### code chunk number 11: Unit2x4.Rnw:4-5
###################################################
zeromargins()


###################################################
### code chunk number 12: 12clustercox.Rnw:287-290
###################################################
plot(Xc, main="", main.panel="", 
     nrows=2, equal.scales=TRUE, pch=16, 
     mar.panel=0, hsep=1, vsep=1)


###################################################
### code chunk number 13: 12clustercox.Rnw:371-372
###################################################
set.seed(1026787)


###################################################
### code chunk number 14: 12clustercox.Rnw:374-375
###################################################
Y <- rnoise(runif, square(1), max=100)


###################################################
### code chunk number 15: 12clustercox.Rnw:379-380
###################################################
Z <- Smooth(Y, sigma=0.07, normalise=TRUE, bleed=FALSE)


###################################################
### code chunk number 16: 12clustercox.Rnw:384-385
###################################################
X <- rpoispp(Z)


###################################################
### code chunk number 17: Unit3T.Rnw:3-5
###################################################
newplot(13.5,0.9)
setmargins(0)


###################################################
### code chunk number 18: 12clustercox.Rnw:392-398
###################################################
pa <- function(i) {
  if(i == 3) list(pch=16) else list(ribside="bottom")
}
plot(solist(Y,Z,X), 
     main="", main.panel="", nrows=1, panel.args=pa,
     equal.scales=TRUE, mar.panel=c(1,0,0,0), hsep=1, valign=TRUE)


###################################################
### code chunk number 19: 12clustercox.Rnw:429-438
###################################################
MosaicParts <- local({
  set.seed(6)
  P <- rpoislinetess(4)
  set.seed(1666)
  logLambda <- rMosaicField(P, rnorm, dimyx=512, rgenargs=list(mean=4, sd=1))
  Lambda <- exp(logLambda)
  X <- rpoispp(Lambda)
  solist(P=P, logLambda=logLambda, Lambda=Lambda, X=X)
})


###################################################
### code chunk number 20: Unit3T.Rnw:3-5
###################################################
newplot(13.5,0.9)
setmargins(0)


###################################################
### code chunk number 21: 12clustercox.Rnw:444-451
###################################################
pa <- function(i) {
  list(list(lwd=2), list(ribside="bottom"), list(pch=16))[[i]]
}
plot(MosaicParts[c("P", "logLambda", "X")],
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=c(1,0,0,0), hsep=1, 
     panel.args=pa, valign=TRUE)


###################################################
### code chunk number 22: 12clustercox.Rnw:525-527
###################################################
set.seed(10101)
LGCPlist <- rLGCP("exp", 4, var=0.5, scale=0.2, nsim=8)


###################################################
### code chunk number 23: Unit2x4.Rnw:4-5
###################################################
zeromargins()


###################################################
### code chunk number 24: 12clustercox.Rnw:533-536
###################################################
plot(LGCPlist, main="", main.panel="", nrows=2, 
     equal.scales=TRUE, mar.panel=0, hsep=1, vsep=1,
     pch=16, cex=0.75)


###################################################
### code chunk number 25: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 26: 12clustercox.Rnw:631-633
###################################################
curve(dlnorm(x, 4, 1), to=250, n=2048,
      ylab=expression(f(x)), xlab=expression(x), main="", lwd=2)


###################################################
### code chunk number 27: 12clustercox.Rnw:683-702
###################################################
RFDemo <- local({
  A1 <- MosaicParts[["logLambda"]]
  S <- A1 - 4
  B <- rMosaicField(rpoislinetess(4), 
                    rnorm, dimyx=512, rgenargs=list(mean=0, sd=1))
  S <- S + B
  A2 <- 4 + S/sqrt(2)
  # note: this calculation seems to be too compute-intensive
  # to perform using lapply/Reduce, so we do it one step at a time
  for(i in 3:10) {
    B <- rMosaicField(rpoislinetess(4), 
                      rnorm, dimyx=512, rgenargs=list(mean=0, sd=1))
    S <- S + B
  }
  A10 <- 4 + S/sqrt(10)
  X <- rLGCP("exp", 4, var=1, scale=.2, saveLambda=TRUE)
  Ainf <- log(attr(X, "Lambda"))
  solist(A1, A2, A10, Ainf)
})


###################################################
### code chunk number 28: Unit3R.Rnw:3-5
###################################################
newplot(22,0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 29: 12clustercox.Rnw:736-738
###################################################
plot(RFDemo[-1], equal.ribbon=TRUE, mar.panel=0, hsep=1, 
     main.panel="", main="")


###################################################
### code chunk number 30: 12clustercox.Rnw:824-826 (eval = FALSE)
###################################################
## Lambda <- rchisq(1, df=5)
## X      <- rpoispp(Lambda, W=A)


###################################################
### code chunk number 31: 12clustercox.Rnw:835-838 (eval = FALSE)
###################################################
## P      <- rpoislinetess(4)
## Lambda <- rMosaicField(P, rchisq, rgenargs=list(df=5))
## X      <- rpoispp(Lambda)


###################################################
### code chunk number 32: 12clustercox.Rnw:851-852
###################################################
X <- rLGCP(model="exp", mu=4, var=0.2, scale=0.1, win = square(1))


###################################################
### code chunk number 33: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 34: 12clustercox.Rnw:885-890
###################################################
Lam <- attr(X, "Lambda")
plot(solist(Lam, X), 
     main="", main.panel="", 
     equal.scales=TRUE, mar.panel=0.2, hsep=1,
     pch=16, cex=1.2)


###################################################
### code chunk number 35: 12clustercox.Rnw:908-909 (eval = FALSE)
###################################################
## X <- rLGCP(model="matern", mu=4, nu = 1, var=0.2, scale=0.1)


###################################################
### code chunk number 36: 12clustercox.Rnw:947-978
###################################################
set.seed(9282) # don't change this value!
MatClustParts <- local({
  Rad <- 0.15
  CEX <- 0.85
  X <- rSpecialMC(kappa=8, r=Rad, mu=5, saveLambda=TRUE, dimyx=512,allkids=TRUE)
  WX <- Window(X)
  Y <- attr(X, "parents")
  AK <- attr(X,"allkids")
  WY <- Window(Y)
  pa <- attr(X, "pid.all")
  Lambda <- attr(X, "Lambda")
  Join <- as.psp(from=Y[pa], to=AK)
  DY <- discs(Y, rep(Rad, npoints(Y)), separate=TRUE, delta=2*pi*Rad/64)
  DE <- lapply(DY, edges)
  DS <- do.call(superimpose, append(DE, list(W=grow.rectangle(WY, Rad))))
  D <- DS[WY]
  MatClustParts <- 
    layered(Blank=WY,
            Lambda=Lambda, WY=WY, WX=WX, Y=Y, D=D, Join=Join, X=X, AK=AK,
            plotargs=list(
              list(type="n"),
              list(ribbon=FALSE),
              list(lty=2),
              list(lwd=2),
              list(cex=CEX, pch=16),
              list(),
              list(),
              list(cex=CEX),
              list(cex=CEX)))
  MatClustParts
})


###################################################
### code chunk number 37: 12clustercox.Rnw:981-984
###################################################
ClusterDemo <- solist(MatClustParts[c(         "WY", "WX", "Y")],
                      MatClustParts[c(         "WY", "WX", "Y",  "Join",    "AK")],
                      MatClustParts[c("Blank",       "WX",               "X")])


###################################################
### code chunk number 38: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 39: 12clustercox.Rnw:991-994
###################################################
plot(ClusterDemo,
     main="", main.panel="",
     mar.panel=0, hsep=1, equal.scales=TRUE)


###################################################
### code chunk number 40: 12clustercox.Rnw:1119-1122
###################################################
MatClustDemo <- solist(MatClustParts[c(         "WY", "WX", "Y")],
                       MatClustParts[c(         "WY", "WX", "Y", "D", "AK")],
                       MatClustParts[c("Blank",       "WX",           "X")])


###################################################
### code chunk number 41: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 42: 12clustercox.Rnw:1129-1132
###################################################
plot(MatClustDemo,
     main="", main.panel="",
     mar.panel=0, hsep=1, equal.scales=TRUE)


###################################################
### code chunk number 43: 12clustercox.Rnw:1153-1157
###################################################
set.seed(20202)
MatClustPars <- list(kappa=8, R=0.1, mu=5)
MatClustEnsemble <- with(MatClustPars, 
                         rMatClust(kappa=kappa, r=R, mu=mu, nsim=8))


###################################################
### code chunk number 44: Unit2x4.Rnw:4-5
###################################################
zeromargins()


###################################################
### code chunk number 45: 12clustercox.Rnw:1168-1172
###################################################
plot(MatClustEnsemble,
     main="", main.panel="", nrows=2, 
     equal.scales=TRUE, mar.panel=0, hsep=1, vsep=1,
     pch=16, cex=0.75)


###################################################
### code chunk number 46: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 47: 12clustercox.Rnw:1215-1216
###################################################
plot(MatClustParts[c("Lambda", "WY", "WX", "Y")], main="")


###################################################
### code chunk number 48: 12clustercox.Rnw:1270-1301
###################################################
set.seed(1666)
ThomasParts <- local({
  kappa <- 3
  Rad <- 0.1
  X <- rThomas(kappa, Rad, 7, saveLambda=TRUE)
  Y <- attr(X, "parents")
  Z <- attr(X, "Lambda")
  W <- Window(X)
  Wp <- dilation(W, 2*Rad)
  Yp <- Y[Wp]
  ZW <- Z[W]
  Yeach <- split(Yp, factor(1:npoints(Yp)))
  Geach <- density(Yeach, Rad)
  GW <- solapply(Geach, "[", i=Wp, drop=FALSE)
  GW <- as.layered(GW)
  layerplotargs(GW) <- rep(list(.plot=contour,
                                levels=c(0.5, 1, 2, 5),
                                drawlabels=FALSE),
                           length(GW))
  ThomasParts <- 
    layered(Wp=Wp, ZW=ZW, W=W, GW=GW, 
            Yp=Yp, X=X,
            plotargs=list(
              list(type="n"),
              list(col=grey(seq(1,0,length=64))),
              list(),
              list(),
              list(pch=16),
              list(etch=TRUE)))
  ThomasParts
})


###################################################
### code chunk number 49: 12clustercox.Rnw:1303-1307
###################################################
ThomasDemo <- solist(
  ThomasParts[c("Wp",       "W",       "Yp"     )],
  ThomasParts[c("Wp",       "W", "GW", "Yp", "X")],
  ThomasParts[c("Wp",       "W",             "X")])


###################################################
### code chunk number 50: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 51: 12clustercox.Rnw:1312-1313
###################################################
setmargins(0.5)


###################################################
### code chunk number 52: 12clustercox.Rnw:1317-1331
###################################################
CEX <- 1
opa <- par(mfrow=c(1,3), mar=rep(0.5,4))
with(ThomasParts, {
  ## Left
  plot(Wp, main="", type="n")
  plot(W, add=TRUE)
  points(Yp, pch=16, cex=CEX)
  ## Middle
  persp(ZW, main="", theta=35, phi=40, border=NA, shade=0.6, axes=FALSE)
  ## Right
  plot(Wp, main="", type="n")
  plot(X, add=TRUE, cex=CEX, show.window=TRUE)
})
par(opa)


###################################################
### code chunk number 53: 12clustercox.Rnw:1359-1366
###################################################
set.seed(180)
XC <- rCauchy(5, 0.1, 8)
XV <- rVarGamma(10, 2, 0.1, 5)
CD <- solist(layered(XC, attr(XC, "parents")[Window(XC)],
                     plotargs=list(list(), list(pch=16))),
             layered(XV, attr(XV, "parents")[Window(XV)],
                     plotargs=list(list(), list(pch=16))))


###################################################
### code chunk number 54: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 55: 12clustercox.Rnw:1372-1374
###################################################
plot(CD, main="", main.panel="", equal.scales=TRUE,
     mar.panel=0, hsep=1)


###################################################
### code chunk number 56: 12clustercox.Rnw:1422-1445
###################################################
infoFun    <- spatstatClusterModelInfo
kernVG     <- infoFun("VarGamma")$ddist
kernCauchy <- infoFun("Cauchy")$ddist
kernMat <- infoFun("MatClust")$ddist
kernThomas <- infoFun("Thomas")$ddist
#
r <- seq(0, 0.3, length.out = 512)
R <- 0.1
scale <- 0.05
aquarius <- c(0.03, 0.02, 0.0155)
gnu <- c(1, 2, 5)
ckT  <- clusterkernel("Thomas",scale=scale)
ckM  <- clusterkernel("MatClust",scale=R)
ckC  <- clusterkernel("Cauchy",scale=scale)
ckV1 <- clusterkernel("VarGamma",scale=aquarius[1],nu=gnu[1])
#ckV2 <- clusterkernel("VarGamma",scale=aquarius[2],nu=gnu[2])
#ckV5 <- clusterkernel("VarGamma",scale=aquarius[3],nu=gnu[3])
xlp <- c(-0.3,-0.1,0.1,0.3)
axlp <- c(0,0.1,0.2,0.3)
LWD <- c(T=3,M=1,C=3,V=1)
LTY <- c(T=5,M=2,C=1,V=1)
COL <- grey(c(0,0,0.6,0))
names(COL) <- names(LTY)


###################################################
### code chunk number 57: Unit2LR.Rnw:3-5
###################################################
newplot(13, 0.9)
setmargins(0,2,0,2)


###################################################
### code chunk number 58: 12clustercox.Rnw:1452-1491
###################################################
par(mfrow=c(1,2))
# Left panel:
B <- 6 * scale
plot(function(x){ckT(x,0)},from=-B,to=B,
     ylim=c(0,90), n=512,ylab="",axes=FALSE,xlab="",
     lwd=LWD[["T"]], col=COL[["T"]], lty=LTY[["T"]])
plot(function(x){ckM(x,0)},from=-B,to=B,add=TRUE,n=512,
     lwd=LWD[["M"]], col=COL[["M"]], lty=LTY[["M"]])
plot(function(x){ckC(x,0)},from=-B,to=B,add=TRUE,n=512,
     lwd=LWD[["C"]], col=COL[["C"]], lty=LTY[["C"]])
plot(function(x){ckV1(x,0)},from=-B,to=B,add=TRUE,n=512,
     lwd=LWD[["V"]], col=COL[["V"]], lty=LTY[["V"]])
#plot(function(x){ckV2(x,0)},from=-B,to=B,add=TRUE,n=512)
#plot(function(x){ckV5(x,0)},from=-B,to=B,add=TRUE,n=512)
axis(side=2,at=c(0,20,40,60,80),labels=TRUE)
axis(side=1,at=xlp,labels=sprintf("%1.1f",xlp))
box()
#legend("topleft",lty=c(1,2,1,1),lwd=c(3,1,3,1),col=grey(c(0,0,0.4,0)),
#       bty="n",legend=c("Thomas","Matern","Cauchy","VarGamma"))
# Right panel:
plot(r, kernThomas(r, scale), main="", type="l", ylim = c(0,20), 
     ylab="Probability density", xlab="Distance from parent", axes=FALSE,
      lty=LTY[["T"]], col=COL[["T"]], lwd=LWD[["T"]])     
     #font.lab=3)
axis(1, at=axlp, labels=sprintf("%1.1f", axlp))
axis(2)
lines(r, kernMat(r, R), 
      lty=LTY[["M"]], col=COL[["M"]], lwd=LWD[["M"]])
lines(r, kernCauchy(r, scale, eps = 0), 
      lty=LTY[["C"]], col=COL[["C"]], lwd=LWD[["C"]])
#for(i in 1:length(gnu))
for(i in 1)
  lines(r, kernVG(r,aquarius[i], gnu[i]),
        lty=LTY[["V"]], col=COL[["V"]], lwd=LWD[["V"]])
    #lines(r, kernVG(r,nu.ker[i],omega[i], eps=0))
legout <- legend("topright",bty="n", 
                 legend=c("Thomas", "Mat\u00e9rn", "Cauchy", "VarGamma"),
                 lwd=LWD, lty=LTY, col=COL)
box()


###################################################
### code chunk number 59: 12clustercox.Rnw:1581-1601
###################################################
InhomMatClustDemo <- local({
  f <- function(x,y){ 1+2*x+2*y + sin(30*(x-y))}
  i <- as.im(f, owin())
  set.seed(42)
  R <- .1
  cent <- rSSI(R, 4, square(c(R,1-R)))
  Window(cent) <- owin()
  marks(cent) <- R
  i1 <- layered(i, unmark(cent), cent, 
                plotargs = list(list(ribside="bottom"),
                                list(pch=20),
                                list(markscale=2)))
  i2 <- i[dilation(cent, R), drop = FALSE]/(pi*R^2)
  X <- rpoispp(i2)
  Window(X) <- owin()
  i22 <- layered(i2, plotargs=list(ribside="bottom"))
  ## i3 <- density(unmark(cent), R/2, edge=FALSE)
  ## i3 <- i3*i
  solist(i1[-1], i1[1], i22, X)
})


###################################################
### code chunk number 60: Unit4l.Rnw:3-5
###################################################
newplot(25, 1)
setmargins(0.07)


###################################################
### code chunk number 61: 12clustercox.Rnw:1606-1610
###################################################
plot(InhomMatClustDemo, 
     main="", main.panel = "", 
     equal.scales=TRUE, nrows=1, valign=TRUE,
     mar.panel=c(1.5,0,0,0), hsep=1)


###################################################
### code chunk number 62: 12clustercox.Rnw:1649-1662
###################################################
InhomThomasDemo <- local({
  f <- function(x,y){ 1+2*x+2*y + sin(30*(x-y))}
  mu <- as.im(f, owin())
  Y <- InhomMatClustDemo[[1]][[1]]
  sigma <- 0.1
  h <- density(Y, sigma, edge=FALSE)
  Lambda <- h * mu
  X <- rpoispp(Lambda)
  hY <- layered(h, Y, plotargs=list(list(ribside="bottom"), list(pch=16)))
  mu <- layered(mu, plotargs=list(list(ribside="bottom")))
  Lambda <- layered(Lambda, plotargs=list(list(ribside="bottom")))
  solist(hY, mu, Lambda, X)
})


###################################################
### code chunk number 63: Unit4l.Rnw:3-5
###################################################
newplot(25, 1)
setmargins(0.07)


###################################################
### code chunk number 64: 12clustercox.Rnw:1667-1671
###################################################
plot(InhomThomasDemo, 
     main="", main.panel = "", 
     equal.scales=TRUE, nrows=1, valign=TRUE,
     mar.panel=c(1.5,0.5,0,0), hsep=1)


###################################################
### code chunk number 65: 12clustercox.Rnw:1790-1791
###################################################
rMatClust(kappa = 5, scale = 0.1, mu = 10, win = square(2))


###################################################
### code chunk number 66: 12clustercox.Rnw:1848-1850
###################################################
clusterradius("Thomas",scale=1)
clusterradius("VarGamma",scale=1,nu=2)


###################################################
### code chunk number 67: 12clustercox.Rnw:1858-1859
###################################################
set.seed(12345)


###################################################
### code chunk number 68: 12clustercox.Rnw:1861-1863
###################################################
mu <- as.im(function(x,y){ exp(2 * x + 1) }, owin())
X <- rMatClust(10, 0.05, mu)


###################################################
### code chunk number 69: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 70: 12clustercox.Rnw:1871-1872
###################################################
plot(X, pch=16, main="")


###################################################
### code chunk number 71: 12clustercox.Rnw:2087-2088
###################################################
fitM <- kppm(redwood ~ 1, "MatClust")


###################################################
### code chunk number 72: 12clustercox.Rnw:2090-2091
###################################################
fitM


###################################################
### code chunk number 73: 12clustercox.Rnw:2111-2116
###################################################
fitP <- ppm(redwood)
vcM <- signif(vcov(fitM), 3)
vcP <- signif(vcov(fitP), 3)
ciM <- signif(confint(fitM), 3)
ciP <- signif(confint(fitP), 3)


###################################################
### code chunk number 74: 12clustercox.Rnw:2143-2144
###################################################
fitT <- kppm(redwood ~ 1, "Thomas")


###################################################
### code chunk number 75: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 76: 12clustercox.Rnw:2155-2156
###################################################
plot(fitT,what="s",main="")


###################################################
### code chunk number 77: 12clustercox.Rnw:2167-2168
###################################################
fitL <- kppm(redwood ~ 1, "LGCP")


###################################################
### code chunk number 78: 12clustercox.Rnw:2196-2198
###################################################
fitTp <- kppm(redwood ~1, "Thomas", statistic="pcf")
fitTp


###################################################
### code chunk number 79: 12clustercox.Rnw:2213-2214
###################################################
fitBeiThom <- kppm(bei ~ elev + grad, "Thomas", data=bei.extra)


###################################################
### code chunk number 80: 12clustercox.Rnw:2216-2217
###################################################
fitBeiThom


###################################################
### code chunk number 81: 12clustercox.Rnw:2277-2278 (eval = FALSE)
###################################################
## ppm(bei ~ elev + grad, data=bei.extra)


###################################################
### code chunk number 82: 12clustercox.Rnw:2312-2313
###################################################
ciThom <- confint(fitBeiThom)


###################################################
### code chunk number 83: 12clustercox.Rnw:2315-2316 (eval = FALSE)
###################################################
## confint(fitBeiThom)


###################################################
### code chunk number 84: 12clustercox.Rnw:2318-2319
###################################################
round(ciThom,2)


###################################################
### code chunk number 85: 12clustercox.Rnw:2322-2323
###################################################
ciPois <- confint(ppm(bei~elev+grad,data=bei.extra))


###################################################
### code chunk number 86: 12clustercox.Rnw:2325-2326 (eval = FALSE)
###################################################
## confint(ppm(bei ~ elev + grad, data=bei.extra))


###################################################
### code chunk number 87: 12clustercox.Rnw:2328-2329
###################################################
round(ciPois,2)


###################################################
### code chunk number 88: 12clustercox.Rnw:2511-2513
###################################################
fit <- kppm(redwood ~ 1, "Thomas")
os <- objsurf(fit)


###################################################
### code chunk number 89: 12clustercox.Rnw:2515-2517 (eval = FALSE)
###################################################
## fit <- kppm(redwood ~ 1, "Thomas")
## contour(objsurf(fit))


###################################################
### code chunk number 90: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 91: 12clustercox.Rnw:2527-2528
###################################################
contour(os, main="", labcex=1, lwd=2)


###################################################
### code chunk number 92: 12clustercox.Rnw:2542-2543
###################################################
unlist(parameters(fit))


###################################################
### code chunk number 93: 12clustercox.Rnw:2552-2554 (eval = FALSE)
###################################################
## fit <- kppm(redwood ~ 1, "Thomas")
## plot(simulate(fit, nsim=4))


###################################################
### code chunk number 94: 12clustercox.Rnw:2563-2566
###################################################
fit <- kppm(redwood ~ 1, "Thomas")
set.seed(93464)
envLT <- envelope(fit, Lest, nsim=39)


###################################################
### code chunk number 95: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 96: 12clustercox.Rnw:2571-2572
###################################################
plot(envLT,main="")


###################################################
### code chunk number 97: 12clustercox.Rnw:2583-2584 (eval = FALSE)
###################################################
## envLT <- envelope(fit, Lest, nsim=39)


###################################################
### code chunk number 98: 12clustercox.Rnw:2612-2616
###################################################
set.seed(424242)
X <- rpoispp(500)
fitTX <- kppm(X~1, "Thomas")
fitLX <- kppm(X~1, "LGCP")


###################################################
### code chunk number 99: 12clustercox.Rnw:2618-2620 (eval = FALSE)
###################################################
## X <- rpoispp(500)
## kppm(X ~ 1, "Thomas")


###################################################
### code chunk number 100: 12clustercox.Rnw:2622-2623
###################################################
fitTX


###################################################
### code chunk number 101: 12clustercox.Rnw:2625-2626 (eval = FALSE)
###################################################
## kppm(X ~ 1, "LGCP")


###################################################
### code chunk number 102: 12clustercox.Rnw:2628-2629
###################################################
fitLX


###################################################
### code chunk number 103: 12clustercox.Rnw:2651-2652
###################################################
psibTX <- with(as.list(fitTX[["clustpar"]]), 1/(1+4*pi*kappa*scale^2))


###################################################
### code chunk number 104: 12clustercox.Rnw:2708-2709
###################################################


###################################################
### code chunk number 105: 12clustercox.Rnw:2737-2738 (eval = FALSE)
###################################################
## redTom <- lockppm(redwoodfull ~ 1, "Thomas", sigma=bw.lockppm)


###################################################
### code chunk number 106: RedTom
###################################################
## if(!file.exists("data/RedTomPalmAuto.rda")) {
##   if(!file.exists("data/RedTomBW.rda")) {
## NOT YET RELEASED IN SPATSTAT
##    bwRedTom <- bw.loccit(redwoodfull, ~1, "Thomas",
##                          srange=c(0.03, 0.2), ns=64, verbose=FALSE)
##    save(bwRedTom, file="data/RedTomBW.rda", compress=TRUE)
##  } else load("data/RedTomBW.rda")
##  redTomPalmAuto <- loccit(redwoodfull, ~1, "Thomas",
##                       sigma=bwRedTom, verbose=FALSE)
##  save(redTomPalmAuto, file="data/RedTomPalmAuto.rda", compress=TRUE)
## } else load("data/RedTomPalmAuto.rda")


###################################################
### code chunk number 107: 12clustercox.Rnw:2761-2762
###################################################
## NOT YET RELEASED IN SPATSTAT
## psR <- psib(redTomPalmAuto)


###################################################
### code chunk number 108: UnitR.Rnw:3-6
###################################################
newplot(9, 0.7)
zeromargins() # strip all margins
setmargins(0.1 + c(0,0,0,2)) # back off


###################################################
### code chunk number 109: 12clustercox.Rnw:2769-2772
###################################################
## NOT YET RELEASED IN SPATSTAT
## plot(psR, main="")
## if(monochrome) plot(redwoodfull, add=TRUE, pch=16, cols="white")
## plot(redwoodfull, add=TRUE, pch=1, lwd=2)


###################################################
### code chunk number 110: 12clustercox.Rnw:3256-3257
###################################################
fit <- kppm(redwood ~ 1, "Thomas", method="clik2")


