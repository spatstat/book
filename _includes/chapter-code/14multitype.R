### R code from vignette source '14multitype.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 14multitype.Rnw:9-13
###################################################
source("R/startup.R")
requireversion(spatstat, "1.41-1.028")
require(RandomFields)
require(dixon)


###################################################
### code chunk number 2: 14multitype.Rnw:16-21
###################################################
## shorter output
Digits <- 4
dopt <- options(digits=Digits)
Terse <- 3
spatstat.options(terse=Terse)


###################################################
### code chunk number 3: MucosaL.Rnw:3-5
###################################################
newplot(5, 0.5)
setmargins(0)


###################################################
### code chunk number 4: 14multitype.Rnw:52-53
###################################################
plot(mucosa, main="", chars=c(16, 3), cex=c(1, 0.6))


###################################################
### code chunk number 5: Mucosa2.Rnw:3-5
###################################################
newplot(10, 1)
setmargins(0)


###################################################
### code chunk number 6: 14multitype.Rnw:383-386
###################################################
plot(split(mucosa), main="", main.panel="",
     mar.panel=0, hsep=1,
     panel.args=function(i) if(i == 1) list(pch=16) else list(pch=3, cex=0.7))


###################################################
### code chunk number 7: 14multitype.Rnw:679-682
###################################################
mydata <- data.frame(x=runif(10), y=runif(10), 
           m=factor(sample(letters[1:3], 10, replace=TRUE)))
X <- as.ppp(mydata, square(1))


###################################################
### code chunk number 8: 14multitype.Rnw:693-696 (eval = FALSE)
###################################################
## copyExampleFiles("amacrine")
## W <- owin(c(0,1060/662), c(0,1))
## X <- scanpp("amacrine.txt", factor.marks=TRUE, window=W)


###################################################
### code chunk number 9: 14multitype.Rnw:713-714 (eval = FALSE)
###################################################
## marks(X) <- sample(marks(X), npoints(X))


###################################################
### code chunk number 10: 14multitype.Rnw:768-770
###################################################
Y <- split(mucosa)
Y


###################################################
### code chunk number 11: 14multitype.Rnw:784-785
###################################################
superimpose(A=runifpoint(5), B=runifpoint(7))


###################################################
### code chunk number 12: 14multitype.Rnw:789-790
###################################################
superimpose(runifpoint(5), runifpoint(7))


###################################################
### code chunk number 13: 14multitype.Rnw:825-827
###################################################
X <- betacells
marks(X) <- marks(X)[,1]


###################################################
### code chunk number 14: 14multitype.Rnw:830-833
###################################################
XX <- do.call(superimpose, split(X))
identical(X, XX)
max(nncross(X, XX)$dist)


###################################################
### code chunk number 15: 14multitype.Rnw:841-845
###################################################
X <- amacrine
Y <- split(amacrine)
Y$on <- rjitter(Y$on, 0.1)
split(X) <- Y


###################################################
### code chunk number 16: UnitL.Rnw:3-5
###################################################
newplot(9, 0.7)
setmargins(0.1+ c(0,2,0,0))


###################################################
### code chunk number 17: 14multitype.Rnw:905-906
###################################################
setmargins(0.05+c(0,1.5, 0, 0))


###################################################
### code chunk number 18: 14multitype.Rnw:910-911
###################################################
plot(lansing, main="", cex=0.6, leg.args=list(cex=1))


###################################################
### code chunk number 19: Unit2x3title.Rnw:3-5
###################################################
newplot(6.5, 0.8)
setmargins(0)


###################################################
### code chunk number 20: 14multitype.Rnw:930-931
###################################################
plot(split(lansing), main="", pch="+", mar.panel=c(0,0,1,0), hsep=1, vsep=1)


###################################################
### code chunk number 21: 14multitype.Rnw:943-944 (eval = FALSE)
###################################################
## plot(nbfires, which.marks="fire.type")


###################################################
### code chunk number 22: 14multitype.Rnw:949-950 (eval = FALSE)
###################################################
## plot(subset(nbfires, year == "1996"), which.marks="fire.type")


###################################################
### code chunk number 23: 14multitype.Rnw:953-954 (eval = FALSE)
###################################################
## plot(subset(nbfires, year == "1996", select=fire.type))


###################################################
### code chunk number 24: 14multitype.Rnw:958-959
###################################################
nbf <- subset(nbfires, year == "1996")


###################################################
### code chunk number 25: nbfiresL.Rnw:3-5
###################################################
newplot(5.76, 0.6)
setmargins(0)


###################################################
### code chunk number 26: 14multitype.Rnw:964-965
###################################################
plot(nbf, which.marks="fire.type", main="")


###################################################
### code chunk number 27: 14multitype.Rnw:976-978
###################################################
## print warnings in summary.ppp
spatstat.options(terse=2)


###################################################
### code chunk number 28: 14multitype.Rnw:983-985
###################################################
lansing
summary(lansing)


###################################################
### code chunk number 29: 14multitype.Rnw:992-994
###################################################
## back to terse printout
spatstat.options(terse=Terse)


###################################################
### code chunk number 30: 14multitype.Rnw:1000-1001
###################################################
intensity(rescale(lansing))


###################################################
### code chunk number 31: 14multitype.Rnw:1007-1008
###################################################
intensity(rescale(lansing, 5280/924))


###################################################
### code chunk number 32: 14multitype.Rnw:1014-1015
###################################################
head(as.data.frame(amacrine), 3)


###################################################
### code chunk number 33: 14multitype.Rnw:1108-1110
###################################################
sum(intensity(lansing))
intensity(unmark(lansing))


###################################################
### code chunk number 34: 14multitype.Rnw:1117-1118
###################################################
sqrt(intensity(lansing)/area(Window(lansing)))


###################################################
### code chunk number 35: 14multitype.Rnw:1124-1129
###################################################
Nx <- Ny <- 4
QC <- lapply(split(lansing), quadratcount, nx=Nx, ny=Ny)
QC <- lapply(QC, as.vector)
v <- sapply(QC, var)
(se <- sqrt(Nx * Ny * v)/area(Window(lansing)))


###################################################
### code chunk number 36: 14multitype.Rnw:1174-1175
###################################################
denlan <- density(split(lansing))


###################################################
### code chunk number 37: listof2x3.Rnw:3-5
###################################################
newplot(16.5, 1)
setmargins(0)


###################################################
### code chunk number 38: 14multitype.Rnw:1180-1181
###################################################
plot(denlan, main="", mar.panel=c(0,0,0,1), hsep=1)


###################################################
### code chunk number 39: 14multitype.Rnw:1198-1199 (eval = FALSE)
###################################################
## plot(density(split(lansing)))


###################################################
### code chunk number 40: 14multitype.Rnw:1218-1219 (eval = FALSE)
###################################################
## plot(density(split(lansing), se=TRUE)$SE)


###################################################
### code chunk number 41: 14multitype.Rnw:1252-1255
###################################################
lambda <- intensity(lansing)
probs <- lambda/sum(lambda)
probs


###################################################
### code chunk number 42: pLansing
###################################################
ProbU <- relrisk(lansing)
ProbD  <- relrisk(lansing, diggle=TRUE)


###################################################
### code chunk number 43: 14multitype.Rnw:1339-1342
###################################################
coco <- if(monochrome) grey(seq(1,0,length=128)) else NULL
plot(ProbD, main="", mar.panel=0.1+c(0,0,1,2), 
     equal.ribbon=TRUE, col=coco)


###################################################
### code chunk number 44: 14multitype.Rnw:1355-1358 (eval = FALSE)
###################################################
## lami <- density(split(lansing))
## lamdot <- Reduce("+", lami)
## probi <- solapply(lami, "/", lamdot)


###################################################
### code chunk number 45: 14multitype.Rnw:1373-1374
###################################################
ProbX <- relrisk(lansing, at="points")


###################################################
### code chunk number 46: 14multitype.Rnw:1376-1377
###################################################
ProbX[1:5]


###################################################
### code chunk number 47: 14multitype.Rnw:1435-1436 (eval = FALSE)
###################################################
## b <- bw.relrisk(lansing)


###################################################
### code chunk number 48: 14multitype.Rnw:1438-1440
###################################################
b <- bw.relrisk(lansing)
b2 <- bw.relrisk(lansing, hmin=0.025, hmax=0.1, nh=32) 


###################################################
### code chunk number 49: 14multitype.Rnw:1452-1453 (eval = FALSE)
###################################################
## Prob <- relrisk(lansing, sigma=b)


###################################################
### code chunk number 50: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 51: 14multitype.Rnw:1462-1467
###################################################
par(mfrow=c(1,2))
oa <- list(col=if(monochrome) 1 else 4, lty=2)
plot(b, main="", optargs=oa)
plot(b2, main="", optargs=oa)
par(mfrow=c(1,1))


###################################################
### code chunk number 52: 14multitype.Rnw:1490-1493
###################################################
dominant <- im.apply(ProbD, which.max)
species <- levels(marks(lansing))
dominant <- eval.im(factor(dominant, levels=1:6, labels=species))


###################################################
### code chunk number 53: 14multitype.Rnw:1502-1503
###################################################
setmargins(0.2 + c(0,0,0,3))


###################################################
### code chunk number 54: 14multitype.Rnw:1508-1509
###################################################
textureplot(dominant, main="")


###################################################
### code chunk number 55: LansingSegTest
###################################################
seglan <- segregation.test(lansing)


###################################################
### code chunk number 56: 14multitype.Rnw:1553-1554 (eval = FALSE)
###################################################
## segregation.test(lansing, nsim=19)


###################################################
### code chunk number 57: 14multitype.Rnw:1556-1557
###################################################
seglan


###################################################
### code chunk number 58: 14multitype.Rnw:1569-1570
###################################################
mucoRRnonparam <- relrisk(mucosa, casecontrol=FALSE)[[1]]


###################################################
### code chunk number 59: MucosaR.Rnw:3-5
###################################################
newplot(5, 0.5)
setmargins(0,0,0,1.2)


###################################################
### code chunk number 60: 14multitype.Rnw:1581-1583
###################################################
plot(mucoRRnonparam, main="")
contour(mucoRRnonparam, add=TRUE, col="white", drawlabels=FALSE)


###################################################
### code chunk number 61: 14multitype.Rnw:1696-1697 (eval = FALSE)
###################################################
## plot(relrisk(chorley, relative=TRUE, control="lung"))


###################################################
### code chunk number 62: 14multitype.Rnw:1704-1705 (eval = FALSE)
###################################################
## plot(relrisk(chorley, relative=TRUE, control="lung", se=TRUE)$SE)


###################################################
### code chunk number 63: 14multitype.Rnw:1709-1712
###################################################
rrcho <- relrisk(chorley,relative=TRUE,control="lung", 
                 hmin=10, hmax=20, nh=64,
                 se=TRUE)


###################################################
### code chunk number 64: 14multitype.Rnw:1714-1715
###################################################
spatstat.options(image.colfun=lighttodark)


###################################################
### code chunk number 65: Chorley2LR.Rnw:3-5
###################################################
newplot(18, 1)
setmargins(0, 2, 0, 2)


###################################################
### code chunk number 66: 14multitype.Rnw:1719-1720
###################################################
setmargins(0,0,0,2)


###################################################
### code chunk number 67: 14multitype.Rnw:1724-1731
###################################################
zz <- range(0.061, range(rrcho[[1]]))
pan <- function(i) { if(i == 1) list(zlim=zz) else list() }
pen <- function(i, y, ...) contour(y, ..., drawlabels=FALSE, col="white")
plot(as.solist(rrcho), 
     main="", main.panel="", mar.panel=0, hsep=2.8, 
     equal.scales=TRUE, panel.end=pen, panel.args=pan,
     ribscale=100)


###################################################
### code chunk number 68: 14multitype.Rnw:1824-1832
###################################################
## generate homogeneous and inhomogeneous multitype Poisson patterns.
set.seed(66888)
X1   <- rmpoispp(100, types=c("A","B"))
X2   <- rmpoispp(c(100,20), types=c("A","B"))
X3   <- rmpoispp(function(x,y,m){300*exp(-3*x)},types=c("A","B"))
lamb <- function(x,y,m) {ifelse(m=="A",
            300*exp(-4*x),300*exp(-4*(1-x))) }
X4   <- rmpoispp(lamb, types=c("A","B"))


###################################################
### code chunk number 69: Unit2L.Rnw:3-5
###################################################
newplot(13, 0.9)
setmargins(0,4,0,0)


###################################################
### code chunk number 70: 14multitype.Rnw:1838-1839
###################################################
plot(solist(X1, X2),main="",main.panel="",chars=c(16,3))


###################################################
### code chunk number 71: Unit2L.Rnw:3-5
###################################################
newplot(13, 0.9)
setmargins(0,4,0,0)


###################################################
### code chunk number 72: 14multitype.Rnw:1915-1916
###################################################
plot(solist(X3,X4),main="",main.panel="", chars=c(16,3))


###################################################
### code chunk number 73: 14multitype.Rnw:1994-1999 (eval = FALSE)
###################################################
## X1 <- rmpoispp(100, types=c("A","B"))
## X2 <- rmpoispp(c(100,20), types=c("A","B"))
## X3 <- rmpoispp(function(x,y,m){300*exp(-3*x)}, types=c("A","B"))
## ll <- function(x,y,m){ 300*exp(-4*ifelse(m=="A", x, (1-x))) }
## X4 <- rmpoispp(ll, types=c("A","B"))


###################################################
### code chunk number 74: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 75: 14multitype.Rnw:2067-2068 (eval = FALSE)
###################################################
## ppm(X ~ marks)


###################################################
### code chunk number 76: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 77: 14multitype.Rnw:2076-2077
###################################################
ppm(lansing ~ marks)


###################################################
### code chunk number 78: 14multitype.Rnw:2094-2095
###################################################
ppm(lansing ~ 1)


###################################################
### code chunk number 79: 14multitype.Rnw:2112-2113
###################################################
ppm(unmark(lansing) ~ 1)


###################################################
### code chunk number 80: 14multitype.Rnw:2132-2133 (eval = FALSE)
###################################################
## ppm(lansing ~ marks + x)


###################################################
### code chunk number 81: 14multitype.Rnw:2135-2138
###################################################
fit <- ppm(lansing ~ marks + x)
co <- coef(fit)
co <- signif(co, Digits)


###################################################
### code chunk number 82: 14multitype.Rnw:2140-2141
###################################################
fit


###################################################
### code chunk number 83: 14multitype.Rnw:2181-2182
###################################################
coX <- signif(coef(ppm(lansing ~ marks * x)), Digits)


###################################################
### code chunk number 84: 14multitype.Rnw:2186-2187
###################################################
ppm(lansing ~ marks * x)


###################################################
### code chunk number 85: 14multitype.Rnw:2224-2227
###################################################
mod3  <- ppm(lansing ~ polynom(x,y,3) * marks)
pred3 <- predict(mod3)
names(pred3) <- levels(marks(lansing))


###################################################
### code chunk number 86: Unit2x3tR.Rnw:4-6
###################################################
newplot(15, 1)
setmargins(0)


###################################################
### code chunk number 87: 14multitype.Rnw:2232-2238
###################################################
pen <- function(i, y, ...) contour(y, ..., drawlabels=FALSE, col="white")
plot(pred3, main="",mar.panel=c(0,0,0,1), hsep=1, panel.end=pen)
if(FALSE){
plot(pred3, equal.ribbon=TRUE,main="",mar.panel=rep(0.1,4), 
     ribmar=c(1,0,0,2))
}


###################################################
### code chunk number 88: 14multitype.Rnw:2260-2263 (eval = FALSE)
###################################################
## f0 <- ppm(lansing ~ polynom(x,y,3) + marks)
## f1 <- ppm(lansing ~ polynom(x,y,3) * marks)
## anova(f0, f1, test="LR")


###################################################
### code chunk number 89: 14multitype.Rnw:2278-2281
###################################################
Ants <- ants
levels(marks(Ants)) <- c("C", "M")
ppm(Ants ~ marks * (side - 1), data=ants.extra)


###################################################
### code chunk number 90: 14multitype.Rnw:2349-2350
###################################################
fit <- ppm(mucosa ~ marks * polynom(x,y,3))


###################################################
### code chunk number 91: cache
###################################################
probs <- relrisk(fit, casecontrol=FALSE)


###################################################
### code chunk number 92: 14multitype.Rnw:2361-2362 (eval = FALSE)
###################################################
## rusk <- relrisk(fit, relative=TRUE, control="other")


###################################################
### code chunk number 93: 14multitype.Rnw:2367-2369
###################################################
mucofit <- ppm(mucosa ~ marks * polynom(x,y,3))
mucoRRparam <- relrisk(mucofit, casecontrol=FALSE)[[1]]


###################################################
### code chunk number 94: MucosaR.Rnw:3-5
###################################################
newplot(5, 0.5)
setmargins(0,0,0,1.2)


###################################################
### code chunk number 95: 14multitype.Rnw:2375-2377
###################################################
plot(mucoRRparam, main="")
contour(mucoRRparam, add=TRUE, col="white", drawlabels=FALSE)


###################################################
### code chunk number 96: 14multitype.Rnw:2434-2436
###################################################
d <- nndist(amacrine, by=marks(amacrine))
head(d, 3)


###################################################
### code chunk number 97: 14multitype.Rnw:2447-2449
###################################################
d <- nndist(amacrine, by=marks(amacrine))
a <- aggregate(d, by=list(from=marks(amacrine)), min)


###################################################
### code chunk number 98: 14multitype.Rnw:2457-2461
###################################################
m <- marks(amacrine)
n <- nnwhich(amacrine)
m1 <- m[n]
table(m, m1)


###################################################
### code chunk number 99: 14multitype.Rnw:2514-2515
###################################################
nncorr(amacrine)


###################################################
### code chunk number 100: 14multitype.Rnw:2552-2553
###################################################
marktable(amacrine, N=1, collapse=TRUE)


###################################################
### code chunk number 101: 14multitype.Rnw:2560-2563
###################################################
dL <- dixon(as.data.frame(lansing))
dA <- dixon(as.data.frame(amacrine))
dM <- dixon(as.data.frame(mucosa))


###################################################
### code chunk number 102: 14multitype.Rnw:2565-2567 (eval = FALSE)
###################################################
## library(dixon)
## dixon(as.data.frame(lansing))$tablaC


###################################################
### code chunk number 103: 14multitype.Rnw:2569-2570
###################################################
dL$tablaC


###################################################
### code chunk number 104: 14multitype.Rnw:2574-2575 (eval = FALSE)
###################################################
## dixon(as.data.frame(amacrine))$tablaC


###################################################
### code chunk number 105: 14multitype.Rnw:2577-2578
###################################################
dA$tablaC


###################################################
### code chunk number 106: 14multitype.Rnw:2642-2643 (eval = FALSE)
###################################################
## K <- Kcross(amacrine, "on", "off")


###################################################
### code chunk number 107: 14multitype.Rnw:2657-2658
###################################################
Kall <- alltypes(rescale(amacrine), Kcross)


###################################################
### code chunk number 108: fasp2x2.Rnw:3-5
###################################################
newplot(12, 0.75)
setmargins(4, 4, 0, 0.5)


###################################################
### code chunk number 109: 14multitype.Rnw:2680-2681
###################################################
plot(Kall, lwd=2, banner=FALSE, xlim=c(0, 100))


###################################################
### code chunk number 110: 14multitype.Rnw:2828-2829 (eval = FALSE)
###################################################
## ma <- markconnect(amacrine, "on", "off")


###################################################
### code chunk number 111: 14multitype.Rnw:2846-2847
###################################################
AM <- alltypes(rescale(amacrine), markconnect)


###################################################
### code chunk number 112: fasp2x2.Rnw:3-5
###################################################
newplot(12, 0.75)
setmargins(4, 4, 0, 0.5)


###################################################
### code chunk number 113: 14multitype.Rnw:2852-2853
###################################################
plot(AM,banner=FALSE)


###################################################
### code chunk number 114: 14multitype.Rnw:2943-2945 (eval = FALSE)
###################################################
## AG <- Gcross(amacrine, "on", "off")
## AJ <- Jcross(amacrine, "on", "off")


###################################################
### code chunk number 115: 14multitype.Rnw:2957-2958
###################################################
AAG <- alltypes(rescale(amacrine), Gcross)


###################################################
### code chunk number 116: fasp2x2.Rnw:3-5
###################################################
newplot(12, 0.75)
setmargins(4, 4, 0, 0.5)


###################################################
### code chunk number 117: 14multitype.Rnw:2963-2964
###################################################
plot(AAG, lwd=2,banner=FALSE)


###################################################
### code chunk number 118: 14multitype.Rnw:2993-2995 (eval = FALSE)
###################################################
## plot(Gcross(amacrine, "on", "off"), km ~ r)
## plot(Fest(split(amacrine)$off), km ~ r, add=TRUE, lty=2)


###################################################
### code chunk number 119: 14multitype.Rnw:3103-3113
###################################################
X  <- rescale(amacrine)
AG <- Gdot(X, "on")
Jdif <- function(X, ..., i) { 
  Jidot <- Jdot(X, ..., i=i)
  Jii <- Jcross(X,i=i,j=i)
  dif <- eval.fv(Jidot-Jii)
  return(dif)
}
AJ <- Jdif(X,i="on")
PA <- function(i){switch(EXPR=i,list(ylim=c(0,1)),list(ylim=c(-0.3,0.3)))}


###################################################
### code chunk number 120: fv2Rolf.Rnw:3-5
###################################################
newplot(12, 1)
zeromargins()


###################################################
### code chunk number 121: 14multitype.Rnw:3118-3120
###################################################
plot(anylist(AG,AJ),main="",main.panel="",
     legend=FALSE,panel.args=PA,xlim=c(0,45))


###################################################
### code chunk number 122: 14multitype.Rnw:3147-3149
###################################################
aG <- alltypes(rescale(amacrine), "G")
Phi <- function(x) asin(sqrt(x))


###################################################
### code chunk number 123: 14multitype.Rnw:3151-3152 (eval = FALSE)
###################################################
## plot(aG, Phi(.) ~ Phi(theo))


###################################################
### code chunk number 124: fasp2x2.Rnw:3-5
###################################################
newplot(12, 0.75)
setmargins(4, 4, 0, 0.5)


###################################################
### code chunk number 125: 14multitype.Rnw:3159-3160
###################################################
plot(aG, Phi(.) ~ Phi(theo), banner=FALSE)


###################################################
### code chunk number 126: 14multitype.Rnw:3172-3177
###################################################
lansL <- alltypes(lansing, Lcross)
subL <- lansL[1:3, 1:3]
hickL <- lansL[2, ]
oaks <- c("redoak", "blackoak", "whiteoak")
oakL <- lansL[oaks, oaks]


###################################################
### code chunk number 127: 14multitype.Rnw:3181-3182
###################################################
Lmh <- lansL[3,2,drop=TRUE]


###################################################
### code chunk number 128: 14multitype.Rnw:3188-3189 (eval = FALSE)
###################################################
## aGfish <- eval.fasp(Phi(aG))


###################################################
### code chunk number 129: 14multitype.Rnw:3280-3283
###################################################
RA <- rescale(amacrine)
MCA <- markcorr(RA)
IA <- Iest(RA, r=seq(0, 100, length=512))


###################################################
### code chunk number 130: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 131: 14multitype.Rnw:3289-3290
###################################################
plot(anylist(MCA, IA), main="", main.panel="", legend=FALSE)


###################################################
### code chunk number 132: 14multitype.Rnw:3383-3389
###################################################
bD <- bw.diggle(mucosa)
bR <- bw.relrisk(mucosa)
LMD <- Lcross.inhom(mucosa, "ECL", "other", sigma=bD)
LMR <- Lcross.inhom(mucosa, "ECL", "other", sigma=bR)
bD3 <- signif(bD, 3)
bR3 <- signif(bR, 3)


###################################################
### code chunk number 133: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 134: 14multitype.Rnw:3395-3396
###################################################
plot(anylist(LMD, LMR), main="", main.panel="", legend=FALSE)


###################################################
### code chunk number 135: 14multitype.Rnw:3428-3430 (eval = FALSE)
###################################################
## LMD <- Lcross.inhom(mucosa, "ECL", "other", sigma=bw.diggle(mucosa))
## LMR <- Lcross.inhom(mucosa, "ECL", "other", sigma=bw.relrisk(mucosa))


###################################################
### code chunk number 136: 14multitype.Rnw:3594-3597 (eval = FALSE)
###################################################
## Amac <- rescale(amacrine)
## E <- envelope(Amac, Lcross, nsim=39, i="on", j="off",
##               simulate=expression(rshift(Amac, radius=150)))


###################################################
### code chunk number 137: 14multitype.Rnw:3614-3618
###################################################
set.seed(40063)
Amac <- rescale(amacrine)
E <- envelope(Amac, Lcross, nsim=39, i="on", j="off",
              simulate=expression(rshift(Amac, radius=150)))


###################################################
### code chunk number 138: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 139: 14multitype.Rnw:3625-3626
###################################################
plot(E, . - r ~ r, main="",legend=FALSE)


###################################################
### code chunk number 140: 14multitype.Rnw:3690-3696
###################################################
Jdif <- function(X, ..., i) {
  Jidot <- Jdot(X, ..., i=i)
  J <- Jest(X, ...)
  dif <- eval.fv(Jidot-J)
  return(dif)
}


###################################################
### code chunk number 141: 14multitype.Rnw:3709-3710
###################################################
set.seed(58742)


###################################################
### code chunk number 142: 14multitype.Rnw:3712-3714
###################################################
EPJ <- envelope(paracou, Jdif, nsim=39, i="juvenile",
                simulate=expression(rlabel(paracou)))


###################################################
### code chunk number 143: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 144: 14multitype.Rnw:3740-3741
###################################################
plot(EPJ, main="",legend=FALSE)


###################################################
### code chunk number 145: 14multitype.Rnw:3773-3775
###################################################
X <- rescale(amacrine)
aE <- alltypes(X, Lcross, nsim=39, envelope=TRUE, global=TRUE)


###################################################
### code chunk number 146: fasp2x2.Rnw:3-5
###################################################
newplot(12, 0.75)
setmargins(4, 4, 0, 0.5)


###################################################
### code chunk number 147: 14multitype.Rnw:3786-3787
###################################################
plot(aE, . - r  ~ r, banner=FALSE, samex=TRUE, samey=TRUE)


###################################################
### code chunk number 148: 14multitype.Rnw:3859-3860 (eval = FALSE)
###################################################
## ppm(amacrine ~ marks, MultiHard())


###################################################
### code chunk number 149: 14multitype.Rnw:4200-4201 (eval = FALSE)
###################################################
## ppm(amacrine ~ marks, interaction=MultiHard())


###################################################
### code chunk number 150: 14multitype.Rnw:4229-4230
###################################################
ppm(hyytiala ~ marks, Hardcore())


###################################################
### code chunk number 151: 14multitype.Rnw:4245-4246
###################################################
ppm(hamster ~ marks, Strauss(0.02))


###################################################
### code chunk number 152: 14multitype.Rnw:4321-4322
###################################################
ppm(amacrine ~ marks, MultiHard())


###################################################
### code chunk number 153: 14multitype.Rnw:4340-4342
###################################################
Beta <- betacells
marks(Beta) <- marks(Beta)[,"type"]


###################################################
### code chunk number 154: 14multitype.Rnw:4345-4346
###################################################
Beta <- subset(betacells, select=type)


###################################################
### code chunk number 155: 14multitype.Rnw:4354-4357
###################################################
R <- diag(c(60,80))
fit0 <- ppm(Beta ~ marks, MultiStrauss(R))
fit0


###################################################
### code chunk number 156: 14multitype.Rnw:4361-4364
###################################################
R2 <- matrix(c(60,60,60,80), nrow=2, ncol=2)
fit1 <- ppm(Beta ~ marks, MultiStrauss(R2))
fit1


###################################################
### code chunk number 157: 14multitype.Rnw:4372-4373
###################################################
anova(fit0, fit1, test="LR")


###################################################
### code chunk number 158: 14multitype.Rnw:4389-4391 (eval = FALSE)
###################################################
## fitP <- ppm(Beta ~ marks + polynom(x,y,3), MultiStrauss(R))
## fitI <- ppm(Beta ~ marks * polynom(x,y,3), MultiStrauss(R))


###################################################
### code chunk number 159: 14multitype.Rnw:4414-4415
###################################################
fitBeta <- ppm(Beta ~ marks, MultiStraussHard(iradii=R2))


###################################################
### code chunk number 160: 14multitype.Rnw:4417-4418 (eval = FALSE)
###################################################
## plot(fitin(fitBeta))


###################################################
### code chunk number 161: fasp2x2.Rnw:3-5
###################################################
newplot(12, 0.75)
setmargins(4, 4, 0, 0.5)


###################################################
### code chunk number 162: 14multitype.Rnw:4431-4432
###################################################
plot(fitin(fitBeta),banner=FALSE)


###################################################
### code chunk number 163: 14multitype.Rnw:4453-4455
###################################################
set.seed(24819)
Beta.sim <- rmh(fitBeta, verbose=FALSE)


###################################################
### code chunk number 164: 14multitype.Rnw:4457-4458 (eval = FALSE)
###################################################
## Beta.sim <- rmh(fitBeta)


###################################################
### code chunk number 165: Beta2L.Rnw:3-5
###################################################
newplot(12, 0.85)
setmargins(0)


###################################################
### code chunk number 166: 14multitype.Rnw:4466-4468
###################################################
plot(solist(Beta,Beta.sim),main="",main.panel="",
     mar.panel=0, hsep=1, equal.scales=TRUE)


###################################################
### code chunk number 167: 14multitype.Rnw:4483-4491 (eval = FALSE)
###################################################
## HC <- matrix(c(20,10,10,20),2,2)
## R  <- matrix(c(30,20,20,30),2,2)
## G  <- matrix(0.3,2,2)
## B  <- c(5,10)*1e-5
## M  <- rmhmodel(cif="straushm",
##                par=list(beta=B,gamma=G,iradii=R,hradii=HC),
##                types=c("A","B"), w=square(1000))
## X  <- rmh(M)


###################################################
### code chunk number 168: UnitL.Rnw:3-5
###################################################
newplot(9, 0.7)
setmargins(0.1+ c(0,2,0,0))


###################################################
### code chunk number 169: 14multitype.Rnw:4546-4547
###################################################
plot(bramblecanes, main="")


###################################################
### code chunk number 170: 14multitype.Rnw:4854-4858
###################################################
Ants <- ants 
levels(marks(Ants)) <- c("Cat", "Mess")
rmat <- matrix(c(65,45,45,30), nrow=2, ncol=2)
fit <- ppm(Ants ~ marks, HierStrauss(rmat, archy=c(2,1)))


###################################################
### code chunk number 171: 14multitype.Rnw:4860-4862
###################################################
fit
coef(summary(fit))


###################################################
### code chunk number 172: 14multitype.Rnw:4864-4866
###################################################
g <- summary(fitin(fit))$sensible$param$gammas
g <- signif(g, Digits)


###################################################
### code chunk number 173: 14multitype.Rnw:4870-4876 (eval = FALSE)
###################################################
## rseq   <- seq(20, 70, by=5)
## s   <- expand.grid(r11=rseq, r12=rseq, r22=rseq)
## pfn <- function(r11, r12, r22) {
##   HierStrauss(matrix(c(r11,r12,r12,r22),2,2), archy=2:1)
## }
## prf <- profilepl(s, pfn, ants ~ marks, correction="translate")


###################################################
### code chunk number 174: 14multitype.Rnw:4894-4896
###################################################
fit0 <- update(fit, HierStrauss(diag(diag(rmat))))
anova(fit0, fit, test="LR")


###################################################
### code chunk number 175: 14multitype.Rnw:4979-4980
###################################################
set.seed(873987)


###################################################
### code chunk number 176: 14multitype.Rnw:4982-4991
###################################################
P <- dirichlet(runifpoint(10))
logLambda <- rMosaicField(P, rnorm, 
                          dimyx=512, rgenargs=list(mean=4, sd=1))
Lambda <- exp(logLambda)
X <- rpoispp(Lambda)
Xlinked <- X %mark% factor(sample(c("a","b"), npoints(X), 
                                  replace=TRUE, prob=c(2,3)/5))
Y <- rpoispp(100)
Xbalanced <- Y %mark% factor(ifelse(Lambda[Y] > exp(4), "a", "b"))


###################################################
### code chunk number 177: 14multitype.Rnw:4994-4995
###################################################
set.seed(676532)


###################################################
### code chunk number 178: 14multitype.Rnw:4997-5003
###################################################
X1 <- rLGCP("exp", 4, var=0.2, scale=.1)
Z1 <- log(attr(X1, "Lambda"))
Z2 <- log(attr(rLGCP("exp", 4, var=0.1, scale=.1), "Lambda"))
Lam1 <- exp(Z1)
Lam2 <- exp((Z1 + Z2)/2)
Xlgcp <- superimpose(a=rpoispp(Lam1), b=rpoispp(Lam2))


###################################################
### code chunk number 179: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 180: 14multitype.Rnw:5009-5013
###################################################
plot(solist(Xlinked, Xlgcp, Xbalanced),
     main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=1,
     legend=FALSE, chars=c(16,3))


###################################################
### code chunk number 181: 14multitype.Rnw:5056-5057
###################################################
set.seed(29872)


###################################################
### code chunk number 182: 14multitype.Rnw:5059-5066
###################################################
ftcf <- function(x0, y0, radius, mu) {
  n <- rpois(1, mu)
  Y   <- runifdisc(n, radius=radius, centre=c(x0, y0))
  Z   <- factor(sample(1:2, npoints(Y), replace=TRUE), levels=1:2)
  return(Y %mark% Z)
}
Xmc2 <- rNeymanScott(15,0.1,ftcf, radius=0.1, mu=5)


###################################################
### code chunk number 183: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 184: 14multitype.Rnw:5073-5074
###################################################
plot(Xmc2, chars=c(1,16),main="")


###################################################
### code chunk number 185: 14multitype.Rnw:5128-5139
###################################################
V <- rmpoispp(c(50,50), types=letters[1:2])
Va <- split(V)$a
Vb <- split(V)$b
oka <- nncross(Va,Vb, what="dist") >0.07
okb <- nncross(Vb,Va, what="dist") >0.07
U <- superimpose(a=Va[oka], b=Vb[okb])
A <- rpoispp(50)
B <- rpoispp(50)
ok <- nncross(B,A,what="dist") > 0.07
B <- B[ok]
AB <- superimpose(a=A,b=B,W=owin())


###################################################
### code chunk number 186: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 187: 14multitype.Rnw:5146-5148
###################################################
plot(solist(U,AB), chars=c(1,16), 
     main.panel="", main="", mar.panel=0, hsep=1, legend=FALSE)


###################################################
### code chunk number 188: 14multitype.Rnw:5187-5195
###################################################
m  <- as.im(function(x, y){5 - 1.5 * (x - 0.5)^2 + 2 * (y - 0.5)^2}, W=owin())
set.seed(31852)
RL <- attr(rLGCP("gauss", m, var=0.15, scale =0.5),"Lambda")
RL <- cut(RL,4,labels=letters[1:4])
X  <- rpoispp(100,win=Window(RL))
Y <- X %mark% RL[X]
layerplotargs(RL) <- list(ribside="left")
layerplotargs(Y) <- list(leg.side="right")


###################################################
### code chunk number 189: Unit3R.Rnw:3-5
###################################################
newplot(22,0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 190: 14multitype.Rnw:5198-5199
###################################################
setmargins(0,1,0,1)


###################################################
### code chunk number 191: 14multitype.Rnw:5203-5207
###################################################
plot(solist(RL, X, Y),
     main="", main.panel="",
     equal.scales=TRUE, valign=TRUE,
     mar.panel=0, hsep=1, box=TRUE)


###################################################
### code chunk number 192: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 193: 14multitype.Rnw:5467-5476 (eval = FALSE)
###################################################
## nt <- length(levels(marks(X)))
## Mf <- function(rwithin, rbetween) { 
##   R <- matrix(rbetween, nt, nt)
##   diag(R) <- rwithin
##   return(MultiStrauss(R))
## }
## df <- expand.grid(rwithin=seq(rmin, rmax, length=5),
##                   rbetween=seq(rmin, rmax, length=5))
## profilepl(df, Mf, X ~ trend)


###################################################
### code chunk number 194: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


