### R code from vignette source '17network.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 17network.Rnw:9-14
###################################################
source("R/startup.R")
source("R/short.output.R")
spatstat.options(terse=2)
Digits <- 3
options(digits=Digits)


###################################################
### code chunk number 2: 17network.Rnw:17-19
###################################################
#requireversion(book, "3.0-1")
requireversion(spatstat, "1.41-1.033")


###################################################
### code chunk number 3: ChicagoR.Rnw:3-5
###################################################
newplot(6, 0.75)
setmargins(0,0,0,2)


###################################################
### code chunk number 4: 17network.Rnw:51-52
###################################################
plot(chicago, main="", col="grey", cols="black", lwd=2, leg.side="right")


###################################################
### code chunk number 5: Unitl.Rnw:3-5
###################################################
newplot(9, 0.7)
setmargins(0.1+ c(0,2,0,0))


###################################################
### code chunk number 6: 17network.Rnw:77-78
###################################################
setmargins(0.1)


###################################################
### code chunk number 7: 17network.Rnw:82-88
###################################################
plot(grow.rectangle(Frame(spiders), c(150,0), 0), type="n", main="")
plot(spiders, add=TRUE, pch=16)
y0 <- 562.5
plot(yardstick(-70, y0-100/2, -70, y0+100/2, "100 mm"),
     angle=90, lwd=2, pos=2, txt.shift=c(-30, 100),
     txt.args=list(srt=90, cex=0.85, adj=c(0.5,0)))


###################################################
### code chunk number 8: 17network.Rnw:128-140
###################################################
ys <- yardstick(-10, 200-50/2, -10, 200+50/2, "50 microns")
Window(ys) <- owin(c(-20, 0), Frame(ys)$yrange)
dendro <- layered(dendrite, ys)
layerplotargs(dendro) <- list(
  list(
    leg.side="right", cex=0.6, 
    leg.args=list(cex=1), lwd=1
    ),
  list(
    angle=90, pos=2, cex=0.85, txt.shift=c(-8,25),
    txt.args=list(srt=90)
    ))


###################################################
### code chunk number 9: DendriteLR.Rnw:3-5
###################################################
newplot(6, 0.75)
setmargins(0,4,0,1)


###################################################
### code chunk number 10: 17network.Rnw:147-148
###################################################
plot(dendro, main="")


###################################################
### code chunk number 11: 17network.Rnw:201-207
###################################################
source("R/shortestpath.R")
L <- domain(chicago)
v <- vertices(L)
pathAB <- shortestpath(L, 94, 303)
pathACB <- c(shortestpath(L, 94, 63),
             shortestpath(L, 63, 303))


###################################################
### code chunk number 12: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 13: 17network.Rnw:214-216
###################################################
plot(simplenet, main="")
points(vertices(simplenet), pch=16)


###################################################
### code chunk number 14: Chicago.Rnw:3-5
###################################################
newplot(5, 0.75)
setmargins(0)


###################################################
### code chunk number 15: 17network.Rnw:220-224
###################################################
plot(L, col="grey", main="")
joinpoints(v, pathACB, lwd=3, lty=2, col=if(monochrome) "black" else "red")
joinpoints(v, pathAB,  lwd=3,        col=if(monochrome) "black" else "green")
points(v[c(94, 303)], pch=16)


###################################################
### code chunk number 16: 17network.Rnw:376-380
###################################################
v <- ppp(x=(-2):2, y=3*c(0,1,2,1,0), c(-3,3), c(-1,7))
edg <- matrix(c(1,2,3,4,2,
                2,3,4,5,4), ncol=2)
letterA <- linnet(v, edges=edg)


###################################################
### code chunk number 17: dpath
###################################################
btime <- system.time(b <- as.linnet(dendrite, sparse=FALSE))


###################################################
### code chunk number 18: 17network.Rnw:445-446 (eval = FALSE)
###################################################
## b <- as.linnet(dendrite, sparse=FALSE)


###################################################
### code chunk number 19: 17network.Rnw:448-450
###################################################
print(object.size(dendrite), units="Mb")
print(object.size(b),        units="Mb")


###################################################
### code chunk number 20: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 21: 17network.Rnw:482-483 (eval = FALSE)
###################################################
## plot(L); text(vertices(L), labels=vertexdegree(L))


###################################################
### code chunk number 22: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 23: 17network.Rnw:537-538
###################################################
mean(lengths.psp(as.psp(domain(chicago))))


###################################################
### code chunk number 24: 17network.Rnw:594-597
###################################################
xx <- list(x=c(-1.5,0,0.5,1.5), y=c(1.5,3,4.5,1.5))
X <- lpp(xx, letterA)
X


###################################################
### code chunk number 25: 17network.Rnw:615-618
###################################################
spiders
spidersm <- rescale(spiders, 1000, c("metre", "metres"))
summary(spidersm)


###################################################
### code chunk number 26: 17network.Rnw:763-768
###################################################
bricks <- domain(spiders)
alpha <- angles.psp(as.psp(bricks)) * 180/pi
mortarvert <- (round(alpha/90) == 1)
f <- function(x, y, seg, tp) { mortarvert[seg] }
vertical <- linfun(f, bricks)


###################################################
### code chunk number 27: 17network.Rnw:779-782
###################################################
vertical(spiders[5:8])
which(vertical(spiders))
table(vertical(spiders))


###################################################
### code chunk number 28: 17network.Rnw:815-819
###################################################
dna <- distfun(split(chicago)$assault)
Dna <- as.linim(dna)
a <- sqrt(Dna) + 3
b <- eval.linim(pmin(Dna, 250))


###################################################
### code chunk number 29: 17network.Rnw:838-840 (eval = FALSE)
###################################################
## plot(dna, style="colour", ribside="left", box=FALSE)
## plot(dna, style="width", adjust=2.5)


###################################################
### code chunk number 30: ChicagoL.Rnw:3-5
###################################################
newplot(6, 0.75)
setmargins(0,1,0,0)


###################################################
### code chunk number 31: 17network.Rnw:847-848
###################################################
plot(dna, style="colour", ribside="left", main="", box=FALSE)


###################################################
### code chunk number 32: Chicago.Rnw:3-5
###################################################
newplot(5, 0.75)
setmargins(0)


###################################################
### code chunk number 33: 17network.Rnw:852-853
###################################################
plot(dna, style="width", main="", adjust=2.5)


###################################################
### code chunk number 34: 17network.Rnw:917-919
###################################################
spidersm <- rescale(spiders, 1000, c("metre", "metres"))
intensity(spidersm)


###################################################
### code chunk number 35: 17network.Rnw:925-926
###################################################
summary(spidersm)


###################################################
### code chunk number 36: 17network.Rnw:929-930
###################################################
intensity(as.ppp(spidersm))


###################################################
### code chunk number 37: 17network.Rnw:1002-1009
###################################################
L <- simplenet
L <- L[boundingbox(vertices(L))]
XX <- as.lpp(0.41259, 0.6024, L=L)
D1 <- density(XX, 0.1)
D2 <- density(XX, 0.2)
D3 <- density(XX, 0.3)
KernelDemo <- solist(XX, D1, D2, D3)


###################################################
### code chunk number 38: Unit4.Rnw:3-5
###################################################
newplot(25, 1)
zeromargins()


###################################################
### code chunk number 39: 17network.Rnw:1014-1017
###################################################
plot(KernelDemo, main="", main.panel="", nrows=1,
     mar.panel=0, hsep=0.5, 
     panel.args=function(i) if(i == 1) list(pch=16) else list(style="w", adjust=5/i))


###################################################
### code chunk number 40: LongDensityCalculation
###################################################
d60 <- density(unmark(chicago), 60)


###################################################
### code chunk number 41: 17network.Rnw:1051-1052
###################################################
spatstat.options(image.colfun=lighttodark)


###################################################
### code chunk number 42: Chicago.Rnw:3-5
###################################################
newplot(5, 0.75)
setmargins(0)


###################################################
### code chunk number 43: 17network.Rnw:1058-1059
###################################################
plot(d60, style="width", adjust=2, main="")


###################################################
### code chunk number 44: ChicagoR.Rnw:3-5
###################################################
newplot(6, 0.75)
setmargins(0,0,0,2)


###################################################
### code chunk number 45: 17network.Rnw:1063-1064
###################################################
plot(d60 * 5280, style="colour", main="", box=FALSE)


###################################################
### code chunk number 46: 17network.Rnw:1076-1078
###################################################
load("data/dendriteLam10.rda")
dlr <- round(range(dendriteLam10), 2)


###################################################
### code chunk number 47: Dendrite.Rnw:3-5
###################################################
newplot(5, 0.6)
setmargins(0)


###################################################
### code chunk number 48: 17network.Rnw:1083-1084
###################################################
plot(dendriteLam10, style="width", main="")


###################################################
### code chunk number 49: 17network.Rnw:1103-1104
###################################################
rhoChicY <- rhohat(unmark(chicago), "y")


###################################################
### code chunk number 50: 17network.Rnw:1126-1128
###################################################
along <- linfun(function(x,y,seg,tp) { tp }, domain(spiders))
rhoalong <- rhohat(spiders, along)


###################################################
### code chunk number 51: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 52: 17network.Rnw:1138-1141
###################################################
plot(anylist(rhoChicY, rhoalong),
     main="", main.panel="", nrows=1,
     legend=FALSE)


###################################################
### code chunk number 53: 17network.Rnw:1158-1160
###################################################
berman.test(unmark(chicago), "y")
cdf.test(spiders, along)


###################################################
### code chunk number 54: 17network.Rnw:1170-1174
###################################################
mortlen <- lengths.psp(as.psp(bricks))
(totlen <- tapply(mortlen, mortarvert, sum)/1000)
(totpts <- table(vertical(spiders)))
totpts/totlen


###################################################
### code chunk number 55: 17network.Rnw:1231-1232
###################################################
intensity(chicago)


###################################################
### code chunk number 56: 17network.Rnw:1236-1238
###################################################
chicagomiles <- rescale(chicago, 5280, c("mile","miles"))
summary(chicagomiles)


###################################################
### code chunk number 57: 17network.Rnw:1257-1258 (eval = FALSE)
###################################################
## DSD <- density(split(dendrite), sigma=10)


###################################################
### code chunk number 58: 17network.Rnw:1260-1261
###################################################
load("data/dendriteSplitLam10.rda")


###################################################
### code chunk number 59: Dendrite3.Rnw:3-5
###################################################
newplot(15, 1)
setmargins(0)


###################################################
### code chunk number 60: 17network.Rnw:1268-1271
###################################################
plot(DSD, main="", main.panel="", nrows=1, 
     plotcommand=plot.linim, box=FALSE,
     mar.panel=0, hsep=0, style="w", equal.scales=TRUE)


###################################################
### code chunk number 61: 17network.Rnw:1282-1283
###################################################
spatstat.options(print.ppm.SE="never")


###################################################
### code chunk number 62: 17network.Rnw:1321-1323 (eval = FALSE)
###################################################
## XS <- rpoislpp(intensity(spiders), domain(spiders))
## XC <- rpoislpp(d60)


###################################################
### code chunk number 63: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 64: 17network.Rnw:1349-1351 (eval = FALSE)
###################################################
## lppm(X ~ trend, ...)
## lppm(X ~ trend, ..., data)


###################################################
### code chunk number 65: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 66: 17network.Rnw:1371-1372
###################################################
lppm(spiders ~ polynom(x,y,2))


###################################################
### code chunk number 67: 17network.Rnw:1393-1394 (eval = FALSE)
###################################################
## fitca <- lppm(chicago ~ marks + polynom(x,y,2))


###################################################
### code chunk number 68: 17network.Rnw:1400-1401 (eval = FALSE)
###################################################
## fitcx <- lppm(chicago ~ marks * polynom(x,y,2))


###################################################
### code chunk number 69: fitmodel
###################################################
fit <- lppm(unmark(chicago) ~ polynom(x,y,2))
lam <- predict(fit, dimyx=512)


###################################################
### code chunk number 70: fillim
###################################################
fillwin <- Window(chicago)
fillmask <- as.mask(fillwin)
lamfill <- predict(fit, locations=fillmask)


###################################################
### code chunk number 71: 17network.Rnw:1477-1478
###################################################
spatstat.options(image.colfun=lighttodark)


###################################################
### code chunk number 72: Chicago.Rnw:3-5
###################################################
newplot(5, 0.75)
setmargins(0)


###################################################
### code chunk number 73: 17network.Rnw:1484-1485
###################################################
plot(lam, style="width", adjust=1.5, main="")


###################################################
### code chunk number 74: 17network.Rnw:1487-1490
###################################################
plot(fillwin, type="n", main="")
contour(lamfill * 1000, lwd=2, add=TRUE, labcex=1)
# plot(lam * 5280, style="colour", main="", box=FALSE)


###################################################
### code chunk number 75: spiders0
###################################################
fit0 <- lppm(spiders ~ polynom(x,y,2) + vertical)


###################################################
### code chunk number 76: spiders1
###################################################
fit1 <- update(fit0, . ~ . + polynom(along,2))


###################################################
### code chunk number 77: 17network.Rnw:1523-1524 (eval = FALSE)
###################################################
## anova(fit0, fit1, test="LR")


###################################################
### code chunk number 78: 17network.Rnw:1526-1527
###################################################
short.output(anova(fit0, fit1, test="LR"), maxwidth=80)


###################################################
### code chunk number 79: 17network.Rnw:1537-1539
###################################################
chicagoG <- mergeLevels(chicago,
               person=c("assault", "robbery"), property=NULL)


###################################################
### code chunk number 80: 17network.Rnw:1544-1545
###################################################
chicagoG


###################################################
### code chunk number 81: 17network.Rnw:1548-1550
###################################################
fitGcx <- lppm(chicagoG ~ marks * polynom(x,y,2))
fitGca <- lppm(chicagoG ~ marks + polynom(x,y,2))


###################################################
### code chunk number 82: 17network.Rnw:1552-1553
###################################################
anova(fitGca, fitGcx, test="LR")


###################################################
### code chunk number 83: 17network.Rnw:1615-1617
###################################################
dend <- as.lpp(unmark(dendrite), sparse=FALSE)
L <- domain(dend)


###################################################
### code chunk number 84: 17network.Rnw:1624-1626
###################################################
v1 <- lpp(vertices(L)[1], L)
d <- distfun(v1)


###################################################
### code chunk number 85: 17network.Rnw:1635-1636
###################################################
load("data/simpletree.rda")


###################################################
### code chunk number 86: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 87: 17network.Rnw:1642-1645
###################################################
plot(simpletree, main="")
plot(vertices(simpletree)[1], add=TRUE, pch=16)
text(midloc, labels=midlabels, font=2)


###################################################
### code chunk number 88: 17network.Rnw:1653-1655
###################################################
tb <- treebranchlabels(L, root=1)
b <- branchlabelfun(L, root=1)


###################################################
### code chunk number 89: 17network.Rnw:1660-1661 (eval = FALSE)
###################################################
## lppm(dend ~ (substr(b,1,1) == "a"))


###################################################
### code chunk number 90: 17network.Rnw:1668-1669
###################################################
g <- function(x,n) factor(substr(x, 1, n))


###################################################
### code chunk number 91: 17network.Rnw:1674-1675
###################################################
step(lppm(dend ~ d * g(b, 1)), trace=0)


###################################################
### code chunk number 92: 17network.Rnw:1683-1684
###################################################
step(lppm(dend ~ d * g(b, 2)), trace=0)


###################################################
### code chunk number 93: 17network.Rnw:1694-1695
###################################################
lppm(dend ~ d * begins(b, "ac"))


###################################################
### code chunk number 94: 17network.Rnw:1702-1703 (eval = FALSE)
###################################################
## dendAC <- extractbranch(dend, code="ac", labels=tb)


###################################################
### code chunk number 95: 17network.Rnw:1725-1759
###################################################
S <- as.psp(simplenet)
iu <- 3
iv <- 10
tu <- 0.3
tv <- 0.2
dt <- 0.1/2
Su <- S$ends[iu,,drop=FALSE]
Sv <- S$ends[iv,,drop=FALSE]
W <- as.owin(simplenet)
u <- with(Su, list(x=x0 + tu*(x1-x0),
                   y=y0 + tu*(y1-y0)))
v <- with(Sv, list(x=x0 + tv*(x1-x0),
                   y=y0 + tv*(y1-y0)))
du <- with(Su, psp(x0 + (tu-dt)*(x1-x0),
                   y0 + (tu-dt)*(y1-y0),
                   x0 + (tu+dt)*(x1-x0),
                   y0 + (tu+dt)*(y1-y0),
                   window=W))
dv <- with(Sv, psp(x0 + (tv-dt)*(x1-x0),
                   y0 + (tv-dt)*(y1-y0),
                   x0 + (tv+dt)*(x1-x0),
                   y0 + (tv+dt)*(y1-y0),
                   window=W))
dutext <- textstring(u$x, u$y, "du")
dvtext <- textstring(v$x, v$y, "dv")
alpha <- angles.psp(S) * 180/pi
au <- alpha[iu]
av <- alpha[iv]
CorrPic <- layered(simplenet, du, dv, dutext, dvtext,
                   plotargs=list(
                     list(col=grey(0.35), window=FALSE),
                     list(lwd=3), list(lwd=3),
                     list(srt=au, pos=3),
                     list(srt=av, pos=3)))


###################################################
### code chunk number 96: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 97: 17network.Rnw:1766-1767
###################################################
plot(CorrPic, main="")


###################################################
### code chunk number 98: lineardisc
###################################################
L <- domain(chicago)
#xy <- list(x=487,y=670)
xy <- list(x=544,y=583)
dd <- lineardisc(L, x=xy, r=400, plot=FALSE)


###################################################
### code chunk number 99: Chicago.Rnw:3-5
###################################################
newplot(5, 0.75)
setmargins(0)


###################################################
### code chunk number 100: 17network.Rnw:1887-1891
###################################################
plot(L, main="", col=grey(0.35))
points(xy, pch=16, cex=1.5)
plot(dd$lines, add=TRUE, lwd=3)
plot(dd$endpoints, add=TRUE, pch=1)


###################################################
### code chunk number 101: 17network.Rnw:1954-1955
###################################################
attr(linearpcf(spiders), "bw")


###################################################
### code chunk number 102: 17network.Rnw:1970-1971 (eval = FALSE)
###################################################
## plot(envelope(spiders, linearpcf, nsim=199, nrank=5))


###################################################
### code chunk number 103: 17network.Rnw:1978-1981
###################################################
set.seed(1678982)
spidpcf <- anylist(linearpcf(spiders),
                   envelope(spiders, linearpcf, nsim=199, nrank=5))


###################################################
### code chunk number 104: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 105: 17network.Rnw:1986-1987
###################################################
plot(spidpcf, main="", main.panel="", legend=FALSE)


###################################################
### code chunk number 106: 17network.Rnw:2033-2034
###################################################
set.seed(19191978)


###################################################
### code chunk number 107: 17network.Rnw:2036-2040
###################################################
fit <- lppm(spiders ~ polynom(x,y,2))
ghat <- linearpcfinhom(spiders, fit)
genv <- envelope(spiders, linearpcfinhom, lambda=fit, 
                 nsim=199, nrank=5)


###################################################
### code chunk number 108: 17network.Rnw:2046-2047
###################################################
sig <- round(attr(ghat, "bw"))


###################################################
### code chunk number 109: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 110: 17network.Rnw:2052-2053
###################################################
plot(anylist(ghat, genv), main="", main.panel="", legend=FALSE)


###################################################
### code chunk number 111: 17network.Rnw:2095-2097 (eval = FALSE)
###################################################
## X <- rpoislpp(0.004, domain(chicago))
## envKestCSR <- envelope(as.ppp(X), Kest, nsim=39)


###################################################
### code chunk number 112: 17network.Rnw:2108-2111 (eval = FALSE)
###################################################
## L <- domain(chicago)
## envKestL <- envelope(as.ppp(X), Kest, nsim=39, 
##                  simulate=expression(as.ppp(rpoislpp(0.004, L))))


###################################################
### code chunk number 113: 17network.Rnw:2118-2124
###################################################
set.seed(101010)
L <- domain(chicago)
X <- rpoislpp(0.004, L)
EKX22 <- envelope(as.ppp(X), Kest, nsim=39)
EKX21 <- envelope(as.ppp(X), Kest, nsim=39, 
                 simulate=expression(as.ppp(rpoislpp(0.004, L))))


###################################################
### code chunk number 114: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 115: 17network.Rnw:2130-2132
###################################################
plot(anylist(EKX22, EKX21),
     main="", main.panel="", nrows=1, legend=FALSE)


###################################################
### code chunk number 116: 17network.Rnw:2153-2157
###################################################
L <- domain(chicago)
HL <- distcdf(as.owin(L), dW=pixellate(L))
A <- area(as.owin(L))
K0 <- eval.fv(A * HL)


###################################################
### code chunk number 117: 17network.Rnw:2239-2240 (eval = FALSE)
###################################################
## KN <- linearK(spiders, correction="none")


###################################################
### code chunk number 118: 17network.Rnw:2245-2247
###################################################
set.seed(1010101)
EKspOY <- envelope(spiders, linearK, correction="none", nsim=39)


###################################################
### code chunk number 119: 17network.Rnw:2254-2255 (eval = FALSE)
###################################################
## envelope(spiders, linearK, correction="none", nsim=39)


###################################################
### code chunk number 120: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 121: 17network.Rnw:2267-2268
###################################################
plot(EKspOY, main="", legend=FALSE)


###################################################
### code chunk number 122: 17network.Rnw:2407-2408
###################################################
set.seed(1091679)


###################################################
### code chunk number 123: 17network.Rnw:2410-2412
###################################################
kls <- linearK(spiders)
ekls <- envelope(spiders, linearK, nsim=39)


###################################################
### code chunk number 124: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 125: 17network.Rnw:2429-2430
###################################################
plot(ekls, main="", legend=FALSE)


###################################################
### code chunk number 126: 17network.Rnw:2524-2525
###################################################
(fithv <- lppm(spiders ~ vertical))


###################################################
### code chunk number 127: 17network.Rnw:2528-2530
###################################################
Khv <- linearKinhom(spiders, fithv)
Khv <- linearKinhom(spiders, predict(fithv))  # equivalent


###################################################
### code chunk number 128: 17network.Rnw:2540-2541
###################################################
set.seed(18098167)


###################################################
### code chunk number 129: inhomKenv
###################################################
fitv <- lppm(spiders ~ polynom(x,y,2) + vertical)
KiS <- linearKinhom(spiders, fitv)
EKiS <- envelope(spiders, linearKinhom, lambda=fit, nsim=39)
EKiSwrong <- envelope(spiders, linearKinhom, lambda=fit, 
                      nsim=39, update=FALSE)


###################################################
### code chunk number 130: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 131: 17network.Rnw:2558-2559
###################################################
plot(anylist(EKiS, EKiSwrong), main="", main.panel="", legend=FALSE)


###################################################
### code chunk number 132: 17network.Rnw:2644-2645 (eval = FALSE)
###################################################
## plot(alltypes(dendrite, linearKcross))


