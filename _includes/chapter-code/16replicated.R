### R code from vignette source '16replicated.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 16replicated.Rnw:10-12
###################################################
source("R/startup.R")
source("R/short.output.R")


###################################################
### code chunk number 2: 16replicated.Rnw:15-19
###################################################
Terse <- 2
Digits <- 3
spatstat.options(terse=Terse)
options(digits=Digits)


###################################################
### code chunk number 3: 16replicated.Rnw:22-25
###################################################
#requireversion(book, "3.0-2")
requireversion(spatstat, "1.42-2.022")
#requireversion(multippm, "4.1-1")


###################################################
### code chunk number 4: 16replicated.Rnw:227-228
###################################################
spatstat.options(terse=0)


###################################################
### code chunk number 5: 16replicated.Rnw:236-237
###################################################
waterstriders


###################################################
### code chunk number 6: 16replicated.Rnw:243-244
###################################################
spatstat.options(terse=Terse)


###################################################
### code chunk number 7: 16replicated.Rnw:255-256
###################################################
sapply(waterstriders, npoints)


###################################################
### code chunk number 8: 16replicated.Rnw:259-260
###################################################
wins <- lapply(waterstriders, Window)


###################################################
### code chunk number 9: 16replicated.Rnw:281-282
###################################################
solist(rpoispp(100), rpoispp(100))


###################################################
### code chunk number 10: 16replicated.Rnw:299-300
###################################################
Y <- solapply(c(10, 30, 100), rpoispp)


###################################################
### code chunk number 11: 16replicated.Rnw:309-310
###################################################
K <- anylist(Kest(cells), Kest(redwood))


###################################################
### code chunk number 12: 16replicated.Rnw:316-317
###################################################
K <- as.anylist(lapply(waterstriders, Kest))


###################################################
### code chunk number 13: 16replicated.Rnw:324-325
###################################################
K <- anylapply(waterstriders, Kest)


###################################################
### code chunk number 14: 16replicated.Rnw:344-345 (eval = FALSE)
###################################################
## plot(K, main="", main.panel=letters[1:3], legend=FALSE)


###################################################
### code chunk number 15: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 16: 16replicated.Rnw:363-364
###################################################
plot(K, main="", main.panel=letters[1:3], legend=FALSE)


###################################################
### code chunk number 17: 16replicated.Rnw:384-385
###################################################
D <- solapply(split(amacrine), density)


###################################################
### code chunk number 18: Amacrine2E.Rnw:3-5
###################################################
newplot(17,1)
setmargins(0.1)


###################################################
### code chunk number 19: 16replicated.Rnw:390-391
###################################################
plot(D, equal.ribbon=TRUE, main="", mar.panel=c(0,0,1,0), hsep=1)


###################################################
### code chunk number 20: 16replicated.Rnw:404-406 (eval = FALSE)
###################################################
## D <- solapply(split(amacrine), density)
## plot(D, equal.ribbon=TRUE, main="")


###################################################
### code chunk number 21: 16replicated.Rnw:515-520
###################################################
G <- hyperframe(X=c(0.23, 1.76, 3.14), T=anylist(sin,cos,tan),
                Y=letters[1:3], Z=factor(letters[1:3]),
                W=list(rpoispp(100),rpoispp(100), rpoispp(100)),
                U=42, V=rpoispp(100), stringsAsFactors=FALSE)
G


###################################################
### code chunk number 22: 16replicated.Rnw:536-537
###################################################
WS <- hyperframe(Striders=waterstriders)


###################################################
### code chunk number 23: 16replicated.Rnw:560-561
###################################################
G$W


###################################################
### code chunk number 24: 16replicated.Rnw:569-572
###################################################
G$U <- letters[24:26]
G$animal <- c("horse","dog","deer")
G


###################################################
### code chunk number 25: 16replicated.Rnw:575-579
###################################################
WS <- hyperframe()
WS$larvae <- waterstriders
WS$experiment <- factor(1:3)
WS


###################################################
### code chunk number 26: 16replicated.Rnw:593-594
###################################################
spatstat.options(terse=0)


###################################################
### code chunk number 27: 16replicated.Rnw:596-597
###################################################
G[3,2]


###################################################
### code chunk number 28: 16replicated.Rnw:599-600
###################################################
spatstat.options(terse=Terse)


###################################################
### code chunk number 29: 16replicated.Rnw:605-606
###################################################
G[3,2,drop=TRUE]


###################################################
### code chunk number 30: 16replicated.Rnw:629-630
###################################################
G[,2] <- anylist(sqrt,exp,log)


###################################################
### code chunk number 31: 16replicated.Rnw:653-654
###################################################
B <- subset(G, npoints(W) > 100)


###################################################
### code chunk number 32: 16replicated.Rnw:662-663
###################################################
B <- subset(G, minnndist(W) < 0.02, select=Z:V)


###################################################
### code chunk number 33: 16replicated.Rnw:674-675
###################################################
head(pyramidal)


###################################################
### code chunk number 34: 16replicated.Rnw:692-693
###################################################
pg <- split(pyramidal, pyramidal$group)


###################################################
### code chunk number 35: Unit5x2t.Rnw:4-5
###################################################
setmargins(0,0,1,0)


###################################################
### code chunk number 36: 16replicated.Rnw:714-715
###################################################
plot(simba, main="", nrows=2, mar.panel=c(0,0,1,0), hsep=1, vsep=1)


###################################################
### code chunk number 37: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 38: 16replicated.Rnw:736-737 (eval = FALSE)
###################################################
## plot(h, e)


###################################################
### code chunk number 39: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 40: 16replicated.Rnw:743-745 (eval = FALSE)
###################################################
## plot(demohyper, 
##      quote({ plot(Image, main=""); plot(Points, add=TRUE) }))


###################################################
### code chunk number 41: Unit3R.Rnw:3-5
###################################################
newplot(22,0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 42: 16replicated.Rnw:753-756
###################################################
plot(demohyper, 
     quote({ plot(Image, main=""); plot(Points, add=TRUE) }),
     main="", parargs=list(mar=1.2 * c(1,1,1,2)))


###################################################
### code chunk number 43: 16replicated.Rnw:767-769 (eval = FALSE)
###################################################
## H <- hyperframe(Bugs=waterstriders)
## plot(H, quote(plot(Kest(Bugs))))


###################################################
### code chunk number 44: 16replicated.Rnw:787-789
###################################################
df <- data.frame(A=1:10, B=10:1)
with(df, A-B)


###################################################
### code chunk number 45: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 46: 16replicated.Rnw:800-801 (eval = FALSE)
###################################################
## with(h,e)


###################################################
### code chunk number 47: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 48: 16replicated.Rnw:809-812
###################################################
H <- hyperframe(Bugs=waterstriders)
with(H, npoints(Bugs))
D <- with(H, distmap(Bugs))


###################################################
### code chunk number 49: 16replicated.Rnw:846-847
###################################################
simba$Dist <- with(simba, distmap(Points))


###################################################
### code chunk number 50: 16replicated.Rnw:856-857
###################################################
with(simba, npoints(Points))


###################################################
### code chunk number 51: 16replicated.Rnw:862-864
###################################################
H <- hyperframe(Gerris=waterstriders)
K <- with(H, Kest(Gerris))


###################################################
### code chunk number 52: 16replicated.Rnw:869-871
###################################################
H <- hyperframe(Gerris=waterstriders)
m <- with(H, nndist(Gerris))


###################################################
### code chunk number 53: 16replicated.Rnw:876-877
###################################################
with(H, min(nndist(Gerris)))


###################################################
### code chunk number 54: 16replicated.Rnw:882-886
###################################################
set.seed(190105)
lambda <- rexp(3, rate=1/50)
H <- hyperframe(lambda=lambda)
H$Points <- with(H, rpoispp(lambda))


###################################################
### code chunk number 55: 16replicated.Rnw:893-896 (eval = FALSE)
###################################################
## lambda <- rexp(3, rate=1/50)
## H <- hyperframe(lambda=lambda)
## H$Points <- with(H, rpoispp(lambda))


###################################################
### code chunk number 56: 16replicated.Rnw:899-900 (eval = FALSE)
###################################################
## plot(H, quote(plot(Points, main=lambda)))


###################################################
### code chunk number 57: 16replicated.Rnw:903-905 (eval = FALSE)
###################################################
## H$Title <- with(H, parse(text=paste("lambda==", signif(lambda, 3))))
## plot(H, quote(plot(Points, main=Title)))


###################################################
### code chunk number 58: Unit3t.Rnw:3-5
###################################################
newplot(16.1,0.9)
setmargins(0,0,1,0)


###################################################
### code chunk number 59: 16replicated.Rnw:914-915
###################################################
setmargins(0,0,2,0)


###################################################
### code chunk number 60: 16replicated.Rnw:920-926
###################################################
options(digits=6)
plot(H, quote(plot(Points, 
                   main=parse(text=paste("lambda==", signif(lambda, 3))))),
     marsize=1,
     main="")
options(digits=Digits)


###################################################
### code chunk number 61: 16replicated.Rnw:937-938
###################################################
H$X <- with(H, rpoispp(50))


###################################################
### code chunk number 62: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 63: 16replicated.Rnw:1006-1009
###################################################
plot(bdspots, equal.scales=TRUE, 
     cex=0.3, pch=16, main="", main.panel="", 
     mar.panel=0, hsep=0.6)


###################################################
### code chunk number 64: OsteoFullT.Rnw:3-5
###################################################
newplot(9.5, 1)
setmargins(0)


###################################################
### code chunk number 65: 16replicated.Rnw:1055-1057
###################################################
plot(osteo$pts, main="", main.panel=as.character(osteo$shortid), 
     ncols=8, mar.panel=0.1+c(0,0,1,0), pch=21, bg='white')


###################################################
### code chunk number 66: 16replicated.Rnw:1074-1075
###################################################
head(osteo)


###################################################
### code chunk number 67: 16replicated.Rnw:1105-1107
###################################################
pyr <- pyramidal
levels(pyr$group) <- c("c", "af", "ph")


###################################################
### code chunk number 68: Unit4x8t.Rnw:3-5
###################################################
newplot(12, 1)
setmargins(0)


###################################################
### code chunk number 69: 16replicated.Rnw:1112-1115
###################################################
plot(pyr$Neurons, main.panel=pyr$group, main="", 
     ncols=8, mar.panel=0, hsep=1, vsep=2, equal.scales=TRUE,
     pch=16, cex=0.7, cex.main=0.8)


###################################################
### code chunk number 70: Unit2L.Rnw:3-5
###################################################
newplot(13, 0.9)
setmargins(0,4,0,0)


###################################################
### code chunk number 71: 16replicated.Rnw:1210-1218
###################################################
pa <- function(i){ if(i == 1) 
                     list(chars=c(16,3), cex=c(1,0.6), 
                          leg.args=list(cex=1)) else
                   list(chars=c(16,7)) }
plot(flu[c(12, 24), 1], 
     panel.args=pa,
     main="", main.panel="", equal.scales=TRUE,
     mar.panel=0, hsep=1)


###################################################
### code chunk number 72: 16replicated.Rnw:1269-1273
###################################################
py <- pyramidal
py$n <- with(py, npoints(Neurons))
py$area <- with(py, area(Neurons))
py <- as.data.frame(py, warn=FALSE)


###################################################
### code chunk number 73: fvSquat.Rnw:3-5
###################################################
newplot(6, 0.65)
setmargins(0.5+c(3,3,0,0))


###################################################
### code chunk number 74: 16replicated.Rnw:1281-1282
###################################################
setmargins(0.5+c(3,3,0,3))


###################################################
### code chunk number 75: 16replicated.Rnw:1286-1290
###################################################
plot(sqrt(n) ~ group, data=py)
v <- pretty(py$n)
axis(4, at=sqrt(v), labels=v)
mtext(side=4, line=2, expression(italic(n)))


###################################################
### code chunk number 76: 16replicated.Rnw:1314-1315
###################################################
sapply(split(py$n/py$area, py$group), mean)


###################################################
### code chunk number 77: 16replicated.Rnw:1319-1322
###################################################
ntot <- sapply(split(py$n, py$group), sum)
atot <- sapply(split(py$area, py$group), sum)
ntot/atot


###################################################
### code chunk number 78: 16replicated.Rnw:1326-1330
###################################################
fitn <- glm(n ~ offset(log(area)) + group, 
           family=poisson, data=py)
newd <- data.frame(area=1, group=levels(py$group))
predict(fitn, newdata=newd, type="response")


###################################################
### code chunk number 79: 16replicated.Rnw:1335-1336 (eval = FALSE)
###################################################
## anova(fitn, test="LRT")


###################################################
### code chunk number 80: 16replicated.Rnw:1338-1339
###################################################
skipblanklines(anova(fitn, test="LRT"))


###################################################
### code chunk number 81: 16replicated.Rnw:1352-1354
###################################################
py$ce <- with(pyramidal, clarkevans(Neurons, correction="Donnelly"))
sapply(split(py$ce, py$group), mean)


###################################################
### code chunk number 82: 16replicated.Rnw:1363-1365 (eval = FALSE)
###################################################
## py$z <- with(pyramidal, clarkevans.test(Neurons)$statistic)
## anova(lm(z ~ group, data=py))


###################################################
### code chunk number 83: 16replicated.Rnw:1367-1369
###################################################
py$z <- with(pyramidal, clarkevans.test(Neurons)$statistic)
skipblanklines(anova(lm(z ~ group, data=py)))


###################################################
### code chunk number 84: 16replicated.Rnw:1372-1375 (eval = FALSE)
###################################################
## sdf <- as.data.frame(simba, warn=FALSE)
## sdf$z <- with(simba, clarkevans.test(Points)$statistic)
## anova(lm(z ~ group, data=sdf))


###################################################
### code chunk number 85: 16replicated.Rnw:1377-1380
###################################################
sdf <- as.data.frame(simba, warn=FALSE)
sdf$z <- with(simba, clarkevans.test(Points)$statistic)
skipblanklines(anova(lm(z ~ group, data=sdf)))


###################################################
### code chunk number 86: 16replicated.Rnw:1389-1391 (eval = FALSE)
###################################################
## plot(pyramidal, quote(plot(density(Neurons), main=group)))
## plot(with(pyramidal, density(Neurons)))


###################################################
### code chunk number 87: 16replicated.Rnw:1395-1396 (eval = FALSE)
###################################################
## plot(with(demohyper, rhohat(Points, Image)))


###################################################
### code chunk number 88: 16replicated.Rnw:1488-1490
###################################################
Keach <- lapply(waterstriders, Kest, ratio=TRUE)
Keach[[1]]


###################################################
### code chunk number 89: 16replicated.Rnw:1494-1495
###################################################
K <- pool(Keach[[1]], Keach[[2]], Keach[[3]])


###################################################
### code chunk number 90: 16replicated.Rnw:1498-1499
###################################################
K <- pool(as.anylist(Keach))


###################################################
### code chunk number 91: 16replicated.Rnw:1549-1551 (eval = FALSE)
###################################################
## plot(K, cbind(pooliso, pooltheo, loiso, hiiso) ~ r, 
##         shade=c("loiso", "hiiso"))


###################################################
### code chunk number 92: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 93: 16replicated.Rnw:1563-1565
###################################################
plot(K, cbind(pooliso, pooltheo, loiso, hiiso) ~ r, 
     shade=c("loiso", "hiiso"), main="")


###################################################
### code chunk number 94: 16replicated.Rnw:1579-1582
###################################################
os <- osteo
os$K <- with(os, K3est(pts, ratio=TRUE))
Kanimal <- anylapply(split(os$K, os$id), pool)


###################################################
### code chunk number 95: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 96: 16replicated.Rnw:1590-1593
###################################################
plot(Kanimal[-1], cbind(pooliso, pooltheo, loiso, hiiso)/1e4 ~ r, 
     shade=c("loiso", "hiiso"),
     main="", legend=FALSE, nrows=1)


###################################################
### code chunk number 97: 16replicated.Rnw:1614-1620
###################################################
pa <- pyramidal
pa$L <- with(pa, Lest(Neurons))
Lsplit <- split(pa$L, pa$group)
Leach <- anylapply(Lsplit, collapse.fv, 
                    same="theo", different="iso")
Lpool <- anylapply(Lsplit, pool)


###################################################
### code chunk number 98: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 99: 16replicated.Rnw:1626-1630
###################################################
plot(Lpool, 
     cbind(pooliso,pooltheo, hiiso, loiso)-r~ r, 
     shade=c("hiiso", "loiso"), xlim=c(0, 0.2), 
     equal.scales=TRUE, legend=FALSE, main="")


###################################################
### code chunk number 100: 16replicated.Rnw:1643-1649 (eval = FALSE)
###################################################
## pa <- pyramidal
## pa$L <- with(pa, Lest(Neurons))
## Lsplit <- split(pa$L, pa$group)
## Lpool <- anylapply(Lsplit, pool)
## plot(Lpool, cbind(pooliso,pooltheo,hiiso,loiso) - r ~ r, 
##      shade=c("hiiso", "loiso"), xlim=c(0, 0.2), equal.scales=TRUE)


###################################################
### code chunk number 101: 16replicated.Rnw:1657-1660 (eval = FALSE)
###################################################
## Leach <- anylapply(Lsplit, collapse.fv, 
##                     same="theo", different="iso")
## plot(Leach, legend=FALSE, xlim=c(0, 0.2), ylim=c(0, 0.2))


###################################################
### code chunk number 102: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 103: 16replicated.Rnw:1666-1669
###################################################
plot(Leach, xlim=c(0, 0.2), ylim=c(0, 0.2),
     equal.scales=TRUE, legend=FALSE, main="",
     col=1, lty=1)


###################################################
### code chunk number 104: 16replicated.Rnw:1719-1721
###################################################
set.seed(195505)
testpyramidal <- studpermu.test(pyramidal, Neurons ~ group)


###################################################
### code chunk number 105: 16replicated.Rnw:1723-1724 (eval = FALSE)
###################################################
## testpyramidal <- studpermu.test(pyramidal, Neurons ~ group)


###################################################
### code chunk number 106: 16replicated.Rnw:1726-1727
###################################################
testpyramidal


###################################################
### code chunk number 107: 16replicated.Rnw:1755-1761
###################################################
set.seed(12345)
sample1 <- rpoispp(100, nsim=10)
sample2 <- rMaternII(110, 0.02, nsim=10)
patterns <- list(Poisson = sample1, MaternII = sample2)
studpermu.test(patterns, summaryfunction = pcf, nperm=199, 
               interval=c(0,0.15))


###################################################
### code chunk number 108: 16replicated.Rnw:1792-1794
###################################################
set.seed(1809981)
stp <- studpermu.test(pyramidal, summaryfunction = Lest, use.Tbar = TRUE)


###################################################
### code chunk number 109: 16replicated.Rnw:1796-1797 (eval = FALSE)
###################################################
## studpermu.test(pyramidal, summaryfunction = Lest, use.Tbar = TRUE)


###################################################
### code chunk number 110: 16replicated.Rnw:1799-1800
###################################################
stp


###################################################
### code chunk number 111: 16replicated.Rnw:1815-1817
###################################################
waka2 <- split(cut(waka, breaks=c(0,20,200), 
                   labels = c("small", "large")))


###################################################
### code chunk number 112: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 113: 16replicated.Rnw:1823-1824
###################################################
plot(waka2, main="", mar.panel=0, hsep=1, pch=16)


###################################################
### code chunk number 114: 16replicated.Rnw:1839-1841 (eval = FALSE)
###################################################
## waka2 <- split(cut(waka, breaks=c(0,20,200), 
##                    labels = c("small", "large")))


###################################################
### code chunk number 115: 16replicated.Rnw:1844-1845
###################################################
Lwak <- anylapply(waka2, Lest, correction="iso")


###################################################
### code chunk number 116: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 117: 16replicated.Rnw:1855-1863
###################################################
plot(Lwak[[1]], main="",  . - r ~ r, 
  lty = c("dashed", "dotdash"), lwd=c(2,1), col =1,
  ylim=c(-.4, .8),  main="", legend = FALSE)
plot(Lwak[[2]], iso - r ~ r, lwd = 2, add=TRUE)
# legend("topright", 
#       c("big", "small", "Poisson"), 
#       lty=c("solid", "dashed", "dotdash"),
#       lwd=c(2,2,1), bty="n")


###################################################
### code chunk number 118: 16replicated.Rnw:1877-1879
###################################################
subwindows <- quadrats(waka, 3, 3)
treegroups <- lapply(waka2, split, f = subwindows)


###################################################
### code chunk number 119: 16replicated.Rnw:1890-1891
###################################################
set.seed(1989001)


###################################################
### code chunk number 120: 16replicated.Rnw:1893-1895
###################################################
wakatest <- studpermu.test(treegroups, summaryfunction = Lest, 
                           rinterval=c(5,15))


###################################################
### code chunk number 121: 16replicated.Rnw:1900-1901
###################################################
options(digits=5) # print.htest subtracts 3


###################################################
### code chunk number 122: 16replicated.Rnw:1903-1904
###################################################
wakatest


###################################################
### code chunk number 123: 16replicated.Rnw:1906-1907
###################################################
options(digits=Digits)


###################################################
### code chunk number 124: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 125: 16replicated.Rnw:1937-1938
###################################################
plot(wakatest, . - r ~ r, lwd.mean=2)


###################################################
### code chunk number 126: cache
###################################################
sizes <- cut(marks(waka), breaks=c(0,20,200), 
            labels = c("small", "large"))
arbres <- nestsplit(waka, sizes, quadrats(waka, nx=3, ny=3))


###################################################
### code chunk number 127: 16replicated.Rnw:1972-1973 (eval = FALSE)
###################################################
## arbres <- nestsplit(waka, sizes, nx=3, ny=3)


###################################################
### code chunk number 128: 16replicated.Rnw:1976-1977
###################################################
set.seed(195507)


###################################################
### code chunk number 129: 16replicated.Rnw:1979-1980
###################################################
epreuveWaka <- studpermu.test(arbres, pts ~ f1)


###################################################
### code chunk number 130: 16replicated.Rnw:2023-2026
###################################################
bro <- unmark(bronzefilter)
mx <- median(coords(bro)$x)
halves <- chop.tess(Window(bro), infline(v=mx))


###################################################
### code chunk number 131: Bronze.Rnw:3-4
###################################################
setmargins(0)


###################################################
### code chunk number 132: 16replicated.Rnw:2039-2041
###################################################
plot(halves, main="", lwd=2)
plot(bro, add=TRUE, pch=19, cex=0.5)


###################################################
### code chunk number 133: 16replicated.Rnw:2052-2053
###################################################
Ks <- anylapply(split(bro, halves), Kscaled)


###################################################
### code chunk number 134: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 135: 16replicated.Rnw:2060-2063
###################################################
#plot(collapse.fv(left=Ks[[1]], right=Ks[[2]], same="theo", different="iso"),
#     main="")
plot(Ks, main="", main.panel=c("left", "right"), legend=FALSE, xlim=c(0, 1.5))


###################################################
### code chunk number 136: 16replicated.Rnw:2074-2076
###################################################
bronzerho <- rhohat(bro, "x", method="tr")
bronzelambda <- predict(bronzerho)


###################################################
### code chunk number 137: fvSquat.Rnw:3-5
###################################################
newplot(6, 0.65)
setmargins(0.5+c(3,3,0,0))


###################################################
### code chunk number 138: 16replicated.Rnw:2088-2089
###################################################
plot(bronzerho, main="", ylim=c(0, 14))


###################################################
### code chunk number 139: 16replicated.Rnw:2100-2101
###################################################
Ki <- anylapply(split(bro, halves), Kinhom, lambda=bronzelambda)


###################################################
### code chunk number 140: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 141: 16replicated.Rnw:2108-2111
###################################################
#plot(collapse.fv(left=Ki[[1]], right=Ki[[2]], same="theo", different="iso"),
#     main="")
plot(Ki, main="", main.panel=c("left", "right"), legend=FALSE, xlim=c(0,1))


###################################################
### code chunk number 142: 16replicated.Rnw:2134-2135
###################################################
bronze6 <- quantess(bro, "x", 6)


###################################################
### code chunk number 143: Bronze.Rnw:3-4
###################################################
setmargins(0)


###################################################
### code chunk number 144: 16replicated.Rnw:2142-2144
###################################################
plot(bronze6, main="")
plot(bro, add=TRUE, pch=19, cex=0.5)


###################################################
### code chunk number 145: 16replicated.Rnw:2154-2155
###################################################
b63 <- nestsplit(bro, bronze6, ny=3)


###################################################
### code chunk number 146: 16replicated.Rnw:2161-2162
###################################################
head(b63)


###################################################
### code chunk number 147: 16replicated.Rnw:2166-2167
###################################################
b63$inten <- factor(as.integer(b63$f1) <= 3, labels=c("Hi","Lo"))


###################################################
### code chunk number 148: 16replicated.Rnw:2170-2171
###################################################
set.seed(4242401)


###################################################
### code chunk number 149: 16replicated.Rnw:2173-2177
###################################################
locTest <- studpermu.test(b63, pts ~ inten, summaryfunction=Kscaled,
                          rinterval=c(0, 1.5))
corrTest <- studpermu.test(b63, pts ~ inten, summaryfunction=Kinhom,
                          lambda=bronzelambda, rinterval = c(0, 0.7))


###################################################
### code chunk number 150: 16replicated.Rnw:2180-2182
###################################################
locTest
corrTest


###################################################
### code chunk number 151: 16replicated.Rnw:2215-2216
###################################################
mppm(Points ~ 1, simba)


###################################################
### code chunk number 152: 16replicated.Rnw:2238-2239
###################################################
mppm(Points ~ group, simba)


###################################################
### code chunk number 153: 16replicated.Rnw:2243-2244
###################################################
mppm(Points ~ id, simba)


###################################################
### code chunk number 154: 16replicated.Rnw:2249-2250
###################################################
simba2 <- simba[c(FALSE,TRUE), ]


###################################################
### code chunk number 155: 16replicated.Rnw:2258-2259
###################################################
mppm(Points ~ Image, data=demohyper)


###################################################
### code chunk number 156: 16replicated.Rnw:2275-2277
###################################################
mppm(Points ~ Group/Image, data=demohyper)
mppm(Points ~ (Group-1)/Image, data=demohyper)


###################################################
### code chunk number 157: 16replicated.Rnw:2331-2332 (eval = FALSE)
###################################################
## mppm(Points ~ 1, simba, random = ~ 1 | id)


###################################################
### code chunk number 158: 16replicated.Rnw:2355-2367
###################################################
H <- hyperframe(P=waterstriders)
mod <- mppm(P ~ 1, data=H, random=~1|id)
## extract fixed effect intercept 
mu <- fixef(mod)
## extract random effect mean and variance 
ss <- summary(mod)
sigma <- exp(as.numeric(ss$ranef$reStruct$id))
lambda <- exp(mu + sigma^2/2)
#
mu     <- signif(mu, Digits)
sigma  <- signif(sigma, Digits)
lambda <- signif(lambda, Digits)


###################################################
### code chunk number 159: 16replicated.Rnw:2373-2375 (eval = FALSE)
###################################################
## H <- hyperframe(P=waterstriders)
## mppm(P ~ 1, data=H, random=~1|id)


###################################################
### code chunk number 160: 16replicated.Rnw:2377-2378
###################################################
mod


###################################################
### code chunk number 161: 16replicated.Rnw:2400-2401 (eval = FALSE)
###################################################
## mppm(Neurons ~ group, data=pyramidal, random=~1|id)


###################################################
### code chunk number 162: 16replicated.Rnw:2403-2404
###################################################
m <- mppm(Neurons ~ group, data=pyramidal, random=~1|id)


###################################################
### code chunk number 163: 16replicated.Rnw:2406-2408
###################################################
# Don't print all 31 random effects estimates..
short.output(m, 1-10, 36-60, excised="\n [...] \n ")


###################################################
### code chunk number 164: 16replicated.Rnw:2486-2488 (eval = FALSE)
###################################################
## fitpyr <-mppm(Neurons ~ group, data=pyramidal, random=~1|id) 
## anova(fitpyr)


###################################################
### code chunk number 165: 16replicated.Rnw:2490-2491
###################################################
anova(m)


###################################################
### code chunk number 166: 16replicated.Rnw:2524-2525
###################################################
mppm(Points ~ id, data=simba2, interaction=Strauss(0.07))


###################################################
### code chunk number 167: 16replicated.Rnw:2545-2548
###################################################
fiteach <- mppm(Points ~ id, data=simba2, interaction=Strauss(0.07),
                iformula = ~Interaction:id)
fiteach


###################################################
### code chunk number 168: 16replicated.Rnw:2558-2560
###################################################
fitgr <- mppm(Points ~ group, simba2, Strauss(0.07), 
              iformula = ~Interaction:group)


###################################################
### code chunk number 169: 16replicated.Rnw:2572-2573
###################################################
fitgr


###################################################
### code chunk number 170: 16replicated.Rnw:2575-2577
###################################################
co <- coef(fitgr)
si <- function(x) { signif(x, 4) }


###################################################
### code chunk number 171: 16replicated.Rnw:2579-2585
###################################################
# sanity check
expectednames <- c("(Intercept)", 
                   "grouptreatment",
                   "groupcontrol:Interaction",
                   "grouptreatment:Interaction")
stopifnot(identical(names(coef(fitgr)), expectednames))


###################################################
### code chunk number 172: 16replicated.Rnw:2589-2590
###################################################
coef(fitgr)


###################################################
### code chunk number 173: 16replicated.Rnw:2631-2632
###################################################
radii <- with(simba2, mean(nndist(Points)))


###################################################
### code chunk number 174: 16replicated.Rnw:2637-2639
###################################################
Rad <- hyperframe(R=radii)
Str <- with(Rad, Strauss(R))


###################################################
### code chunk number 175: 16replicated.Rnw:2642-2644
###################################################
Int <- hyperframe(str=Str)
mppm(Points ~ 1, simba2, interaction=Int)


###################################################
### code chunk number 176: 16replicated.Rnw:2658-2659 (eval = FALSE)
###################################################
## mppm(Points ~ id, simba2, interaction=Int, iformula = ~str:group)


###################################################
### code chunk number 177: 16replicated.Rnw:2669-2672
###################################################
h <- hyperframe(Y=waterstriders)
g <- hyperframe(po=Poisson(), str4 = Strauss(4), str7= Strauss(7))
mppm(Y ~ 1, data=h, interaction=g, iformula=~str4)


###################################################
### code chunk number 178: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 179: 16replicated.Rnw:2684-2685 (eval = FALSE)
###################################################
## interaction=hyperframe(po=Poisson(), str=Strauss(0.07))


###################################################
### code chunk number 180: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 181: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 182: 16replicated.Rnw:2691-2692 (eval = FALSE)
###################################################
## iformula=~ifelse(group=="control", po, str)


###################################################
### code chunk number 183: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 184: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 185: 16replicated.Rnw:2705-2706 (eval = FALSE)
###################################################
## iformula=~I((group=="control")*po) + I((group=="treatment") * str)


###################################################
### code chunk number 186: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 187: 16replicated.Rnw:2716-2721
###################################################
g <- hyperframe(po=Poisson(), str=Strauss(0.07))
fit2 <- mppm(Points ~ 1, simba2, g, 
             iformula=~I((group=="control")*po) 
                     + I((group=="treatment") * str))
fit2


###################################################
### code chunk number 188: 16replicated.Rnw:2731-2735
###################################################
fit2a <- mppm(Points ~ 1, simba2, g, 
             iformula=~I((group=="treatment") * str))
fit2b <- mppm(Points ~ 1, simba2, Strauss(0.07), 
             iformula=~I((group=="treatment") * Interaction))


###################################################
### code chunk number 189: 16replicated.Rnw:2756-2758
###################################################
## temporarily suppress standard errors etc
op <- spatstat.options(print.ppm.SE="never")


###################################################
### code chunk number 190: 16replicated.Rnw:2766-2769
###################################################
H <- hyperframe(W=waterstriders)
fit <- mppm(W ~ 1, H)
subfits(fit)


###################################################
### code chunk number 191: 16replicated.Rnw:2794-2796
###################################################
fitI <- mppm(W ~ id, H)
subI <- subfits(fitI)


###################################################
### code chunk number 192: 16replicated.Rnw:2799-2800
###################################################
subII <- with(H, ppm(W ~ 1))


###################################################
### code chunk number 193: 16replicated.Rnw:2805-2807
###################################################
## reinstate standard errors
spatstat.options(op)


###################################################
### code chunk number 194: 16replicated.Rnw:2821-2823
###################################################
fit <- mppm(P ~ x, hyperframe(P=waterstriders))
res <- residuals(fit, type="Pearson")


###################################################
### code chunk number 195: 16replicated.Rnw:2832-2833
###################################################
smor <- with(hyperframe(res=res), Smooth(res, sigma=4))


###################################################
### code chunk number 196: Unit3R.Rnw:3-5
###################################################
newplot(22,0.9)
setmargins(0,0,0,1)


###################################################
### code chunk number 197: 16replicated.Rnw:2843-2844
###################################################
plot(smor, main="", main.panel="", equal.ribbon=TRUE)


###################################################
### code chunk number 198: 16replicated.Rnw:2857-2858
###################################################
sapply(res, integral.msr)


###################################################
### code chunk number 199: 16replicated.Rnw:2862-2866
###################################################
mod <- mppm(Neurons ~ group * x, data=pyramidal)
res <- residuals(mod, type="raw")
df <- as.data.frame(pyramidal, warn=FALSE)
df$resid <- sapply(res, integral.msr)


###################################################
### code chunk number 200: 16replicated.Rnw:2868-2869 (eval = FALSE)
###################################################
## plot(resid ~ group, df)


###################################################
### code chunk number 201: fvSquat.Rnw:3-5
###################################################
newplot(6, 0.65)
setmargins(0.5+c(3,3,0,0))


###################################################
### code chunk number 202: 16replicated.Rnw:2876-2877
###################################################
plot(resid~group, df)


###################################################
### code chunk number 203: 16replicated.Rnw:2891-2893
###################################################
fit <- mppm(P ~ 1, hyperframe(P=waterstriders))
subs <- hyperframe(Model=subfits(fit))


###################################################
### code chunk number 204: 16replicated.Rnw:2895-2896 (eval = FALSE)
###################################################
## plot(subs, quote(diagnose.ppm(Model)))


###################################################
### code chunk number 205: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 206: 16replicated.Rnw:2908-2911
###################################################
plot(subs, 
     quote(diagnose.ppm(Model, plot.neg="contour", xlab="", ylab="", rlab="")),
     main="")


###################################################
### code chunk number 207: 16replicated.Rnw:2927-2935
###################################################
H <- hyperframe(P = waterstriders)
fitall <- mppm(P ~ 1, H)
together <- subfits(fitall)
separate <- with(H, ppm(P))
Fits <- hyperframe(Together=together, Separate=separate)
dr <- with(Fits, unlist(coef(Separate)) - unlist(coef(Together)))
dr
exp(dr)


###################################################
### code chunk number 208: 16replicated.Rnw:2951-2958
###################################################
H <- hyperframe(X=waterstriders)
# Poisson with constant intensity for all patterns
fit1 <- mppm(X~1, H)
quadrat.test(fit1, nx=2)
# uniform Poisson with different intensity for each pattern
fit2 <- mppm(X ~ id, H)
quadrat.test(fit2, nx=2)


###################################################
### code chunk number 209: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 210: 16replicated.Rnw:2979-2980 (eval = FALSE)
###################################################
## cdf.test(model, covariate, test)


###################################################
### code chunk number 211: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


