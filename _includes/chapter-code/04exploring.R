### R code from vignette source '04exploring.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 04exploring.Rnw:9-12
###################################################
source("R/startup.R")
Digits <- 4
options(digits=Digits)


###################################################
### code chunk number 2: 04exploring.Rnw:104-108
###################################################
lon <- rescale(longleaf, 200)
ham <- hamster[c(TRUE, FALSE, FALSE)]
zeromargins()
setmargins(0.05)


###################################################
### code chunk number 3: 04exploring.Rnw:112-114
###################################################
plot(solist(japanesepines, ham, lon), main="", main.panel="",
     equal.scales=TRUE, mar.panel=0, hsep=2)


###################################################
### code chunk number 4: 04exploring.Rnw:184-185
###################################################
setmargins(0)


###################################################
### code chunk number 5: 04exploring.Rnw:189-190
###################################################
plot(finpines, main="", mar.panel=c(0,0.1,0.1,0), hsep=2.5)


###################################################
### code chunk number 6: 04exploring.Rnw:207-211
###################################################
symbolpars <- get("known.unknowns", environment(symbolmap))
listpars <- function(s) { 
  UScommasep(paste0("\\\\texttt", paren(s, "{")))
}


###################################################
### code chunk number 7: 04exploring.Rnw:218-221
###################################################
a <- plot(amacrine)
a
(plot(longleaf))


###################################################
### code chunk number 8: 04exploring.Rnw:242-244 (eval = FALSE)
###################################################
## A <- colourmap(heat.colors(128), range=range(marks(longleaf)))
## plot(longleaf, pch=21, bg=A, cex=1)


###################################################
### code chunk number 9: UnitL.Rnw:3-5
###################################################
newplot(9, 0.7)
setmargins(0.1+ c(0,2,0,0))


###################################################
### code chunk number 10: 04exploring.Rnw:257-260
###################################################
coco <- if(monochrome) grey(seq(0,1,length=128)) else heat.colors(128)
A <- colourmap(coco, range=range(marks(longleaf)))
plot(longleaf, pch=21, bg=A, cex=1, main="")


###################################################
### code chunk number 11: 04exploring.Rnw:275-278 (eval = FALSE)
###################################################
## juveniles <- subset(longleaf, marks <= 30)
## a <- plot(longleaf, maxsize=15)
## plot(juveniles, symap=a)


###################################################
### code chunk number 12: Unit2L.Rnw:3-5
###################################################
newplot(13, 0.9)
setmargins(0,4,0,0)


###################################################
### code chunk number 13: 04exploring.Rnw:282-283
###################################################
setmargins(0)


###################################################
### code chunk number 14: 04exploring.Rnw:287-291
###################################################
juveniles <- subset(longleaf, marks <= 30)
a <- plot(longleaf, markscale=0.25, do.plot=FALSE)
plot(solist(longleaf, juveniles), symap=a,
     main="", main.panel="", mar.panel=0, hsep=1.5, equal.scales=TRUE)


###################################################
### code chunk number 15: 04exploring.Rnw:303-305
###################################################
g1 <- symbolmap(inputs=letters[1:10], pch=11:20)
g2 <- symbolmap(range=c(0,100), size=function(x) {x/50})


###################################################
### code chunk number 16: 04exploring.Rnw:309-312
###################################################
g3 <- update(g2, col="red")
g4 <- update(g3, col=function(x) ifelse(x < 30, "red", "black"))
g4


###################################################
### code chunk number 17: 04exploring.Rnw:332-333 (eval = FALSE)
###################################################
## plot(murchison$greenstone, col="grey")


###################################################
### code chunk number 18: MurchGreen.Rnw:3-5
###################################################
newplot(6, 0.4)
zeromargins()


###################################################
### code chunk number 19: 04exploring.Rnw:340-341
###################################################
plot(murchison$greenstone, main = "", col="grey")


###################################################
### code chunk number 20: 04exploring.Rnw:361-364 (eval = FALSE)
###################################################
## plot(square(c(-1,1)), main = "")
## plot(ellipse(1,0.5), col = rgb(0,0,0,.2), add = TRUE)
## plot(ellipse(0.5,1), col = rgb(0,0,0,.2), add = TRUE)


###################################################
### code chunk number 21: 04exploring.Rnw:422-423
###################################################
setmargins(0)


###################################################
### code chunk number 22: 04exploring.Rnw:427-435
###################################################
Y <- bei.extra[["elev"]]
par(mfrow=c(1,2), mar=c(0,0,0,3))
plot(Y, main="", ribargs=list(las=1))
contour(Y, add=TRUE, nlevels=6)
par(mar=c(0,3,0,0))
persp(Y, main="", theta=20, phi=30, border=NA, shade=0.6,
      zlab="elevation")
par(mfrow=c(1,1))


###################################################
### code chunk number 23: 04exploring.Rnw:516-519
###################################################
g <- colourmap(rainbow(128), range=c(0,100))
h <- colourmap(c("green", "yellow", "red"), 
               inputs=c("Low", "Medium", "High"))


###################################################
### code chunk number 24: 04exploring.Rnw:526-528
###################################################
g(50)
h("Medium")


###################################################
### code chunk number 25: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 26: 04exploring.Rnw:540-544 (eval = FALSE)
###################################################
## ra <- range(X, Y)
## cm <- colourmap(rainbow(128), range=ra)
## plot(X, col=cm)
## plot(Y, col=cm)


###################################################
### code chunk number 27: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 28: 04exploring.Rnw:615-618 (eval = FALSE)
###################################################
## persp(bei.extra$elev, expand=6, theta=-30, phi=20,
##       colmap=terrain.colors(128), shade=0.2, 
##       apron=TRUE, main="", box=FALSE)


###################################################
### code chunk number 29: 04exploring.Rnw:627-631 (eval = FALSE)
###################################################
## M <- persp(bei.extra$elev, theta=-45, phi=18, expand=7,
##            border=NA, apron=TRUE, shade=0.3, 
##            box=FALSE, visible=TRUE)
## perspPoints(bei, Z=bei.extra$elev, M=M, pch=16)


###################################################
### code chunk number 30: 04exploring.Rnw:641-642
###################################################
setmargins(0)


###################################################
### code chunk number 31: 04exploring.Rnw:646-652
###################################################
M <- persp(bei.extra$elev, 
           theta=-45, phi=18, expand=7,
           border=NA, apron=TRUE, shade=0.3, 
           box=FALSE, visible=TRUE,
           main="")
perspPoints(bei, Z=bei.extra$elev, M=M, pch=16, cex=0.3)


###################################################
### code chunk number 32: 04exploring.Rnw:671-672
###################################################
setmargins(3,4,0.2,0.2)


###################################################
### code chunk number 33: 04exploring.Rnw:676-677
###################################################
with(bei.extra, plot(transect.im(elev), main=""))


###################################################
### code chunk number 34: 04exploring.Rnw:699-700
###################################################
U <- cut(bei.extra$elev, 4, labels=c("lo","mlo","mhi","hi"))


###################################################
### code chunk number 35: Bei2R.Rnw:3-5
###################################################
newplot(10,1)
setmargins(0.1)


###################################################
### code chunk number 36: 04exploring.Rnw:713-714
###################################################
setmargins(0,0,0,2)


###################################################
### code chunk number 37: 04exploring.Rnw:718-722
###################################################
par(mfrow=c(1,2))
plot(U, main="", ribargs=list(las=1))
textureplot(U, main="")
par(mfrow=c(1,1))


###################################################
### code chunk number 38: 04exploring.Rnw:752-754 (eval = FALSE)
###################################################
## with(bei.extra, hist(grad, probability=TRUE))
## with(bei.extra, plot(spatialcdf(grad, normalise=TRUE)))


###################################################
### code chunk number 39: 04exploring.Rnw:761-762
###################################################
setmargins(0.3, 0.3, 0.1, 0.1)


###################################################
### code chunk number 40: 04exploring.Rnw:766-774
###################################################
par(font.lab=1)
a <- with(bei.extra, hist(grad, plot=FALSE))
b <- with(bei.extra, spatialcdf(grad, normalise=TRUE))
pa <- function(i) {
  if(i == 1) list(freq=FALSE) else list(ylab="Cumulative probability")
}
plot(anylist(a,b), main="", main.panel="", 
     mar.panel=c(3.2,3,0.1,0.1), hsep=1.5, panel.args=pa)


###################################################
### code chunk number 41: 04exploring.Rnw:809-811 (eval = FALSE)
###################################################
## plot(amacrine)
## identify(amacrine)


###################################################
### code chunk number 42: UnitR.Rnw:3-6
###################################################
newplot(9, 0.7)
zeromargins() # strip all margins
setmargins(0.1 + c(0,0,0,2)) # back off


###################################################
### code chunk number 43: 04exploring.Rnw:880-881
###################################################
setmargins(0,0,0,3)


###################################################
### code chunk number 44: 04exploring.Rnw:885-888
###################################################
X <- layered(density(cells), cells)
layerplotargs(X)[[2]] <- list(pch=16)
plot(X, main="")


###################################################
### code chunk number 45: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 46: 04exploring.Rnw:911-912 (eval = FALSE)
###################################################
## X <- layered(..., plotargs=p)


###################################################
### code chunk number 47: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 48: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 49: 04exploring.Rnw:917-919 (eval = FALSE)
###################################################
## X <- layered(...)
## layerplotargs(X) <- p


###################################################
### code chunk number 50: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 51: 04exploring.Rnw:929-932 (eval = FALSE)
###################################################
## X <- layered(density(cells), cells)
## layerplotargs(X)[[2]] <- list(pch=16)
## plot(X, main="")


###################################################
### code chunk number 52: 04exploring.Rnw:967-974
###################################################
X  <- swedishpines
QC <- quadratcount(X)
QCI <- as.im(X,dimyx=5)
DI <- density(X)
L <- solist(X, QC, QCI, DI)
names(L) <- c("Swedish Pines pattern", "Quadrat counts",
               "Quadrat count image", "Estimated intensity")


###################################################
### code chunk number 53: 04exploring.Rnw:980-981
###################################################
setmargins(0.2)


###################################################
### code chunk number 54: 04exploring.Rnw:985-990
###################################################
pa <- function(i) { if(i > 2) list(ribargs=list(las=1)) else list() }
plot(L,main="",
     equal.scales=TRUE, halign=TRUE, valign=TRUE,
     panel.args=pa,
     mar.panel=0.2, hsep=2, vsep=1)


###################################################
### code chunk number 55: 04exploring.Rnw:1011-1014 (eval = FALSE)
###################################################
## P <- solist(A=cells, B=japanesepines, C=redwood)
## plot(P, equal.scales=TRUE)
## plot(P, equal.scales=TRUE, valign=TRUE)


###################################################
### code chunk number 56: 04exploring.Rnw:1106-1107
###################################################
setmargins(0.5)


###################################################
### code chunk number 57: 04exploring.Rnw:1111-1112
###################################################
pairs(density(split(lansing))[c(2,3,5)])


###################################################
### code chunk number 58: 04exploring.Rnw:1131-1132 (eval = FALSE)
###################################################
## pairs(density(split(lansing)[c(2,3,5)]))


###################################################
### code chunk number 59: 04exploring.Rnw:1166-1170
###################################################
L <- density(split(lansing)[c(2,3,5)])
df <- pairs(L, plot=FALSE)
co <- cor(df)
round(co, 2)


###################################################
### code chunk number 60: 04exploring.Rnw:1260-1261
###################################################
setmargins(0)


###################################################
### code chunk number 61: 04exploring.Rnw:1265-1272
###################################################
X <- unmark(demopat)
Frame(X) <- grow.rectangle(Frame(X), 500)
plot(Frame(X), main="", lty=1)
plot(Window(X), add=TRUE, col="grey")
plot(boundingbox(X), add=TRUE, lty=3)
plot(boundingbox(Window(X)), add=TRUE, lty=2)
plot(X, add=TRUE, pch=16)


###################################################
### code chunk number 62: 04exploring.Rnw:1301-1303
###################################################
X <- redwood
marks(X) <- nndist(X)


###################################################
### code chunk number 63: 04exploring.Rnw:1319-1321
###################################################
Y <- chorley
levels(marks(Y)) <- c("case", "control")


###################################################
### code chunk number 64: 04exploring.Rnw:1324-1327
###################################################
m <- marks(Y)
levels(m) <- c("case", "control")
marks(Y) <- m


###################################################
### code chunk number 65: 04exploring.Rnw:1333-1336 (eval = FALSE)
###################################################
## m <- marks(X)
## m[3] <- 5
## marks(X) <- m


###################################################
### code chunk number 66: 04exploring.Rnw:1352-1353 (eval = FALSE)
###################################################
## marks(redwood) <- nndist(redwood)


###################################################
### code chunk number 67: 04exploring.Rnw:1424-1426
###################################################
W <- Window(clmfires)
U <- simplify.owin(W,10) # Gives a polygon with about 200 edges.


###################################################
### code chunk number 68: Unit2.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 69: 04exploring.Rnw:1434-1436
###################################################
plot(solist(W,U),
     main="", main.panel="", equal.scales=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 70: 04exploring.Rnw:1465-1467
###################################################
bei
bei[1:10]


###################################################
### code chunk number 71: 04exploring.Rnw:1481-1482 (eval = FALSE)
###################################################
## bei[-c(2,3,7)]


###################################################
### code chunk number 72: 04exploring.Rnw:1491-1492 (eval = FALSE)
###################################################
## swedishpines[nndist(swedishpines) > 10]


###################################################
### code chunk number 73: 04exploring.Rnw:1497-1498 (eval = FALSE)
###################################################
## longleaf[marks(longleaf) >= 42]


###################################################
### code chunk number 74: 04exploring.Rnw:1506-1507 (eval = FALSE)
###################################################
## longleaf[c(FALSE,TRUE)]


###################################################
### code chunk number 75: 04exploring.Rnw:1531-1534
###################################################
W <- owin(c(100,800), c(100,400))
W
bei[W]


###################################################
### code chunk number 76: 04exploring.Rnw:1549-1550 (eval = FALSE)
###################################################
## subset(cells, x > 0.5 & y < 0.4)


###################################################
### code chunk number 77: 04exploring.Rnw:1558-1559 (eval = FALSE)
###################################################
## subset(longleaf, marks >= 42)


###################################################
### code chunk number 78: 04exploring.Rnw:1565-1566 (eval = FALSE)
###################################################
## subset(finpines, diameter > 2 & height < 4)


###################################################
### code chunk number 79: 04exploring.Rnw:1575-1576 (eval = FALSE)
###################################################
## subset(finpines, diameter > 2, select=height)


###################################################
### code chunk number 80: 04exploring.Rnw:1581-1582 (eval = FALSE)
###################################################
## subset(nbfires, year == 1999, select=cause:fnl.size)


###################################################
### code chunk number 81: 04exploring.Rnw:1588-1589 (eval = FALSE)
###################################################
## subset(finpines, select = -height)


###################################################
### code chunk number 82: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 83: 04exploring.Rnw:1605-1606 (eval = FALSE)
###################################################
## marks(X) <- NULL


###################################################
### code chunk number 84: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 85: 04exploring.Rnw:1613-1616 (eval = FALSE)
###################################################
## plot(unmark(anemones))
## radii <- rexp(npoints(redwood), rate=10)
## plot(redwood %mark% radii)


###################################################
### code chunk number 86: 04exploring.Rnw:1627-1629
###################################################
elev <- bei.extra$elev
Y <- bei %mark% elev[bei]


###################################################
### code chunk number 87: 04exploring.Rnw:1634-1639 (eval = FALSE)
###################################################
## X <- amacrine
## marks(X) <- data.frame(type=marks(X), nn=nndist(amacrine))
## Y <- finpines
## vol <- with(marks(Y), (100 * pi/12) * height * diameter^2)
## marks(Y) <- cbind(marks(Y), volume=vol)


###################################################
### code chunk number 88: 04exploring.Rnw:1649-1651
###################################################
Y <- cut(longleaf, breaks=c(0, 5, 20, Inf))
Y


###################################################
### code chunk number 89: 04exploring.Rnw:1657-1659
###################################################
Y <- cut(longleaf, breaks=3)
Y


###################################################
### code chunk number 90: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 91: 04exploring.Rnw:1682-1683 (eval = FALSE)
###################################################
## Y <- rescale(X, s)


###################################################
### code chunk number 92: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 93: 04exploring.Rnw:1716-1717
###################################################
rescale(lansing)


###################################################
### code chunk number 94: 04exploring.Rnw:1731-1732
###################################################
murch2 <- solapply(murchison, rescale, s=1000, unitname="km")


###################################################
### code chunk number 95: 04exploring.Rnw:1772-1773 (eval = FALSE)
###################################################
## rotate(chorley, pi/2, centre="centroid")


###################################################
### code chunk number 96: Amacrine2title.Rnw:3-5
###################################################
newplot(9,0.95)
setmargins(0, 0, 1, 0)


###################################################
### code chunk number 97: 04exploring.Rnw:2015-2016
###################################################
setmargins(0)


###################################################
### code chunk number 98: 04exploring.Rnw:2020-2021
###################################################
plot(split(amacrine), main="", mar.panel=0.1, hsep=1)


###################################################
### code chunk number 99: 04exploring.Rnw:2033-2034 (eval = FALSE)
###################################################
## Y <- split(nbfires, "cause")


###################################################
### code chunk number 100: 04exploring.Rnw:2043-2045 (eval = FALSE)
###################################################
## V <- split(lansing)
## A <- solapply(V, adaptive.density)


###################################################
### code chunk number 101: 04exploring.Rnw:2053-2055 (eval = FALSE)
###################################################
## A <- by(lansing, FUN=adaptive.density)
## plot(A)


###################################################
### code chunk number 102: 04exploring.Rnw:2069-2072
###################################################
S <- split(chorley)
cases <- S$larynx
controls <- S$lung


###################################################
### code chunk number 103: 04exploring.Rnw:2091-2095
###################################################
X <- ants
u <- split(X)
u$Messor <- rjitter(u$Messor)
split(X) <- u


###################################################
### code chunk number 104: 04exploring.Rnw:2116-2117
###################################################
X <- superimpose(u$Messor, u$Cataglyphis)


###################################################
### code chunk number 105: 04exploring.Rnw:2123-2124
###################################################
X <- superimpose(Cataglyphis=u$Cataglyphis, Messor=u$Messor)


###################################################
### code chunk number 106: 04exploring.Rnw:2138-2141
###################################################
X <- runifpoint(50, square(c(0,2)))
Y <- runifpoint(50, square(c(1,3)))
superimpose(X,Y)


###################################################
### code chunk number 107: 04exploring.Rnw:2144-2145
###################################################
superimpose(X, Y, W=square(3))


###################################################
### code chunk number 108: 04exploring.Rnw:2197-2198
###################################################
summary(chorley)


###################################################
### code chunk number 109: nztrees2.Rnw:3-5
###################################################
newplot(10.5, 0.95)
setmargins(0)


###################################################
### code chunk number 110: 04exploring.Rnw:2319-2326
###################################################
f <- function(x, ..., do.plot=TRUE) {
  if(is.ppp(x)) plot(x, ..., do.plot=do.plot) else 
  contour(x, ..., axes=FALSE, do.plot=do.plot)
}
plot(solist(nztrees, density(nztrees, 10)),
     main="", main.panel="", plotcommand=f, 
     equal.scales=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 111: 04exploring.Rnw:2338-2339 (eval = FALSE)
###################################################
## contour(density(nztrees, 10), axes=FALSE)


###################################################
### code chunk number 112: HistSquat.Rnw:3-5
###################################################
newplot(6, 0.6)
setmargins(3,3,4,1)


###################################################
### code chunk number 113: 04exploring.Rnw:2359-2360
###################################################
setmargins(3, 3.5, 0, 0.2)


###################################################
### code chunk number 114: 04exploring.Rnw:2364-2366
###################################################
par(font.lab=1)
hist(coords(nztrees)$x, nclass=25, main="", col="grey")


###################################################
### code chunk number 115: 04exploring.Rnw:2377-2378
###################################################
chopped <- owin(c(0,148),c(0,95))


###################################################
### code chunk number 116: 04exploring.Rnw:2381-2384
###################################################
win <- Window(nztrees)
chopped <- trim.rectangle(win, xmargin=c(0,5), ymargin=0)
chopped


###################################################
### code chunk number 117: 04exploring.Rnw:2392-2393
###################################################
nzchop <- nztrees[chopped]


###################################################
### code chunk number 118: 04exploring.Rnw:2396-2397
###################################################
summary(nzchop)


###################################################
### code chunk number 119: 04exploring.Rnw:2399-2401 (eval = FALSE)
###################################################
## plot(density(nzchop, 10))
## plot(nzchop, add=TRUE)


###################################################
### code chunk number 120: nztreesR.Rnw:3-5
###################################################
newplot(7, 0.7)
setmargins(0, 0, 0, 1)


###################################################
### code chunk number 121: 04exploring.Rnw:2411-2413
###################################################
plot(density(nzchop, 10), main="")
plot(nzchop, add=TRUE, pch=16, etch=TRUE)


###################################################
### code chunk number 122: 04exploring.Rnw:2449-2465
###################################################
## set up example pixel image
W <- Window(demopat)
Frame(W) <- grow.rectangle(Frame(W), 1500)
Z <- as.im(function(x,y) { sqrt(x) - sqrt(y)}, W, eps=500)
xbreaks <- (Z$xcol[-1] + Z$xcol[-ncol(Z)])/2
ybreaks <- (Z$yrow[-1] + Z$yrow[-nrow(Z)])/2
vlines <- psp(xbreaks, rep(Z$yrange[1], length(xbreaks)), 
              xbreaks, rep(Z$yrange[2], length(xbreaks)),
              window=Frame(Z))
hlines <- psp(rep(Z$xrange[1], length(ybreaks)), ybreaks, 
              rep(Z$xrange[2], length(ybreaks)), ybreaks, 
              window=Frame(Z))
thelines <- superimpose(vlines, hlines, W=Frame(Z))
inlines <- thelines[Window(Z)]
outlines <- thelines[complement.owin(Window(Z))]
nays <- pixelcentres(complement.owin(Window(Z)))


###################################################
### code chunk number 123: 04exploring.Rnw:2469-2470
###################################################
setmargins(0)


###################################################
### code chunk number 124: 04exploring.Rnw:2474-2481
###################################################
plot(Z, main="", ribbon=FALSE)
plot(nays, pch=4, add=TRUE)
plot(inlines, add=TRUE, col="white")
plot(outlines, add=TRUE)
plot(as.polygonal(Window(Z)), add=TRUE, lwd=6)
plot(Frame(Z), add=TRUE, lwd=2)
#with(Z,points(xcol[22],yrow[4],pch=8,cex=0.8))


###################################################
### code chunk number 125: 04exploring.Rnw:2626-2628
###################################################
elev <- bei.extra$elev
elev[list(x=142,y=356)]


###################################################
### code chunk number 126: 04exploring.Rnw:2633-2635 (eval = FALSE)
###################################################
## plot(elev)
## elev[locator(1)]


###################################################
### code chunk number 127: 04exploring.Rnw:2643-2645 (eval = FALSE)
###################################################
## S <- owin(c(200,300), c(100,200))
## plot(elev[S])


###################################################
### code chunk number 128: 04exploring.Rnw:2648-2651 (eval = FALSE)
###################################################
## plot(elev)
## S <- clickpoly(add=TRUE)
## plot(elev[S, drop=FALSE, tight=TRUE])


###################################################
### code chunk number 129: 04exploring.Rnw:2656-2663
###################################################
S <- dget("data/subWinElev.dput")
V <- as.ppp(vertices(S), S, check=FALSE)
A <- layered(elev, S, V)
layerplotargs(A)[[1]] <- list(ribbon=FALSE)
B <- layered(elev[S,tight=TRUE,drop=FALSE])
cmap <- plot(elev, do.plot=FALSE)
layerplotargs(B)[[1]] <- list(ribbon=FALSE, col=cmap)


###################################################
### code chunk number 130: 04exploring.Rnw:2666-2667
###################################################
setmargins(0)


###################################################
### code chunk number 131: 04exploring.Rnw:2671-2673
###################################################
plot(solist(A,B), main="", main.panel="", 
     equal.scales=FALSE, mar.panel=0, hsep=1)


###################################################
### code chunk number 132: 04exploring.Rnw:2708-2710
###################################################
Z <- distmap(cells)
Z[] <- Z[] + 1


###################################################
### code chunk number 133: 04exploring.Rnw:2721-2722 (eval = FALSE)
###################################################
## Y <- eval.im(Z - 1.1)


###################################################
### code chunk number 134: 04exploring.Rnw:2730-2731 (eval = FALSE)
###################################################
## X <- eval.im(Z + Y)


###################################################
### code chunk number 135: 04exploring.Rnw:2736-2739 (eval = FALSE)
###################################################
## eval.im(sqrt(Z))
## eval.im(sin(pi * Z))
## eval.im(Z > 3)


###################################################
### code chunk number 136: 04exploring.Rnw:2742-2743 (eval = FALSE)
###################################################
## eval.im(log(X) + Y - 3)


###################################################
### code chunk number 137: 04exploring.Rnw:2747-2749 (eval = FALSE)
###################################################
## W <- eval.im(max(0,Z))  # Throws an error.
## W <- eval.im(pmax(0,Z)) # Works.


###################################################
### code chunk number 138: 04exploring.Rnw:2754-2756 (eval = FALSE)
###################################################
## eval.im(if(X < 3) 3 else 1)   # Throws an error.
## eval.im(ifelse(X < 3, 3, 1))  # Works.


###################################################
### code chunk number 139: 04exploring.Rnw:2764-2765 (eval = FALSE)
###################################################
## logY <- eval.im(log(Y))


###################################################
### code chunk number 140: 04exploring.Rnw:2779-2780 (eval = FALSE)
###################################################
## eval.im(density(cells) - 3)  # Throws an error


###################################################
### code chunk number 141: 04exploring.Rnw:2785-2787 (eval = FALSE)
###################################################
## dcells <- density(cells)
## eval.im(dcells - 3) # Works


###################################################
### code chunk number 142: 04exploring.Rnw:2791-2792 (eval = FALSE)
###################################################
## eval.im(Z - 3, envir=list(Z=density(cells)))


###################################################
### code chunk number 143: 04exploring.Rnw:2810-2812 (eval = FALSE)
###################################################
## g <- ecdf(Z)  # Remember that g is a *function*.
## Y <- eval.im(g(Z))


###################################################
### code chunk number 144: 04exploring.Rnw:2834-2838 (eval = FALSE)
###################################################
## Y <- Z+3
## X <- Z+Y
## W <- log(Z)
## U <- sin(pi*Z)


###################################################
### code chunk number 145: 04exploring.Rnw:2845-2846
###################################################
Y <- density(cells) - 3


###################################################
### code chunk number 146: 04exploring.Rnw:2932-2934
###################################################
elev <- bei.extra$elev
W <- levelset(elev, 145, ">")


###################################################
### code chunk number 147: 04exploring.Rnw:2950-2952
###################################################
grad <- bei.extra$grad
V <- solutionset(elev <= 140 & grad > 0.1)


###################################################
### code chunk number 148: 04exploring.Rnw:2965-2968
###################################################
plot(solist(layered(W, Frame(W)),
            layered(V, Frame(V))), 
     main="", main.panel="",mar.panel=0, hsep=1, box=TRUE)


###################################################
### code chunk number 149: 04exploring.Rnw:2976-2981
###################################################
BW <- layered(W, bei[W])
BV <- layered(V, bei[V])
layerplotargs(BW) <- layerplotargs(BV) <- 
    list(list(invert=TRUE, col="grey"),
         list(pch=3, cex=0.3, cols="black"))


###################################################
### code chunk number 150: 04exploring.Rnw:2987-2988
###################################################
plot(solist(BW, BV), main="", main.panel="", mar.panel=0, hsep=1)


###################################################
### code chunk number 151: 04exploring.Rnw:3077-3078 (eval = FALSE)
###################################################
## ls(pos="package:spatstat",pattern="\\.psp$")


###################################################
### code chunk number 152: 04exploring.Rnw:3128-3131
###################################################
v <- as.im(function(x,y){factor(round(3 * (x^2 + y^2)))}, W=owin())
levels(v) <- letters[seq(length(levels(v)))]
ex.tess <- tess(image=v)


###################################################
### code chunk number 153: 04exploring.Rnw:3135-3136
###################################################
setmargins(0.05)


###################################################
### code chunk number 154: 04exploring.Rnw:3140-3144
###################################################
plot(solist(quadrats(owin(), 4, 4), 
	    dirichlet(runifpoint(15)),
	    layered(ex.tess, plotargs=list(legend=FALSE))),
     main="", main.panel="", mar.panel=0, hsep=1, equal.scales=TRUE)


###################################################
### code chunk number 155: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 156: 04exploring.Rnw:3159-3160 (eval = FALSE)
###################################################
## tess(xgrid=xg, ygrid=yg)


###################################################
### code chunk number 157: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 158: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 159: 04exploring.Rnw:3169-3170 (eval = FALSE)
###################################################
## quadrats(W, nx, ny)


###################################################
### code chunk number 160: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 161: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 162: 04exploring.Rnw:3179-3180 (eval = FALSE)
###################################################
## tess(tiles=z)


###################################################
### code chunk number 163: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 164: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 165: 04exploring.Rnw:3201-3202 (eval = FALSE)
###################################################
## tess(image=Z)


###################################################
### code chunk number 166: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 167: 04exploring.Rnw:3233-3234
###################################################
setmargins(0)


###################################################
### code chunk number 168: 04exploring.Rnw:3238-3243
###################################################
X <- runifpoint(42)
plot(solist(X,
	    dirichlet(X),
	    delaunay(X)),
     main="", main.panel="", mar.panel=0, hsep=1, equal.scales=TRUE)


###################################################
### code chunk number 169: 04exploring.Rnw:3287-3291
###################################################
X <- runifpoint(10)
V <- dirichlet(X)
U <- tiles(V)
sapply(U, diameter)


###################################################
### code chunk number 170: 04exploring.Rnw:3315-3316
###################################################
setmargins(0,0,0,2)


###################################################
### code chunk number 171: 04exploring.Rnw:3318-3322
###################################################
set.seed(19171107)
X <- runifpoint(100)
Z <- dirichlet(runifpoint(16))
V <- layered(cut(X,Z), plotargs=list(leg.side="right"))


###################################################
### code chunk number 172: 04exploring.Rnw:3326-3328
###################################################
plot(solist(X, Z, V),
     main="", main.panel="", mar.panel=0, hsep=1, equal.scales=TRUE)


###################################################
### code chunk number 173: 04exploring.Rnw:3339-3340
###################################################
setmargins(0)


###################################################
### code chunk number 174: 04exploring.Rnw:3344-3345
###################################################
plot(split(X, Z), main="", ncols=8, mar.panel=0, hsep=1, vsep=1)


###################################################
### code chunk number 175: 04exploring.Rnw:3361-3365
###################################################
X  <- dirichlet(runifpoint(15))
Y  <- quadrats(owin(), 4, 4)
XY <- layered(X,Y)
Z  <- intersect.tess(X,Y)


###################################################
### code chunk number 176: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 177: 04exploring.Rnw:3371-3375
###################################################
plot(solist(X,Y,Z),main="",
     main.panel=c("tessellation X","tessellation Y",
                  "intersection of X and Y"),
     mar.panel=0, hsep=1, vsep=1)


