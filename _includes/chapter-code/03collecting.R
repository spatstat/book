### R code from vignette source '03collecting.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 03collecting.Rnw:9-10
###################################################
source("R/startup.R")


###################################################
### code chunk number 2: 03collecting.Rnw:403-404 (eval = FALSE)
###################################################
## copyExampleFiles("vesicles")


###################################################
### code chunk number 3: 03collecting.Rnw:407-409 (eval = FALSE)
###################################################
## ves <- read.table("vesicles.txt", header=TRUE)  
## ves <- read.csv("vesicles.csv")  


###################################################
### code chunk number 4: 03collecting.Rnw:524-528 (eval = FALSE)
###################################################
## df <- read.table("vesicles.txt", header=TRUE) 
## x <- df$x
## y <- df$y
## X <- ppp(x, y, c(22,587), c(11,1031))


###################################################
### code chunk number 5: 03collecting.Rnw:531-532 (eval = FALSE)
###################################################
## X <- with(df, ppp(x, y, c(22,587), c(11,1031)))


###################################################
### code chunk number 6: 03collecting.Rnw:603-606 (eval = FALSE)
###################################################
## copyExampleFiles('finpines')
## fp <- read.table('finpines.txt', header=TRUE) 
## X <- with(fp, ppp(x, y, c(-5,5), c(-8,2), marks=diameter))


###################################################
### code chunk number 7: 03collecting.Rnw:611-613 (eval = FALSE)
###################################################
## fp <- read.table("finpines.txt", header=TRUE) 
## X <- as.ppp(fp, owin(c(-5,5), c(-8,2)))


###################################################
### code chunk number 8: 03collecting.Rnw:620-621 (eval = FALSE)
###################################################
## X <- scanpp("finpines.txt", owin(c(-5,5), c(-8,2)))


###################################################
### code chunk number 9: 03collecting.Rnw:656-657
###################################################
demopat


###################################################
### code chunk number 10: 03collecting.Rnw:661-662
###################################################
opa <- options(width=80)


###################################################
### code chunk number 11: 03collecting.Rnw:664-665
###################################################
marks(demopat)


###################################################
### code chunk number 12: 03collecting.Rnw:667-668
###################################################
options(opa)


###################################################
### code chunk number 13: 03collecting.Rnw:674-675
###################################################
marks(demopat) <- factor(marks(demopat), levels=c("B", "A"))


###################################################
### code chunk number 14: 03collecting.Rnw:698-699
###################################################
finpines


###################################################
### code chunk number 15: 03collecting.Rnw:731-732 (eval = FALSE)
###################################################
## unitname(X) <- "m"


###################################################
### code chunk number 16: 03collecting.Rnw:736-737 (eval = FALSE)
###################################################
## unitname(X) <- c("metre", "metres")


###################################################
### code chunk number 17: 03collecting.Rnw:742-743 (eval = FALSE)
###################################################
## unitname(X) <- list("cm", "cm", 42)


###################################################
### code chunk number 18: 03collecting.Rnw:843-844 (eval = FALSE)
###################################################
## x[x == -999] <- NA


###################################################
### code chunk number 19: 03collecting.Rnw:882-884
###################################################
mybad <- ppp(x=c(-0.2, runif(10)), 
             y=c( 0.3, runif(10)), window=square(1))


###################################################
### code chunk number 20: 03collecting.Rnw:892-894
###################################################
mybad
attr(mybad, "rejects")


###################################################
### code chunk number 21: 03collecting.Rnw:900-901
###################################################
as.ppp(mybad)


###################################################
### code chunk number 22: Rletter3.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 23: 03collecting.Rnw:1075-1079
###################################################
plot(solist(as.rectangle(letterR),
            letterR,
            as.mask(letterR, eps=0.1)),
     lwd=2, main="", main.panel="", mar.panel=0, hsep=2, box=FALSE)


###################################################
### code chunk number 24: 03collecting.Rnw:1118-1119
###################################################
owin(c(0,3), c(1,2))


###################################################
### code chunk number 25: 03collecting.Rnw:1122-1125
###################################################
as.owin(c(0,3,1,2))
square(5)
square(c(1,3))


###################################################
### code chunk number 26: 03collecting.Rnw:1168-1169
###################################################
Z <-  owin(poly=list(x=c(0,10,0), y=c(0,0,10)))


###################################################
### code chunk number 27: 03collecting.Rnw:1179-1182
###################################################
ZH <- owin(poly=list(list(x=c(0,10,0), y=c(0,0,10)),
                     list(x=c(2,2,4,4), y=c(2,4,4,2))))
plot(solist(Z, ZH), main="", main.panel="", mar.panel=0, hsep=1, hatch=TRUE)


###################################################
### code chunk number 28: 03collecting.Rnw:1200-1202 (eval = FALSE)
###################################################
## ZH <- owin(poly=list(list(x=c(0,10,0), y=c(0,0,10)),
##                      list(x=c(2,2,4,4), y=c(2,4,4,2))))


###################################################
### code chunk number 29: 03collecting.Rnw:1225-1227 (eval = FALSE)
###################################################
## bd <- read.table("mitochondria.txt", header=TRUE)
## W  <- owin(poly=bd)


###################################################
### code chunk number 30: 03collecting.Rnw:1235-1238 (eval = FALSE)
###################################################
## bd <- read.table("vesicleswindow.txt", header=TRUE)
## bds <- split(bd[,c("x","y")], bd$id)
## W  <- owin(poly=bds)


###################################################
### code chunk number 31: 03collecting.Rnw:1245-1246
###################################################
as.data.frame(ZH)


###################################################
### code chunk number 32: 03collecting.Rnw:1275-1276
###################################################
W <- disc(radius=3, centre=c(0,0))


###################################################
### code chunk number 33: 03collecting.Rnw:1303-1316
###################################################
Eps <- 0.2
P <- pixelcentres(Frame(letterR), eps=Eps)
inR <- inside.owin(P, w=letterR)
M <- as.mask(letterR, eps=Eps)
Masking <- layered(M,
                   P[inR],
                   P[!inR],
                   letterR,
                   plotargs=list(list(col="grey", border=NA),
                                 list(pch=16),
                                 list(pch=1),
                                 list(lwd=3)))
MaskDemo <- solist(Masking[4], Masking[-1], Masking[c(1,4)])


###################################################
### code chunk number 34: Rletter3.Rnw:3-5
###################################################
newplot(12.5, 0.9)
setmargins(0)


###################################################
### code chunk number 35: 03collecting.Rnw:1323-1325
###################################################
plot(MaskDemo, main="", main.panel="", nrows=1, 
     equal.scales=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 36: 03collecting.Rnw:1490-1493
###################################################
blankpage <- function(a, b, main="") {
  plot(owin(1/2 + c(0,a), 1/2 + c(0,b)), main=main, claim.title.space=TRUE)
}


###################################################
### code chunk number 37: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 38: 03collecting.Rnw:1497-1498
###################################################
setmargins(0)


###################################################
### code chunk number 39: 03collecting.Rnw:1502-1514
###################################################
nr <- 3
nc <- 4
par(mfrow=c(1,3))
blankpage(nr, nc, "Cartesian")
for(i in 1:nr) for(j in 1:nc) text(i, j, paste0("(",i,",",j,")"), font=2)
par(mar=c(0,0,0,2))
blankpage(nc,nr, "European")
for(i in 1:nr) for(j in 1:nc) text(j, nr+1-i, paste0("(",i,",",j,")"), font=2)
par(mar=c(0,0,0,2))
blankpage(nc,nr, "spatstat")
for(i in 1:nr) for(j in 1:nc) text(j, i, paste0("(",i,",",j,")"), font=2)
par(mfrow=c(1,1))


###################################################
### code chunk number 40: 03collecting.Rnw:1525-1526 (eval = FALSE)
###################################################
## mm <- transmat(m, from="Cartesian", to="European")


###################################################
### code chunk number 41: 03collecting.Rnw:1591-1593
###################################################
fn  <- system.file("rawdata", "vesicles", "vesiclesimage.tif", 
                   package="spatstat.data")


###################################################
### code chunk number 42: 03collecting.Rnw:1601-1603 (eval = FALSE)
###################################################
## fn  <- system.file("rawdata/vesicles/vesiclesimage.tif", 
##                    package="spatstat.data")


###################################################
### code chunk number 43: readtiff
###################################################
library(tiff)
mat <- readTIFF(fn, as.is=TRUE, info=TRUE)


###################################################
### code chunk number 44: smat
###################################################
smat <- transmat(mat, from="European", to="spatstat")


###################################################
### code chunk number 45: 03collecting.Rnw:1627-1632
###################################################
pixscale <- 2.5
vim <- im(smat,
          xrange=c(0, ncol(smat) * pixscale), 
          yrange=c(0, nrow(smat) * pixscale),
          unitname="nm")


###################################################
### code chunk number 46: 03collecting.Rnw:1637-1639
###################################################
vcolmap <- colourmap(gray((0:100)/100), range=c(0,255))
vimh <- rotate(vim, pi/2)


###################################################
### code chunk number 47: Vesicles90.Rnw:3-5
###################################################
newplot(10, 0.5)
setmargins(0)


###################################################
### code chunk number 48: 03collecting.Rnw:1644-1645
###################################################
plot(vimh, col = vcolmap, main = "", ribbon=FALSE)


###################################################
### code chunk number 49: readjpg
###################################################
require(jpeg)
fn  <- system.file("rawdata", "sandholes", "sandholes.jpg",
                   package="spatstat.data")
arr <- readJPEG(fn)


###################################################
### code chunk number 50: 03collecting.Rnw:1665-1666
###################################################
str(arr)


###################################################
### code chunk number 51: 03collecting.Rnw:1668-1670
###################################################
excol <- rgb(arr[1,1,1], arr[1,1,2], arr[1,1,3])
excol <- paste0("\\\\",excol)


###################################################
### code chunk number 52: mapcolour
###################################################
mats <- rgb(arr[,,1], arr[,,2], arr[,,3])
dim(mats) <- dim(arr)[1:2]


###################################################
### code chunk number 53: sandholes
###################################################
sand <- im(transmat(mats, "European", "spatstat"))


###################################################
### code chunk number 54: Sandholes.Rnw:3-5
###################################################
newplot(9, 0.7)
setmargins(0)


###################################################
### code chunk number 55: 03collecting.Rnw:1702-1703
###################################################
plot(sand, main = "", ribbon=FALSE)


###################################################
### code chunk number 56: 03collecting.Rnw:1716-1718 (eval = FALSE)
###################################################
## unitname(sand) <- list("cm", "cm", 30/609)
## sand <- rescale(sand)


###################################################
### code chunk number 57: 03collecting.Rnw:1754-1759
###################################################
fn  <- system.file("rawdata", "gorillas", "vegetation.asc",
                   package="spatstat.data")
pixvals <- scan(fn, skip=6)
pixvals[pixvals == -9999] <- NA
mat <- matrix(pixvals, nrow=149, ncol=181, byrow=TRUE)


###################################################
### code chunk number 58: 03collecting.Rnw:1766-1768
###################################################
vtype <- c("Disturbed", "Colonising", "Grassland",
           "Primary", "Secondary", "Transition")


###################################################
### code chunk number 59: 03collecting.Rnw:1771-1774
###################################################
f <- factor(mat, labels=vtype)
is.factor(f)
is.matrix(f)


###################################################
### code chunk number 60: 03collecting.Rnw:1779-1780
###################################################
dim(f) <- c(149,181)


###################################################
### code chunk number 61: 03collecting.Rnw:1783-1784
###################################################
factorim <- im(f)


###################################################
### code chunk number 62: 03collecting.Rnw:1789-1793
###################################################
x0 <- 580440.38505253 ; y0 <- 674156.51146465 
dx <- dy <- 30.70932052048 
factorim <- im(f, xrange=x0 + dx * c(0, 181), 
                  yrange=y0 + dy * c(0, 149))


###################################################
### code chunk number 63: UnitR.Rnw:3-6
###################################################
newplot(9, 0.7)
zeromargins() # strip all margins
setmargins(0.1 + c(0,0,0,2)) # back off


###################################################
### code chunk number 64: 03collecting.Rnw:1797-1798
###################################################
setmargins(0,0,0,4.5)


###################################################
### code chunk number 65: 03collecting.Rnw:1803-1804
###################################################
plot(factorim, main="", ribargs=list(las=1))


###################################################
### code chunk number 66: 03collecting.Rnw:1840-1842
###################################################
f <- function(x,y){15*(cos(sqrt((x-3)^2+3*(y-3)^2)))^2}
A <- as.im(f, W=square(6))


###################################################
### code chunk number 67: UnitR.Rnw:3-6
###################################################
newplot(9, 0.7)
zeromargins() # strip all margins
setmargins(0.1 + c(0,0,0,2)) # back off


###################################################
### code chunk number 68: 03collecting.Rnw:1854-1855
###################################################
plot(A, main="", ribargs=list(las=1))


###################################################
### code chunk number 69: 03collecting.Rnw:1868-1869
###################################################
g <- funxy(f, W=square(6))


###################################################
### code chunk number 70: 03collecting.Rnw:1970-1971
###################################################
P <- solist(A=cells, B=japanesepines, C=redwood)


###################################################
### code chunk number 71: 03collecting.Rnw:1991-1992
###################################################
KP <- anylist(A=Kest(cells), B=Kest(japanesepines), C=Kest(redwood))


###################################################
### code chunk number 72: 03collecting.Rnw:1995-1996
###################################################
KP <- as.anylist(lapply(P, Kest))


###################################################
### code chunk number 73: 03collecting.Rnw:2028-2029
###################################################
ws <- hyperframe(Larvae=waterstriders)


###################################################
### code chunk number 74: 03collecting.Rnw:2076-2077
###################################################
mito <- simplify.owin(vesicles.extra$mitochondria, 40)


###################################################
### code chunk number 75: Vesicles.Rnw:3-5
###################################################
newplot(6, 0.3)
setmargins(0)


###################################################
### code chunk number 76: 03collecting.Rnw:2082-2086
###################################################
plot(vim, col = vcolmap, main = "", ribbon=FALSE)
plot(mito, add=TRUE, border="white", lwd=2)
points(vertices(mito), col="white")
plot(vesicles[c(FALSE,TRUE,FALSE)], add=TRUE, col="white")


###################################################
### code chunk number 77: 03collecting.Rnw:2088-2092
###################################################
plot(vesicles, main="")
with(vesicles.extra, {
  plot(activezone, add=TRUE, lwd=8)
})


###################################################
### code chunk number 78: 03collecting.Rnw:2108-2111 (eval = FALSE)
###################################################
## plot(vim)
## mito <- clickpoly(add=TRUE, col="white", win=Window(vim))
## vesi <- clickppp(add=TRUE, col="white", win=Window(vim))


###################################################
### code chunk number 79: 03collecting.Rnw:2123-2124 (eval = FALSE)
###################################################
## plot(vesicles, add=TRUE, chars=3, col="green")


###################################################
### code chunk number 80: 03collecting.Rnw:2232-2239
###################################################
library(maptools)
oldfolder <- getwd()
setwd(system.file("shapes", package="maptools"))
baltim   <- readShapeSpatial("baltim.shp")
columbus <- readShapeSpatial("columbus.shp")
fylk     <- readShapeSpatial("fylk-val.shp")
setwd(oldfolder)


###################################################
### code chunk number 81: 03collecting.Rnw:2287-2289 (eval = FALSE)
###################################################
## balt <- as(baltim, "ppp")
## bdata <- slot(baltim, "data")


###################################################
### code chunk number 82: 03collecting.Rnw:2409-2410
###################################################
zeromargins()


###################################################
### code chunk number 83: 03collecting.Rnw:2414-2423
###################################################
A <- as.owin(chorley)
A <- simplify.owin(A, 1)
B <- scalardilate(A, 1/3, origin="centroid")
B <- rotate(B, 4*pi/3, centre="centroid")
D <- setminus.owin(A, B)
layerplotargs(D) <- list(col="grey")
plot(solist(A, D), lwd=2, main="", main.panel="",
     mar.panel=0, hsep=2,
     equal.scales=TRUE, nrows=1)


###################################################
### code chunk number 84: 03collecting.Rnw:2508-2512 (eval = FALSE)
###################################################
## cp <- as(columbus, "SpatialPolygons")
## cregions <- slot(cp, "polygons")
## cregions <- lapply(cregions, function(x){ SpatialPolygons(list(x)) })
## cwindows <- solapply(cregions, as.owin)


###################################################
### code chunk number 85: 03collecting.Rnw:2520-2522 (eval = FALSE)
###################################################
## ch <- hyperframe(window=cwindows)
## ch <- cbind.hyperframe(ch, columbus@data)


###################################################
### code chunk number 86: 03collecting.Rnw:2594-2597
###################################################
df <- tools::Adobe_glyphs
ii <- match("mu", df$adobe)
df[ii,]


###################################################
### code chunk number 87: 03collecting.Rnw:2600-2601 (eval = FALSE)
###################################################
## unitname(X) <- "\u00B5m"


