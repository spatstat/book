### R code from vignette source '09inferpois'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 09inferpois.Rnw:9-12
###################################################
source("R/startup.R")
requireversion(spatstat, "1.41-1.023")


###################################################
### code chunk number 2: Unit2x3.Rnw:3-5
###################################################
newplot(9.5, 1)
setmargins(0)


###################################################
### code chunk number 3: 09inferpois.Rnw:222-233
###################################################
Y <- psp(c(0,    0.5, 0.5),
         c(0.99, 0.5, 0.5),
         c(0.5,  0.5, 0.99),
         c(0.5,  0.01, 0.99),
         owin())
dY <- distfun(Y, owin())
lam <- list(as.im(100, owin()), 
            as.im(function(x,y) 400 * x^2 * (1-y), owin()),
            as.im(function(x,y) 300 * exp(-10*dY(x,y)), owin()))
X <- lapply(lam, rpoispp)
Z <- as.solist(append(lam, X))


###################################################
### code chunk number 4: 09inferpois.Rnw:237-241
###################################################
pan <- function(i) { if(i <= 3) list(col=whitetoblack, box=TRUE) else list() }
plot(Z, equal.scales=TRUE, main.panel="", main="", pch=16, 
     ribbon=FALSE, panel.args=pan,
     mar.panel=0.1+ 0.3*c(0,1,0,1))


###################################################
### code chunk number 5: 09inferpois.Rnw:405-412
###################################################
W <- as.owin(austates)
aa <- tile.areas(austates)
## state populations in millions (ABS, December 2012)
pp <- c(2.47, 0.24, 1.66, 4.61, 7.35, 5.68, 0.51)
lam.state <- 10 * pp/aa
f <- function(x,y) { lam.state[tileindex(x,y,austates)] }
X <- rpoispp(f, win=W)


###################################################
### code chunk number 6: AUr.Rnw:3-5
###################################################
newplot(10,0.5)
setmargins(0,0,0,2)


###################################################
### code chunk number 7: 09inferpois.Rnw:419-421
###################################################
plot(as.im(f, W=W, dimyx=256), main="", col=lighttodark, box=FALSE)
plot(W, add=TRUE)


###################################################
### code chunk number 8: AU.Rnw:3-5
###################################################
newplot(8,0.45)
zeromargins()


###################################################
### code chunk number 9: 09inferpois.Rnw:426-428
###################################################
plot(austates, main="")
points(X, pch=16, cex=0.8)


###################################################
### code chunk number 10: 09inferpois.Rnw:577-579
###################################################
Digits <- 4
dopt <- options(digits=Digits)


###################################################
### code chunk number 11: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 12: 09inferpois.Rnw:614-615 (eval = FALSE)
###################################################
## ppm(X ~ trend, ...)


###################################################
### code chunk number 13: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 14: 09inferpois.Rnw:649-651
###################################################
fit <- ppm(bei ~ 1)
fit


###################################################
### code chunk number 15: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 16: 09inferpois.Rnw:677-678 (eval = FALSE)
###################################################
## ppm(X ~ trend)


###################################################
### code chunk number 17: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 18: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 19: 09inferpois.Rnw:685-686 (eval = FALSE)
###################################################
## ppm(X ~ trend, ..., data)


###################################################
### code chunk number 20: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 21: 09inferpois.Rnw:746-750
###################################################
## Temporarily suppress printing of standard errors and CI's in ppm
spatstat.options(print.ppm.SE="never")
## Remove wasted space in output
spatstat.options(terse=2)


###################################################
### code chunk number 22: 09inferpois.Rnw:775-776
###################################################
bei.extra


###################################################
### code chunk number 23: 09inferpois.Rnw:781-783
###################################################
fit <- ppm(bei ~ grad, data=bei.extra)
fit


###################################################
### code chunk number 24: 09inferpois.Rnw:785-787
###################################################
co <- signif(coef(fit), Digits)
expco <- signif(exp(coef(fit)), Digits)


###################################################
### code chunk number 25: 09inferpois.Rnw:847-853
###################################################
fit2 <- ppm(bei ~ grad + I(grad^2),data=bei.extra) 
ef1 <- effectfun(fit, "grad", se.fit=TRUE)
ef2 <- effectfun(fit2, "grad", se.fit=TRUE)
xr <- c(0, 0.25)
yr <- range(with(ef1, range(.[.x <= xr[2] ])), 
            with(ef2, range(.[.x <= xr[2] ])))


###################################################
### code chunk number 26: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 27: 09inferpois.Rnw:858-862
###################################################
par(mfrow = c(1,2))
plot(ef1, main="", xlim=xr, ylim = yr, legend=FALSE)
plot(ef2, main="", xlim=xr, ylim = yr, legend=FALSE)
par(mfrow=c(1,1))


###################################################
### code chunk number 28: 09inferpois.Rnw:907-908 (eval = FALSE)
###################################################
## ppm(bei ~ atan(grad), data=bei.extra)


###################################################
### code chunk number 29: 09inferpois.Rnw:922-923 (eval = FALSE)
###################################################
## ppm(bei ~ I(atan(grad) * 180/pi), data=bei.extra)


###################################################
### code chunk number 30: 09inferpois.Rnw:931-933 (eval = FALSE)
###################################################
## degrees <- function(x) { x * 180/pi }
## ppm(bei ~ degrees(atan(grad)), data=bei.extra)


###################################################
### code chunk number 31: 09inferpois.Rnw:946-947
###################################################
ppm(bei ~ grad + I(grad^2), data=bei.extra)


###################################################
### code chunk number 32: 09inferpois.Rnw:949-950
###################################################
fit <- ppm(bei ~ grad + I(grad^2), data=bei.extra)


###################################################
### code chunk number 33: MurRescale
###################################################
mur <- solapply(murchison, rescale, s=1000, unitname="km")
mf <- do.call(boundingbox, lapply(mur, Frame))


###################################################
### code chunk number 34: Murchison.Rnw:3-5
###################################################
newplot(6, 0.5)
zeromargins()


###################################################
### code chunk number 35: murchisonBig
###################################################
with(mur, {
  plot(mf, main="")
  plot(greenstone, add=TRUE, 
       col=if(monochrome) "lightgrey" else "green", 
       border=NA)
  plot(gold, pch=3, add=TRUE)
  plot(faults, add=TRUE)
})


###################################################
### code chunk number 36: 09inferpois.Rnw:1069-1070 (eval = FALSE)
###################################################
## mur <- solapply(murchison, rescale, s=1000, unitname="km")


###################################################
### code chunk number 37: MurchReedy.Rnw:3-5
###################################################
newplot(6.8, 0.6)
setmargins(0)


###################################################
### code chunk number 38: murchreedy
###################################################
reedy <- owin(c(580, 650), c(6986, 7026))
with(mur, {
  plot(greenstone[reedy], 
       border=NA, col=if(monochrome) "grey" else "lightgreen", 
       main="")
  plot(faults[reedy], add=TRUE, lwd=2)
  plot(gold[reedy], add=TRUE, pch=3, cex=1.5)
  plot(reedy, add=TRUE, lty=2)
})


###################################################
### code chunk number 39: 09inferpois.Rnw:1119-1120
###################################################
dfault <- with(mur,distfun(faults))


###################################################
### code chunk number 40: 09inferpois.Rnw:1123-1125
###################################################
fit <- ppm(gold ~ dfault, data=mur)
fit


###################################################
### code chunk number 41: 09inferpois.Rnw:1127-1129
###################################################
co <- signif(coef(fit), Digits)
expco <- signif(exp(co), Digits)


###################################################
### code chunk number 42: 09inferpois.Rnw:1156-1157
###################################################
efd <- effectfun(fit, "dfault", se.fit=TRUE)


###################################################
### code chunk number 43: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 44: 09inferpois.Rnw:1163-1164
###################################################
plot(efd, main="", xlim=c(0, 20), legend=FALSE)


###################################################
### code chunk number 45: 09inferpois.Rnw:1197-1199
###################################################
spatstat.options(terse=1)
spatstat.options(print.ppm.SE="poisson")


###################################################
### code chunk number 46: 09inferpois.Rnw:1212-1216
###################################################
fit <- ppm(gold ~ greenstone, data=mur)
co <- coef(fit)
co <- signif(co, Digits)
ecco <- signif(exp(cumsum(co)), Digits)


###################################################
### code chunk number 47: 09inferpois.Rnw:1218-1219
###################################################
ppm(gold ~ greenstone, data=mur)


###################################################
### code chunk number 48: 09inferpois.Rnw:1249-1250
###################################################
ppm(gold ~ greenstone - 1, data=mur)


###################################################
### code chunk number 49: 09inferpois.Rnw:1252-1256
###################################################
fit <- ppm(gold ~ greenstone-1,data=mur)
co <- coef(fit)
co <- signif(co, Digits)
ecco <- signif(exp(co), Digits)


###################################################
### code chunk number 50: gex
###################################################
gor <- rescale(gorillas, 1000, unitname="km")
gor <- unmark(gor)
gex <- lapply(gorillas.extra, rescale, 
              s=1000, unitname="km")


###################################################
### code chunk number 51: 09inferpois.Rnw:1339-1340
###################################################
oldveglev <- levels(gex$vegetation)


###################################################
### code chunk number 52: Gorillas2R.Rnw:4-6
###################################################
newplot(8.5, 1)
setmargins(0,0,0,5)


###################################################
### code chunk number 53: 09inferpois.Rnw:1349-1357
###################################################
vegcol <- if(monochrome) grey((5:0)/6) else NULL
argh <- function(i) switch(i,
                           list(pch=3, cex=0.7),
                           list(box=FALSE,
                                ribargs=list(las=2, box=TRUE),
                                col=vegcol))
plot(solist(gor, gex$vegetation), main="", main.panel="", 
     equal.scales=TRUE, panel.args=argh, mar.panel=0.1+c(0,0,0,1))


###################################################
### code chunk number 54: 09inferpois.Rnw:1370-1371
###################################################
opa <- options(width=78)


###################################################
### code chunk number 55: 09inferpois.Rnw:1373-1383
###################################################
names(gex)
shorten <- function(x) substr(x, 1, 4) 
names(gex) <- shorten(names(gex))
names(gex)
names(gex)[4:5] <- c("sang", "styp")
names(gex)
isfactor <- !sapply(lapply(gex, levels), is.null)
for(i in which(isfactor))
  levels(gex[[i]]) <- shorten(levels(gex[[i]]))
levels(gex$vege)


###################################################
### code chunk number 56: 09inferpois.Rnw:1385-1386
###################################################
options(opa)


###################################################
### code chunk number 57: 09inferpois.Rnw:1391-1392
###################################################
ppm(gor ~ vege, data=gex)


###################################################
### code chunk number 58: 09inferpois.Rnw:1394-1399
###################################################
co <- coef(ppm(gor ~ vege, data=gex))
na <- names(co)
co <- signif(co, Digits)
eco <- signif(exp(co), Digits)
lev <- levels(gex[["vege"]])


###################################################
### code chunk number 59: 09inferpois.Rnw:1445-1448
###################################################
fitveg <- ppm(gor ~ vege - 1, data=gex)
fitveg
exp(coef(fitveg))


###################################################
### code chunk number 60: 09inferpois.Rnw:1455-1457
###################################################
vt <- tess(image=gex$vege)
intensity(quadratcount(gor, tess=vt))


###################################################
### code chunk number 61: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 62: 09inferpois.Rnw:1529-1530 (eval = FALSE)
###################################################
## options(contrasts=c(arg1,arg2))


###################################################
### code chunk number 63: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 64: 09inferpois.Rnw:1569-1571
###################################################
spatstat.options(terse=2)
spatstat.options(print.ppm.SE="never")


###################################################
### code chunk number 65: 09inferpois.Rnw:1596-1598
###################################################
fitadd <- ppm(bei ~ elev + grad, data=bei.extra)
fitadd


###################################################
### code chunk number 66: 09inferpois.Rnw:1600-1603
###################################################
co <- signif(coef(fitadd), Digits)
re <- round(with(bei.extra,range(elev)), 1)
rg <- round(with(bei.extra,range(grad)), 4)


###################################################
### code chunk number 67: 09inferpois.Rnw:1651-1652
###################################################
ppm(gold ~ dfault + greenstone,data=mur)


###################################################
### code chunk number 68: 09inferpois.Rnw:1654-1656
###################################################
co <- coef(ppm(gold ~ dfault + greenstone,data=mur))
co <- signif(co, Digits)


###################################################
### code chunk number 69: 09inferpois.Rnw:1667-1668 (eval = FALSE)
###################################################
## ppm(gor ~ vege + styp, data=gex)


###################################################
### code chunk number 70: 09inferpois.Rnw:1700-1701
###################################################
jpines <- residualspaper[["Fig1"]]


###################################################
### code chunk number 71: 09inferpois.Rnw:1716-1717
###################################################
ppm(jpines ~ x + y)


###################################################
### code chunk number 72: 09inferpois.Rnw:1719-1722
###################################################
fut <- ppm(jpines ~ x + y)
co <- as.numeric(coef(fut))
fco <- signif(co, digits=Digits)


###################################################
### code chunk number 73: 09inferpois.Rnw:1738-1739
###################################################
ppm(jpines ~ polynom(x,y,2))


###################################################
### code chunk number 74: 09inferpois.Rnw:1757-1758
###################################################
ppm(jpines ~ (x < 0.5))


###################################################
### code chunk number 75: chorley
###################################################
if(draftversion) {
  reload.or.compute(datafilepath("chorleyCoarse.rda"), {
    lung <- split(chorley)$lung
    larynx <- split(chorley)$larynx
    smo <- density(lung, sigma=0.15, eps=0.1)
    smo <- eval.im(pmax(smo, 1e-10))
    Q <- quadscheme(larynx, eps=0.5)
  })
  draftmessage <- 
      "using coarse quadrature scheme, eps=0.5, in draft version"
} else {
  reload.or.compute(datafilepath("chorley.rda"), {
    lung <- split(chorley)$lung
    larynx <- split(chorley)$larynx
    smo <- density(lung, sigma=0.15, eps=0.1)
    smo <- eval.im(pmax(smo, 1e-10))
    Q <- quadscheme(larynx, eps=0.1)
  })
  draftmessage <- NULL
}


###################################################
### code chunk number 76: 09inferpois.Rnw:1827-1830 (eval = FALSE)
###################################################
##   lung <- split(chorley)$lung
##   larynx <- split(chorley)$larynx
##   smo <- density(lung, sigma=0.15, eps=0.1, positive=TRUE)


###################################################
### code chunk number 77: 09inferpois.Rnw:1840-1841
###################################################
smo <- eval.im(pmax(smo, 1e-10))


###################################################
### code chunk number 78: ChorleyR.Rnw:3-5
###################################################
newplot(9, 0.65)
setmargins(0, 0, 0, 2)


###################################################
### code chunk number 79: 09inferpois.Rnw:1851-1853
###################################################
plot(smo, main="", col=greytoblack, box=FALSE)
plot(Window(chorley), add=TRUE)


###################################################
### code chunk number 80: 09inferpois.Rnw:1868-1869
###################################################
ppm(larynx ~ offset(log(smo)))


###################################################
### code chunk number 81: 09inferpois.Rnw:1871-1873
###################################################
fit <- ppm(larynx ~ offset(log(smo)))
co <- signif(coef(fit), Digits)


###################################################
### code chunk number 82: 09inferpois.Rnw:1957-1958
###################################################
ppm(bei ~ log(grad), data=bei.extra)


###################################################
### code chunk number 83: 09inferpois.Rnw:1960-1961
###################################################
co <- coef(ppm(bei ~ log(grad),data=bei.extra))


###################################################
### code chunk number 84: 09inferpois.Rnw:1987-1990 (eval = FALSE)
###################################################
## G <- bei.extra[["grad"]]
## G[bei[42]] <- 0
## ppm(bei ~ log(G))


###################################################
### code chunk number 85: 09inferpois.Rnw:1992-2000
###################################################
G <- bei.extra[["grad"]]
G[bei[42]] <- 0
oo <- try(ppm(bei ~ log(G)), silent=TRUE)
## reformat error message to respect text width
oo <- gsub("\n", "", oo)
spatstat:::splat(oo)
##oo <- strsplit(oo, " ")
##do.call(cat, append(oo, list(fill=TRUE)))


###################################################
### code chunk number 86: 09inferpois.Rnw:2018-2019
###################################################
ppm(gold ~ log(dfault), data=mur)


###################################################
### code chunk number 87: 09inferpois.Rnw:2021-2022
###################################################
co <- coef(ppm(gold ~ log(dfault),data=mur))


###################################################
### code chunk number 88: 09inferpois.Rnw:2127-2129
###################################################
fit <- ppm(bei ~ elev * grad, data=bei.extra)
fit


###################################################
### code chunk number 89: 09inferpois.Rnw:2164-2165
###################################################
ppm(gor ~ vege * heat, data=gex)


###################################################
### code chunk number 90: 09inferpois.Rnw:2202-2204
###################################################
mco <- coef(ppm(gold ~ dfault * greenstone, data=mur))
mco <- signif(co2, Digits)


###################################################
### code chunk number 91: 09inferpois.Rnw:2215-2216
###################################################
ppm(gold ~ dfault * greenstone, data=mur)


###################################################
### code chunk number 92: 09inferpois.Rnw:2244-2246
###################################################
ppm(gold ~ greenstone/dfault, data=mur)
ppm(gold ~ greenstone/dfault - 1, data=mur)


###################################################
### code chunk number 93: 09inferpois.Rnw:2266-2267
###################################################
ppm(gold ~ dfault/greenstone, data=mur)


###################################################
### code chunk number 94: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 95: 09inferpois.Rnw:2282-2283 (eval = FALSE)
###################################################
## ppm(Y ~ X1 + X2 + X3 + ... + Xn)


###################################################
### code chunk number 96: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 97: 09inferpois.Rnw:2293-2294 (eval = FALSE)
###################################################
## ppm(gor ~ . , data=gex)


###################################################
### code chunk number 98: 09inferpois.Rnw:2298-2299 (eval = FALSE)
###################################################
## ppm(gor ~ . - heat, data=gex)


###################################################
### code chunk number 99: 09inferpois.Rnw:2333-2334 (eval = FALSE)
###################################################
## ppm(gor ~ .^2, data=gex)


###################################################
### code chunk number 100: 09inferpois.Rnw:2436-2437
###################################################
spatstat.options(terse=1, print.ppm.SE="poisson")


###################################################
### code chunk number 101: 09inferpois.Rnw:2439-2443
###################################################
beikm <- rescale(bei, 1000, unitname="km")
bei.extrakm <- lapply(bei.extra, rescale, s=1000, unitname="km")
fitkm <- ppm(beikm ~ x + y)
fitkm


###################################################
### code chunk number 102: 09inferpois.Rnw:2445-2446
###################################################
spatstat.options(terse=2, print.ppm.SE="poisson")


###################################################
### code chunk number 103: 09inferpois.Rnw:2455-2458
###################################################
cf <- coef(summary(fitkm))
cf <- subset(cf, select=-Ztest)["x", ]
cf <- signif(as.numeric(cf), Digits)


###################################################
### code chunk number 104: 09inferpois.Rnw:2472-2473
###################################################
coef(fitkm)


###################################################
### code chunk number 105: 09inferpois.Rnw:2478-2479 (eval = FALSE)
###################################################
## plot(fitkm, how="image", se=FALSE)


###################################################
### code chunk number 106: BeiR.Rnw:3-5
###################################################
newplot(13,0.95)
setmargins(0,0,0,1)


###################################################
### code chunk number 107: 09inferpois.Rnw:2487-2491
###################################################
plot(fitkm, how="image", se=FALSE, main="", 
     col=if(monochrome) darktolight else NULL,
     ribscale=1/1000,
     pppargs=if(monochrome) list(cols="white", pch=3, cex=0.5) else list())


###################################################
### code chunk number 108: 09inferpois.Rnw:2504-2505
###################################################
coef(summary(fitkm))


###################################################
### code chunk number 109: 09inferpois.Rnw:2531-2532
###################################################
vcov(fitkm)


###################################################
### code chunk number 110: 09inferpois.Rnw:2537-2538
###################################################
sqrt(diag(vcov(fitkm)))


###################################################
### code chunk number 111: 09inferpois.Rnw:2542-2543
###################################################
confint(fitkm, level=0.95)


###################################################
### code chunk number 112: 09inferpois.Rnw:2552-2554
###################################################
co <- vcov(fitkm, what="corr") 
round(co, 2)


###################################################
### code chunk number 113: 09inferpois.Rnw:2562-2565
###################################################
fitch <- update(fitkm, . ~ I(x-0.5) + I(y-0.25))
co <- vcov(fitch, what="corr")
round(co, 2)


###################################################
### code chunk number 114: 09inferpois.Rnw:2589-2591
###################################################
fit <- ppm(bei ~ polynom(grad, elev, 2), data=bei.extra)
lamhat <- predict(fit)


###################################################
### code chunk number 115: 09inferpois.Rnw:2596-2597
###################################################
lamB <- predict(fit, locations=bei)


###################################################
### code chunk number 116: 09inferpois.Rnw:2599-2600
###################################################
lamhatSE <- predict(fit, se=TRUE)$se


###################################################
### code chunk number 117: Bei2.Rnw:3-5
###################################################
newplot(8,1)
setmargins(0.1)


###################################################
### code chunk number 118: 09inferpois.Rnw:2605-2609
###################################################
plot(solist(lamhat, lamhatSE), 
     axes=FALSE, plotcommand="contour", drawlabels=FALSE,
     main="", main.panel="", 
     mar.panel=0, hsep=0.2, equal.scales=TRUE)


###################################################
### code chunk number 119: 09inferpois.Rnw:2629-2633 (eval = FALSE)
###################################################
## M <- persp(bei.extra$elev, colin=lamhat, colmap=topo.colors, 
##            shade=0.4, theta=-55, phi=25, expand=6, 
##            box=FALSE, apron=TRUE, visible=TRUE)
## perspPoints(bei, Z=bei.extra$elev, M=M, pch=20, cex=0.1)


###################################################
### code chunk number 120: 09inferpois.Rnw:2639-2640
###################################################
zeromargins()


###################################################
### code chunk number 121: 09inferpois.Rnw:2644-2651
###################################################
col.lam <- if(monochrome) grey(seq(0,1,length=128)) else topo.colors
col.pts <-  if(monochrome) 1 else 2
M <- persp(bei.extra$elev, colin=lamhat,
           colmap=col.lam, shade=0.4, theta=-55, phi=25, expand=6, 
           box=FALSE, apron=TRUE, visible=TRUE, main="")
perspPoints(bei, Z=bei.extra$elev, M=M, 
            pch=".", col=col.pts, cex=1.25)


###################################################
### code chunk number 122: 09inferpois.Rnw:2699-2701
###################################################
B <- levelset(bei.extra$elev, 130)
predict(fit, type="count", window=B)


###################################################
### code chunk number 123: 09inferpois.Rnw:2716-2717
###################################################
predict(fit, B, type="count", se=TRUE)


###################################################
### code chunk number 124: 09inferpois.Rnw:2720-2721
###################################################
predict(fit, B, type="count", interval="confidence")


###################################################
### code chunk number 125: 09inferpois.Rnw:2728-2729
###################################################
predict(fit, B, type="count", interval="prediction")


###################################################
### code chunk number 126: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 127: 09inferpois.Rnw:2759-2760 (eval = FALSE)
###################################################
## update(object, ...)


###################################################
### code chunk number 128: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 129: 09inferpois.Rnw:2775-2777
###################################################
fitcsr <- ppm(bei ~ 1, data=bei.extra)
update(fitcsr, bei ~ grad)


###################################################
### code chunk number 130: 09inferpois.Rnw:2789-2790
###################################################
fitgrad <- update(fitcsr, . ~ grad)


###################################################
### code chunk number 131: 09inferpois.Rnw:2794-2796
###################################################
fitall <- update(fitgrad, . ~ . + elev)
fitall


###################################################
### code chunk number 132: 09inferpois.Rnw:2799-2800
###################################################
fitp <- update(fitall, . ~ . - 1)


###################################################
### code chunk number 133: 09inferpois.Rnw:2806-2807
###################################################
update(gor ~ (heat + vege)^2,   . ~ . - heat:vege)


###################################################
### code chunk number 134: 09inferpois.Rnw:2810-2811
###################################################
update(gor ~ (heat + vege + sang)^3, . ~ .)


###################################################
### code chunk number 135: 09inferpois.Rnw:2838-2840
###################################################
fit0 <- ppm(bei ~ 1)
fit1 <- ppm(bei ~ grad, data=bei.extra)


###################################################
### code chunk number 136: 09inferpois.Rnw:2857-2858
###################################################
anova(fit0, fit1, test="LR")


###################################################
### code chunk number 137: 09inferpois.Rnw:2882-2884
###################################################
fit2e <- ppm(bei ~ polynom(elev, 2), data=bei.extra)
fit2e1g <- update(fit2e, . ~ . + grad)


###################################################
### code chunk number 138: 09inferpois.Rnw:2886-2887
###################################################
anova(fit2e, fit2e1g, test="LR")


###################################################
### code chunk number 139: 09inferpois.Rnw:2934-2938
###################################################
fitprop <- ppm(bei ~ offset(log(grad)),data=bei.extra)
fitnull <- ppm(bei ~1)
AIC(fitprop)
AIC(fitnull)


###################################################
### code chunk number 140: 09inferpois.Rnw:2980-2982
###################################################
fitxy <- ppm(swedishpines ~ x + y)
drop1(fitxy)


###################################################
### code chunk number 141: 09inferpois.Rnw:2987-2989
###################################################
fitcsr <- ppm(swedishpines ~ 1)
add1(fitcsr, ~x+y)


###################################################
### code chunk number 142: 09inferpois.Rnw:3003-3006
###################################################
fitxy <- ppm(swedishpines ~ x + y)
fitopt <- step(fitxy)
fitopt


###################################################
### code chunk number 143: 09inferpois.Rnw:3022-3023
###################################################
bigfit <- ppm(swedishpines ~ polynom(x,y,3))


###################################################
### code chunk number 144: 09inferpois.Rnw:3025-3026 (eval = FALSE)
###################################################
## formula(bigfit)


###################################################
### code chunk number 145: 09inferpois.Rnw:3028-3029
###################################################
spatstat:::splat(pasteFormula(formula(bigfit)))


###################################################
### code chunk number 146: 09inferpois.Rnw:3031-3034
###################################################
goodfit <- step(bigfit, trace=0)
formula(goodfit)
AIC(goodfit)


###################################################
### code chunk number 147: 09inferpois.Rnw:3061-3062
###################################################
X <- simulate(fitprop)[[1]]


###################################################
### code chunk number 148: Bei.Rnw:3-5
###################################################
newplot(12,0.8)
setmargins(0)


###################################################
### code chunk number 149: 09inferpois.Rnw:3068-3069
###################################################
plot(X, main="", pch=3, cex=0.75)


###################################################
### code chunk number 150: 09inferpois.Rnw:3135-3138
###################################################
fit2 <- ppm(bei ~ sqrt(grad) + x, data=bei.extra)
mo <- model.images(fit2)
names(mo)


###################################################
### code chunk number 151: 09inferpois.Rnw:3228-3229 (eval = FALSE)
###################################################
## ppm(gold ~ bs(dfault, 5), data=mur, use.gam=TRUE)


###################################################
### code chunk number 152: 09inferpois.Rnw:3886-3888
###################################################
qat <- quadscheme(cells, nd=8, nt=4)
qde <- quadscheme(cells, nd=8, method="dirichlet")


###################################################
### code chunk number 153: 09inferpois.Rnw:3891-3892
###################################################
setmargins(0)


###################################################
### code chunk number 154: 09inferpois.Rnw:3896-3899
###################################################
plot(solist(qde, qat), main="", main.panel="",
     pch=16, cex=1.25, dum=list(pch=3, cex=1.25),
     tiles=TRUE, lwd=3, mar.panel=0, hsep=1)


###################################################
### code chunk number 155: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 156: 09inferpois.Rnw:3956-3957 (eval = FALSE)
###################################################
## quadscheme(data, dummy, ..., method="grid")


###################################################
### code chunk number 157: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 158: gexAgain
###################################################
gor <- rescale(unmark(gorillas), 1000, unitname="km")
gex <- lapply(gorillas.extra, rescale, 
              s=1000, unitname="km")
shorten <- function(x) substr(x, 1, 4) 
names(gex) <- shorten(names(gex))
isfactor <- !unlist(lapply(lapply(gex, levels), is.null))
for(i in which(isfactor))
  levels(gex[[i]]) <- shorten(levels(gex[[i]]))


###################################################
### code chunk number 159: 09inferpois.Rnw:4060-4061
###################################################
ppm(gor ~ vege, data=gex)


###################################################
### code chunk number 160: 09inferpois.Rnw:4066-4068
###################################################
vt <- tess(image=gex$vege)
intensity(quadratcount(gor, tess=vt))


###################################################
### code chunk number 161: 09inferpois.Rnw:4074-4075
###################################################
fitveg2 <- ppm(gor ~ vege - 1, data=gex, nd=256)


###################################################
### code chunk number 162: 09inferpois.Rnw:4077-4078
###################################################
exp(coef(fitveg2))


###################################################
### code chunk number 163: 09inferpois.Rnw:4083-4085
###################################################
Q <- pixelquad(gor, gex$vege)
fitveg3 <- ppm(Q ~ vege - 1, data=gex)


###################################################
### code chunk number 164: 09inferpois.Rnw:4087-4088
###################################################
exp(coef(fitveg3))


###################################################
### code chunk number 165: 09inferpois.Rnw:4127-4128
###################################################
set.seed(191919)


###################################################
### code chunk number 166: 09inferpois.Rnw:4130-4133
###################################################
dfdata <- as.data.frame(lapply(gex, "[", i=gor))
samp <- rSSI(0.5, 42, Window(gor))
dfsamp <- as.data.frame(lapply(gex, "[", i=samp))


###################################################
### code chunk number 167: 09inferpois.Rnw:4137-4140
###################################################
G <- quadscheme(gor, samp, method="d")
df <- rbind(dfdata, dfsamp)
fitdf <- ppm(G ~ vege - 1, data=df)


###################################################
### code chunk number 168: 09inferpois.Rnw:4142-4143
###################################################
exp(coef(fitdf))


###################################################
### code chunk number 169: 09inferpois.Rnw:4192-4200
###################################################
set.seed(19421492)
Xpix <- runifpoint(70)
Qpix <- quadratcount(Xpix, 7)
Ipix <- Qpix
Ipix[] <- 1 * (Qpix > 0)
Wpix <- square(1)
pointcolour <- grey(0.2)
tilecolour <- grey(0.7)


###################################################
### code chunk number 170: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 171: 09inferpois.Rnw:4207-4211
###################################################
plot(Xpix, pch=16, cols=pointcolour, main="")
plot(Qpix, col=tilecolour, add=TRUE,
     textargs=list(col="black", cex=1.5))
plot(Wpix, add=TRUE)


###################################################
### code chunk number 172: 09inferpois.Rnw:4213-4217
###################################################
plot(Xpix, pch=16, cols=pointcolour, main="")
plot(Ipix, col=tilecolour, add=TRUE,
     textargs=list(col="black", cex=1.5))
plot(Wpix, add=TRUE)


###################################################
### code chunk number 173: 09inferpois.Rnw:4555-4556
###################################################
fit <- slrm(bei ~ grad, data=bei.extra)


###################################################
### code chunk number 174: 09inferpois.Rnw:4558-4559
###################################################
fit


###################################################
### code chunk number 175: 09inferpois.Rnw:4836-4837
###################################################
set.seed(42)


###################################################
### code chunk number 176: 09inferpois.Rnw:4839-4841
###################################################
fitM <- ppm(bei ~ grad, data=bei.extra)
fitL <- ppm(bei ~ grad, data=bei.extra, method="logi")


###################################################
### code chunk number 177: 09inferpois.Rnw:4843-4845
###################################################
coef(fitM)
coef(fitL)


###################################################
### code chunk number 178: 09inferpois.Rnw:4923-4929
###################################################
X <- split(chorley)$larynx
D <- split(chorley)$lung
incin <- as.ppp(chorley.extra$incin, W = Window(chorley))
dincin <- distfun(incin)
Q <- quadscheme.logi(X,D)
fit <- ppm(Q~dincin)


###################################################
### code chunk number 179: 09inferpois.Rnw:4961-4962
###################################################
fitVB <- ppm(bei ~ grad, data=bei.extra, method="VBlogi")


###################################################
### code chunk number 180: 09inferpois.Rnw:4964-4965
###################################################
coef(fitVB)


###################################################
### code chunk number 181: 09inferpois.Rnw:4980-4982
###################################################
fitVBp <- ppm(bei ~ grad, data=bei.extra,
              prior.mean = c(0,0), prior.var = diag(c(10000,0.01)))


###################################################
### code chunk number 182: 09inferpois.Rnw:4984-4985
###################################################
coef(fitVBp)


###################################################
### code chunk number 183: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 184: 09inferpois.Rnw:5062-5063 (eval = FALSE)
###################################################
## profilepl(s, f, ...)


###################################################
### code chunk number 185: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 186: profileNZ
###################################################
thresh <- function(x,y,a) { x < a }
df <- data.frame(a=1:152)
nzfit <- profilepl(df, Poisson, nztrees ~ thresh, eps=0.5)


###################################################
### code chunk number 187: 09inferpois.Rnw:5090-5091
###################################################
nzfit


###################################################
### code chunk number 188: fvSquat.Rnw:3-5
###################################################
newplot(6, 0.65)
setmargins(0.5+c(3,3,0,0))


###################################################
### code chunk number 189: 09inferpois.Rnw:5103-5104
###################################################
plot(nzfit, main="", lwd=2, col.opt=1, lty.opt=2)


###################################################
### code chunk number 190: chorleyload
###################################################
if(draftversion) {
  load(datafilepath("chorleyCoarse.rda"))
  draftmessage <- 
    "using coarse quadrature scheme, eps=0.5, in draft version"
} else {
  load(datafilepath("chorley.rda"))
  draftmessage <- NULL
}


###################################################
### code chunk number 191: 09inferpois.Rnw:5232-5235 (eval = FALSE)
###################################################
##   lung <- split(chorley)$lung
##   larynx <- split(chorley)$larynx
##   Q <- quadscheme(larynx, eps=0.1)


###################################################
### code chunk number 192: 09inferpois.Rnw:5245-5247 (eval = FALSE)
###################################################
## smo <- density(lung, sigma=0.15, eps=0.1)
## smo <- eval.im(pmax(smo, 1e-10))


###################################################
### code chunk number 193: 09inferpois.Rnw:5258-5259
###################################################
CRfit0 <- ppm(Q ~ offset(log(smo)))


###################################################
### code chunk number 194: 09inferpois.Rnw:5261-5262 (eval = FALSE)
###################################################
## ppm(Q ~ offset(log(smo)))


###################################################
### code chunk number 195: 09inferpois.Rnw:5264-5265
###################################################
CRfit0


###################################################
### code chunk number 196: chorleyDfun
###################################################
d2incin <- function(x, y, xincin=354.5, yincin=413.6) {
  (x - xincin)^2 + (y - yincin)^2
}


###################################################
### code chunk number 197: 09inferpois.Rnw:5325-5328
###################################################
raisin <- function(x,y, alpha, beta) {
  1 + alpha * exp( - beta * d2incin(x,y))
}


###################################################
### code chunk number 198: chorleyDfit
###################################################
chorleyDfit <- ippm(Q ~offset(log(smo) + log(raisin)), 
                    start=list(alpha=5, beta=1))


###################################################
### code chunk number 199: 09inferpois.Rnw:5342-5345
###################################################
parDfit <- parameters(chorleyDfit)
alphahat <- parDfit$alpha
betahat <-  parDfit$beta


###################################################
### code chunk number 200: 09inferpois.Rnw:5347-5348
###################################################
chorleyDfit


###################################################
### code chunk number 201: 09inferpois.Rnw:5356-5357
###################################################
unlist(parameters(chorleyDfit))


###################################################
### code chunk number 202: 09inferpois.Rnw:5408-5412 (eval = FALSE)
###################################################
## X <- rotate(copper$Points, pi/2)
## L <- rotate(copper$Lines, pi/2)
## D <- distfun(L)
# NOT YET RELEASED IN SPATSTAT:
## copfit <- locppm(X ~ D, eps=1.5, sigma=bw.locppm)


###################################################
### code chunk number 203: calcCopper
###################################################
# if(!file.exists("data/Copper.rda")) {
#  copperBW <- 14.6
#  coppereps <- 1.5
# NOT YET RELEASED IN SPATSTAT:
#  CopFit <- locppm(X, ~D, covariates=list(D=D), eps=coppereps, 
#                   vcalc="full", locations="fine", sigma=copperBW)
#  CopLambda <- predict(CopFit)
#  save(CopFit, CopLambda, file="data/Copper.rda", compress=TRUE)
# } else load("data/Copper.rda")


###################################################
### code chunk number 204: Copper2FullBottom.Rnw:3-5
###################################################
newplot(15, 1)
setmargins(0)


###################################################
### code chunk number 205: 09inferpois.Rnw:5428-5429
###################################################
setmargins(1,0,0,0)


###################################################
### code chunk number 206: 09inferpois.Rnw:5433-5437
###################################################
## conto <- function(i, y, ...) {contour(y, ...)}
## NOT YET RELEASED IN SPATSTAT:
## plot(CopFit, main="", main.panel="", mar.panel=0, hsep=1, 
     equal.scales=TRUE, ribside="bottom", ribsep=0.05,
     panel.end=conto)


###################################################
### code chunk number 207: CopperFullBottom.Rnw:3-5
###################################################
newplot(15, 0.8)
setmargins(0)


###################################################
### code chunk number 208: 09inferpois.Rnw:5461-5463
###################################################
## NOT YET RELEASED IN SPATSTAT:
# plot(CopLambda, main="", ribside="bottom", ribsep=0.05,
#     style="imagecontour", contourargs=list(col="white", drawlabels=FALSE))


###################################################
### code chunk number 209: 09inferpois.Rnw:5609-5610
###################################################
options(dopt)


