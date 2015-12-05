### R code from vignette source '01intro.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 01intro.Rnw:9-10
###################################################
source("R/startup.R")


###################################################
### code chunk number 2: 01intro.Rnw:41-42
###################################################
ECL <- split(mucosa)[[1]]


###################################################
### code chunk number 3: 01intro.Rnw:50-53
###################################################
plot(solist(layered(ECL, plotargs=list(cex=0.75, pch=16)), 
            layered(unmark(shapley), plotargs=list(pch="."))), 
     main="", main.panel="", mar.panel=0.2)


###################################################
### code chunk number 4: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 5: 01intro.Rnw:105-108
###################################################
plot(waterstriders, main="", main.panel="",
     equal.scales=TRUE, mar.panel=rep(0.2,4), hsep=1,
     pch=16, cex=0.75)


###################################################
### code chunk number 6: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 7: 01intro.Rnw:124-125
###################################################
plot(residualspaper[["Fig1"]], pch=16, main="", cex=0.9)


###################################################
### code chunk number 8: Urkiola.Rnw:3-5
###################################################
newplot(6, 0.65)
setmargins(0.1)


###################################################
### code chunk number 9: 01intro.Rnw:197-201
###################################################
a <- plot(urkiola, chars=c(3,16), cex=0.7,
          cols=grey(c(0.4, 0)), legend=FALSE, main="")
plot(update(a, cex=1.2), add=TRUE, xlim=c(32,72), ylim=c(6,52), 
     vertical=TRUE, side="left")


###################################################
### code chunk number 10: Amacrine.Rnw:3-5
###################################################
newplot(5.5,0.8)
setmargins(0, 1, 0, 0)


###################################################
### code chunk number 11: 01intro.Rnw:227-229
###################################################
zeromargins() # remove all margins
setmargins(0,0,0,0.1) # back off a little


###################################################
### code chunk number 12: 01intro.Rnw:234-237
###################################################
plot(amacrine, main="", chars=c(1,16), 
     cols=if(monochrome) grey(c(0, 0.4)) else c("red", "blue"), 
     leg.side="left")


###################################################
### code chunk number 13: 01intro.Rnw:263-264
###################################################
flu19 <- flu[19, 1, drop=TRUE]


###################################################
### code chunk number 14: UnitL.Rnw:3-5
###################################################
newplot(9, 0.7)
setmargins(0.1+ c(0,2,0,0))


###################################################
### code chunk number 15: 01intro.Rnw:268-269
###################################################
setmargins(0)


###################################################
### code chunk number 16: 01intro.Rnw:275-277
###################################################
flucols <- if(monochrome) grey(c(0,0.4)) else c("blue", "pink")
plot(flu19, main="", chars=c(1,3), cols=flucols, cex=c(1,0.7))


###################################################
### code chunk number 17: AntsUR.Rnw:3-6
###################################################
newplot(6,0.65)
zeromargins()
setmargins(0,0,0,2)


###################################################
### code chunk number 18: 01intro.Rnw:332-340
###################################################
sy <- plot(ants, chars=c(1,3), legend=FALSE, main="")
plot(sy, xlim=c(635,750),ylim=c(525,735), 
     add=TRUE, vertical=TRUE, side="right")
lines(ants.extra$trackNE)
lines(ants.extra$trackSW)
lines(ants.extra$fieldscrub, lwd=2)
text(357, 517, "scrub", font=4)
text(582, 212, "field", font=4)


###################################################
### code chunk number 19: UnitL.Rnw:3-5
###################################################
newplot(9, 0.7)
setmargins(0.1+ c(0,2,0,0))


###################################################
### code chunk number 20: 01intro.Rnw:393-394
###################################################
plot(longleaf, main="", leg.side="left")


###################################################
### code chunk number 21: BronzeL.Rnw:3-4
###################################################
setmargins(0, 0.1, 0,0)


###################################################
### code chunk number 22: 01intro.Rnw:421-422
###################################################
plot(bronzefilter, main="", markscale=2)


###################################################
### code chunk number 23: BeiR.Rnw:3-5
###################################################
newplot(13,0.95)
setmargins(0,0,0,1)


###################################################
### code chunk number 24: 01intro.Rnw:472-474
###################################################
plot(bei.extra[["elev"]], main="")
plot(bei, add=TRUE, pch="+")


###################################################
### code chunk number 25: Copper.Rnw:3-5
###################################################
newplot(12, 0.8)
setmargins(0)


###################################################
### code chunk number 26: 01intro.Rnw:493-495
###################################################
plot(rotate(copper$Lines, pi/2), main="")
plot(rotate(copper$Points, pi/2), add=TRUE, pch=16)


###################################################
### code chunk number 27: Chorley.Rnw:3-5
###################################################
newplot(8, 0.5)
setmargins(0)


###################################################
### code chunk number 28: chorleydata
###################################################
chco <- if(monochrome) c("grey", "black", "black") else
                       c("green", "red", "blue")
chorley.extra[["plotit"]](main="", cols=chco)


###################################################
### code chunk number 29: Chicago.Rnw:3-5
###################################################
newplot(5, 0.75)
setmargins(0)


###################################################
### code chunk number 30: 01intro.Rnw:583-584
###################################################
plot(unmark(chicago), main="", col="grey", lwd=2, cols="black", pch=16)


###################################################
### code chunk number 31: 01intro.Rnw:632-633
###################################################
setmargins(0)


###################################################
### code chunk number 32: 01intro.Rnw:638-643
###################################################
ape1 <- osteo[osteo$shortid==4, "pts", drop=TRUE]
plot(ape1[1:6], 
     main="", main.panel="", ncols=3,
     mar.panel=0.1, hsep=1, vsep=1,
     cex=1.5, pch=21, bg='white')


###################################################
### code chunk number 33: 01intro.Rnw:670-672
###################################################
wmd <- unlist(lapply(waterstriders, function(x) mean(nndist(x))))
wmd <- round(10 * wmd)


###################################################
### code chunk number 34: 01intro.Rnw:684-686
###################################################
wtf <- unlist(lapply(waterstriders, clarkevans, correction="D"))
wtf <- round(wtf, 3)


###################################################
### code chunk number 35: 01intro.Rnw:742-744
###################################################
wpc <- anylapply(waterstriders, pcf, 
                 correction="iso", adjust=1.5, divisor="d")


###################################################
### code chunk number 36: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 37: 01intro.Rnw:751-756
###################################################
plot(wpc,
     main="", main.panel="", 
     hsep=1, mar.panel=0.2+c(3,3,0,0), 
     legend=FALSE, equal.scales=TRUE,
     cex.lab=1.2, cex.axis=1.2)


###################################################
### code chunk number 38: waterenvelope
###################################################
## compute pointwise 5% significance bands 
wpce <- anylapply(waterstriders, envelope, fun=pcf, 
                  correction="iso", adjust=1.5, divisor="d", 
                  nsim=1999, nrank=50, verbose=FALSE)


###################################################
### code chunk number 39: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 40: 01intro.Rnw:788-794
###################################################
plot(wpce, 
     pmin(., 2) ~ r,
     main="", main.panel="", 
     hsep=1, mar.panel=0.2+c(3,3,0,0),
     lwd=2, legend=FALSE, equal.scales=TRUE,
     ylab=expression(g(r)), cex.lab=1.2, cex.axis=1.2)


###################################################
### code chunk number 41: 01intro.Rnw:821-833
###################################################
ECL <- split(mucosa)[[1]]
## quadrat counts superimposed on grey point pattern
qc <- quadratcount(ECL, 5, 4)
qc <- layered(ECL, qc,
              plotargs=list(list(cols="grey", pch=16), list()))
## white point pattern superimposed on colour/greyscale map + contour of density
dens <- density(ECL)
dens <- layered(dens, dens, ECL, 
                plotargs=list(list(ribargs=list(las=1)),
                              list(.plot="contour"),
                              list(pch=16, cols="white")))
if(monochrome) layerplotargs(dens)[[1]]$col <- grey(seq(1,0,length=128)^2)


###################################################
### code chunk number 42: 01intro.Rnw:837-838
###################################################
setmargins(0,0,0,1)


###################################################
### code chunk number 43: 01intro.Rnw:843-845
###################################################
plot(solist(qc, dens), main="", main.panel="",
     equal.scales=TRUE, mar.panel=c(0,0,0,1))


###################################################
### code chunk number 44: 01intro.Rnw:954-961
###################################################
beikm <- rescale(bei, s=1000, unitname="km")
bei.extra.km <- lapply(bei.extra, rescale, s=1000, unitname="km")
attach(bei.extra.km)
rB <- rhohat(beikm, elev)
fitB <- ppm(beikm~polynom(elev,2))
lamB <- effectfun(fitB, "elev", se.fit=TRUE)
detach(bei.extra.km)


###################################################
### code chunk number 45: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 46: 01intro.Rnw:969-972
###################################################
plot(anylist(rB, lamB), 
     main="", main.panel="", mar.panel=c(3.1,3,0.1,0), hsep=1, 
     equal.scales=TRUE, ylab="Intensity", legend=FALSE)


###################################################
### code chunk number 47: 01intro.Rnw:1000-1006
###################################################
## fit Softcore models
watsoft <- lapply(waterstriders, ppm, ~1, Softcore(0.5), rbord=10)
## extract fitted interactions
watint <- lapply(watsoft, fitin)
## convert to 'fv' objects
watint <- anylapply(watint, plot, plotit=FALSE)


###################################################
### code chunk number 48: fv3.Rnw:3-5
###################################################
newplot(12.5, 1.0)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 49: 01intro.Rnw:1013-1017
###################################################
plot(watint, 
     mar.panel=c(4,4,1,1), main.panel="", main="", 
     xlim=c(0, 6), legend=FALSE,
     ylab="potential")


###################################################
### code chunk number 50: simSoft
###################################################
## simulate Softcore models
## takes approx 3 * 40 = 120 sec 
##        (for default nrep=5e5)
set.seed(1234321)
watsim <- lapply(watsoft, simulate, nsim=1)
watsim <- anylapply(watsim, "[[", i=1)


###################################################
### code chunk number 51: Unit3.Rnw:3-5
###################################################
newplot(19,0.9)
zeromargins()


###################################################
### code chunk number 52: 01intro.Rnw:1057-1058
###################################################
setmargins(0.2)


###################################################
### code chunk number 53: 01intro.Rnw:1063-1066
###################################################
plot(watsim, main="", main.panel="",
     equal.scales=TRUE, mar.panel=rep(0.2,4), hsep=1,
     pch=16, cex=0.75)


###################################################
### code chunk number 54: 01intro.Rnw:1086-1091
###################################################
infB <- influence(fitB)
resB <- Smooth(residuals(fitB, type="Pearson"))
layerplotargs(infB) <- list(maxsize=0.05, 
                            leg.args=list(labelmap=1000))
layerplotargs(resB) <- list(.plot="contour")


###################################################
### code chunk number 55: Bei2r.Rnw:3-5
###################################################
newplot(8.5,1)
setmargins(0.1, 0.1, 0.1, 1.6)


###################################################
### code chunk number 56: 01intro.Rnw:1095-1096
###################################################
setmargins(0,1,0,0)


###################################################
### code chunk number 57: 01intro.Rnw:1101-1104
###################################################
plot(solist(infB, resB), 
     main="", main.panel="", equal.scales=TRUE,
     mar.panel=c(0,1,0,0))


