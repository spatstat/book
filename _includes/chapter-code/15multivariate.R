### R code from vignette source '15multivariate.Rnw'
## Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner

###################################################
### code chunk number 1: 15multivariate.Rnw:9-10
###################################################
source("R/startup.R")


###################################################
### code chunk number 2: SprucesL.Rnw:3-5
###################################################
newplot(7.1, 0.9)
setmargins(0,0.5,0,0)


###################################################
### code chunk number 3: 15multivariate.Rnw:133-134
###################################################
plot(spruces, main="", markscale=4)


###################################################
### code chunk number 4: Unit2L.Rnw:3-5
###################################################
newplot(13, 0.9)
setmargins(0,4,0,0)


###################################################
### code chunk number 5: 15multivariate.Rnw:171-172
###################################################
setmargins(0,0.7,0,0)


###################################################
### code chunk number 6: 15multivariate.Rnw:176-177
###################################################
plot(finpines, main="", markscale=0.1, mar.panel=0, hsep=1.2)


###################################################
### code chunk number 7: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 8: 15multivariate.Rnw:216-220
###################################################
par(mfrow=c(1,2))
hist(marks(spruces), main="")
hist(marks(longleaf), main="")
par(mfrow=c(1,1))


###################################################
### code chunk number 9: 15multivariate.Rnw:234-236 (eval = FALSE)
###################################################
## Hp <- anylapply(marks(finpines), hist, plot=FALSE) 
## plot(Hp)


###################################################
### code chunk number 10: 15multivariate.Rnw:240-241 (eval = FALSE)
###################################################
## pairs(marks(finpines))


###################################################
### code chunk number 11: 15multivariate.Rnw:244-246 (eval = FALSE)
###################################################
## pairs(marks(finpines), diag.panel=panel.histogram)
## with(marks(finpines), plot(diameter ~ height))


###################################################
### code chunk number 12: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 13: 15multivariate.Rnw:257-258
###################################################
with(marks(finpines), plot(diameter ~ height))


###################################################
### code chunk number 14: 15multivariate.Rnw:268-270
###################################################
vols <- with(marks(finpines), (pi/12) * height * diameter^2)
X <- finpines %mark% vols


###################################################
### code chunk number 15: 15multivariate.Rnw:289-290 (eval = FALSE)
###################################################
## pairs(as.data.frame(longleaf))


###################################################
### code chunk number 16: 15multivariate.Rnw:311-312
###################################################
Y <- cut(longleaf, breaks=c(0, 30, Inf), labels=c("J","A"))


###################################################
### code chunk number 17: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 18: 15multivariate.Rnw:325-327
###################################################
X <- cut(longleaf,breaks=c(0,30,Inf),right=FALSE,labels=c("J","A"))
plot(X,main="",chars=c(20,1))


###################################################
### code chunk number 19: 15multivariate.Rnw:351-352 (eval = FALSE)
###################################################
## NB <- cut(nbfires, "fnl.size", breaks=4)


###################################################
### code chunk number 20: 15multivariate.Rnw:360-361
###################################################
big.old <- subset(nbfires, fnl.size > 10 & dis.date < "2001-01-01")


###################################################
### code chunk number 21: 15multivariate.Rnw:368-370
###################################################
longsmooth <- Smooth(longleaf, bw.smoothppp)
longnear   <- nnmark(longleaf)


###################################################
### code chunk number 22: 15multivariate.Rnw:396-397 (eval = FALSE)
###################################################
## longsmooth <- Smooth(longleaf, bw.smoothppp)


###################################################
### code chunk number 23: Unit2R.Rnw:3-5
###################################################
newplot(13, 0.9)
setmargins(0,0,0,4)


###################################################
### code chunk number 24: 15multivariate.Rnw:408-411
###################################################
plot(solist(longsmooth, longnear),
     main="", main.panel="", 
     equal.ribbon=TRUE, mar.panel=0, hsep=1)


###################################################
### code chunk number 25: 15multivariate.Rnw:496-497
###################################################
msd <- sqrt(markvar(longleaf, bw.smoothppp))


###################################################
### code chunk number 26: 15multivariate.Rnw:503-511
###################################################
mfit <- Smooth(longleaf, bw.smoothppp, at="points")
res <- marks(longleaf) - mfit
stuff <- solist(layered(msd, 
                        plotargs=list(ribside="bottom")), 
                layered(setmarks(longleaf, res), 
                        plotargs=list(leg.side="bottom")),
                layered(idw(longleaf),
                        plotargs=list(ribside="bottom")))


###################################################
### code chunk number 27: 15multivariate.Rnw:516-517
###################################################
setmargins(0)


###################################################
### code chunk number 28: 15multivariate.Rnw:522-525
###################################################
plot(stuff,
     main="", main.panel="", equal.scales=TRUE, valign=TRUE,
     mar.panel=c(1,0,0,0), hsep=0.5)


###################################################
### code chunk number 29: 15multivariate.Rnw:541-541
###################################################



###################################################
### code chunk number 30: 15multivariate.Rnw:553-555 (eval = FALSE)
###################################################
## mfit <- Smooth(longleaf, bw.smoothppp, at="points")
## res <- marks(longleaf) - mfit


###################################################
### code chunk number 31: 15multivariate.Rnw:703-705 (eval = FALSE)
###################################################
## envelope(spruces, markcorr, nsim=39, 
##          simulate=expression(rlabel(spruces)))


###################################################
### code chunk number 32: 15multivariate.Rnw:713-716
###################################################
set.seed(27291)
emcs <- envelope(spruces, markcorr, nsim=39, 
                 simulate=expression(rlabel(spruces)))


###################################################
### code chunk number 33: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 34: 15multivariate.Rnw:722-724
###################################################
plot(emcs, main="", ylim.covers=c(0.5, 1.1), 
     legendpos="top", legendargs=list(cex=0.85))


###################################################
### code chunk number 35: 15multivariate.Rnw:787-792
###################################################
set.seed(6876109)
smki <- Kmark(spruces, returnL=TRUE)
smke <- envelope(spruces, Kmark, returnL=TRUE,
                 simulate=expression(rlabel(spruces)),
                 nsim=1999, nrank=50, savefuns=TRUE)


###################################################
### code chunk number 36: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 37: 15multivariate.Rnw:798-799 (eval = FALSE)
###################################################
## Kmark(X, f, ...)


###################################################
### code chunk number 38: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 39: 15multivariate.Rnw:808-812 (eval = FALSE)
###################################################
## smki <- Kmark(spruces, returnL=TRUE)
## smke <- envelope(spruces, Kmark, returnL=TRUE,
##                  simulate=expression(rlabel(spruces)),
##                  nsim=1999, nrank=50, savefuns=TRUE)


###################################################
### code chunk number 40: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 41: 15multivariate.Rnw:824-827
###################################################
plot(anylist(smki, smke), main.panel="", main="",
     panel.args=function(i) if(i == 1) list() else list(. - mmean ~ r),
     legend=FALSE)


###################################################
### code chunk number 42: 15multivariate.Rnw:847-849
###################################################
Lspruce <- Lest(spruces)
diffL <- eval.fv(smki - Lspruce)


###################################################
### code chunk number 43: 15multivariate.Rnw:902-903
###################################################
mv <- markvario(spruces)


###################################################
### code chunk number 44: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 45: 15multivariate.Rnw:908-909
###################################################
plot(mv, main="", legendpos="top")


###################################################
### code chunk number 46: 15multivariate.Rnw:951-953
###################################################
ES <- Emark(spruces)
VS <- Vmark(spruces)


###################################################
### code chunk number 47: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 48: 15multivariate.Rnw:958-960
###################################################
plot(anylist(ES, VS), main="", main.panel="",
     panel.args=function(i) list(ylim.covers=if(i==1) c(0,0.3) else 0))


###################################################
### code chunk number 49: 15multivariate.Rnw:984-985
###################################################
nnmean(spruces)


###################################################
### code chunk number 50: 15multivariate.Rnw:997-998
###################################################
nnvario(spruces)


###################################################
### code chunk number 51: 15multivariate.Rnw:1029-1031
###################################################
nncorr(spruces)
nncorr(finpines)


###################################################
### code chunk number 52: 15multivariate.Rnw:1043-1045
###################################################
rr <- nncorr(spruces)[["correlation"]]
sqrt(2 * (1-rr^2)/(npoints(spruces)-4))


###################################################
### code chunk number 53: Unit.Rnw:3-5
###################################################
newplot(6, 0.7)
setmargins(0)


###################################################
### code chunk number 54: 15multivariate.Rnw:1078-1080
###################################################
X <- osteo$pts[[36]]
plot(X, main="", pch=21, bg='white', cex=1.2)


###################################################
### code chunk number 55: 15multivariate.Rnw:1136-1137
###################################################
X <- osteo$pts[[36]]


###################################################
### code chunk number 56: 15multivariate.Rnw:1145-1149 (eval = FALSE)
###################################################
## copyExampleFiles("osteo")
## xyz <- read.table("osteo36.txt", header=TRUE)
## b <- box3(c(0,81),c(0,100),c(-100,0),unitname=c("micron", "microns"))
## X <- with(xyz, pp3(x, y, z, b))


###################################################
### code chunk number 57: 15multivariate.Rnw:1151-1153
###################################################
X
summary(X)


###################################################
### code chunk number 58: 15multivariate.Rnw:1199-1200 (eval = FALSE)
###################################################
## plot(X, main="", pch=21, bg='white', cex=1.2)


###################################################
### code chunk number 59: PromptOff.Rnw:1-2
###################################################
options(prompt="  ")


###################################################
### code chunk number 60: 15multivariate.Rnw:1270-1271 (eval = FALSE)
###################################################
## K3est(X, ..., rmax, nrval, correction)


###################################################
### code chunk number 61: PromptOn.Rnw:1-2
###################################################
options(prompt="> ")


###################################################
### code chunk number 62: 15multivariate.Rnw:1284-1286
###################################################
KX <- K3est(X)
EK <- envelope(X, K3est, nsim=1999, nrank=50, nrval=512)


###################################################
### code chunk number 63: fv2.Rnw:3-5
###################################################
newplot(12, 0.95)
setmargins(0.5+c(3,3,0,1))


###################################################
### code chunk number 64: 15multivariate.Rnw:1292-1297
###################################################
pan <- function(i) {
  if(i == 1) list() else list(fmla= sqrt(.) ~ r, legendargs=list(cex=0.85))
}
plot(anylist(KX, EK), main="", main.panel="", 
     mar.panel=c(3,3,0,0), hsep=2, panel.args=pan)


###################################################
### code chunk number 65: 15multivariate.Rnw:1340-1342 (eval = FALSE)
###################################################
## E <- envelope(X, K3est, nsim=1999, nrank=50, nrval=512)
## plot(E, sqrt(.) ~ r)


###################################################
### code chunk number 66: 15multivariate.Rnw:1380-1381
###################################################
p3 <- pcf3est(X)


###################################################
### code chunk number 67: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 68: 15multivariate.Rnw:1386-1387
###################################################
plot(p3, main="")


###################################################
### code chunk number 69: 15multivariate.Rnw:1425-1426
###################################################
G3 <- G3est(X)


###################################################
### code chunk number 70: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 71: 15multivariate.Rnw:1431-1432
###################################################
plot(G3, main="", legend=FALSE)


###################################################
### code chunk number 72: 15multivariate.Rnw:1449-1452
###################################################
v <- eroded.volumes(domain(X), c(0, 10, 20, 30))
f <- v/v[1]
round(f, 2)


###################################################
### code chunk number 73: 15multivariate.Rnw:1530-1531
###################################################
F3 <- F3est(X, sphere="digital")


###################################################
### code chunk number 74: fv.Rnw:3-5
###################################################
newplot(6, 0.5)
setmargins(0.5+c(3,3,1,0))


###################################################
### code chunk number 75: 15multivariate.Rnw:1537-1538
###################################################
plot(F3, main="")


###################################################
### code chunk number 76: 15multivariate.Rnw:1615-1622
###################################################
df <- data.frame(x=runif(100, max=3), 
                 y=runif(100, max=3), 
                 z=runif(100, max=2),
                 t=runif(100))
bb <- boxx(c(0,3), c(0,3), c(0,2), c(0,1))
X <- ppx(data=df, domain=bb, coord.type=c("s","s", "s", "t"))
X


###################################################
### code chunk number 77: 15multivariate.Rnw:1631-1633
###################################################
marks(X) <- with(as.hyperframe(df), disc(centre=c(x,y)))
X


