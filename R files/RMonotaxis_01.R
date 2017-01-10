# LAZILY INPUTTING DATA BECAUSE I DONT FEEL LIKE MESSING WITH .CSVs
SLmuscle = c(30.0, 26.5, 26.0, 26.5, 28.0,
       22.0, 25.5, 30.5, 7.3, 5.6,
       11.8, 11.3, 19.0, 15.9, 15.0,
       9.6, 4.8, 3.8, 7.5, 12.5, 17.1)

MBF = 1.5*c(92.566, 53.271, 46.927, 67.043, 67.926,
        38.358, 57.123, 89.629, 0.784, 0.233,
        5.281, 9.878, 19.071, 11.025, 14.062,
        2.049, 0.13, 0.087, 1.08, 5.776, 13.441)

CBF = 1.5*c(42.753, 24.426, 21.299, 30.831, 30.442,
        18.043, 26.708, 41.807, 0.377, 0.105,
        2.37, 4.161, 8.963, 5.414, 6.435,
        0.968, 0.063, 0.037, 0.499, 2.675, 6.202)

MBF2 = 1.5*c(96.848, 51.812, 46.044, 60.142, 55.975,
         40.696, 60.332, 81.28, 0.823, 0.173,
         5.235, 2.285, 21.46, 10.929, 14.118, 
         2.019, 0.155, 0.096, 1.098, 4.938, 12.152)

CBF2 = 1.5*c(46.155, 23.447, 21.123, 28.498, 25.374,
         18.297, 27.922, 37.826, 0.398, 0.081,
         2.467, 1.027, 9.776, 5.351, 6.314,
         0.983, 0.076, 0.044, 0.522, 2.302, 5.927)

A1 = c(1.773, 1.580, .713, .787, 1.701,
       .355, .778, 1.483, .008, .004,
       .039, .039, .308, .145, .252,
       .031, .003, .001, .014, .077, .240)

A2 = c(8.754, 5.151, 4.354, 5.905, 5.832,
       3.096, 4.864, 10.080, .025, .008,
       .210, .141, 1.151, .622, .729,
       .078, .004, .001, .028, .254, .953)

A3 = c(3.320, 2.019, 1.794, 2.157, 2.441,
       1.083, 1.813, 3.397, .011, .004,
       .118, .038, .647, .388, .315,
       .041, .001, .001, .018, .109, .491)



# messed around with low-order polynomials, but log transforms seem
# to yield best results in all cases
fit3 = lm(log(MBF)~log(SL))
summary(fit3)
par(mfrow=c(1,1))
plot(log(SL), log(MBF))


logSL=log(SL)
logA1=log(A1)
logA2=log(A2)
logA3=log(A3)

fitA1 = lm(logA1~logSL)
fitA2 = lm(logA2~logSL)
fitA3 = lm(logA3~logSL)
fitAM = lm((log(A1+A2+A3)~logSL))

summary(fitA1)
summary(fitA2)
summary(fitA3)
summary(fitAM)

# plotting all the muscle masses against size w/ log-transformed BFLs:
legendText1=expression(R^2~"="~"0.9861")
legendText2=expression(text=paste(R^2~"="~"0.9967"))
legendText3=expression(text=paste(R^2~"="~"0.9883"))
legendTextAM=expression(text=paste(R^2~"="~"0.9975"))

par(mfrow=c(2,2))
par(mar=c(5,5,4,3))
plot(SL, A1, col="blue", pch=19, 
     xlab="Standard length (cm)",
     ylab="A1 mass (g)",
     cex.lab=1.25)
curve(exp(-11.57406+3.53326*log(x)), col="blue", lty=2, lwd=2, add=TRUE)
text(120, 1500, legendText1, cex=2)

plot(SL, A2, col="darkgreen", pch=19,
     xlab="Standard length (cm)",
     ylab="A2 mass (g)",
     cex.lab=1.25)
curve(exp(-12.22022+4.25710*log(x)), col="darkgreen", lty=2, lwd=2, add=TRUE)
text(120, 8750, legendText2, cex=2)

plot(SL, A3, col="red", pch=19,
     xlab="Standard length (cm)",
     ylab="A3 mass (g)",
     cex.lab=1.25)
curve(exp(-12.5631+4.0805*log(x)), col="red", lty=2, lwd=2, add=TRUE)
text(120, 3000, legendText3, cex=2)

plot(SL, A1+A2+A3, col="grey", pch=19,
     xlab="Standard length (cm)",
     ylab="Total adductor mandibulae mass (g)",
     cex.lab=1.25)
curve(exp(-11.12177+4.05835*log(x)), col="grey", lty=2, lwd=2, add=TRUE)
text(120, 13000, legendTextAM, cex=2)


#A1 = coral
#A2 = cornflowerblue
#A3 = darkgoldenrod2
#total = seagreen
# plot all muscle subdivision masses on same plot?
total = A1+A2+A3
par(mfrow=c(1,1))
plot(SL, A1+A2+A3, col="seagreen", pch=17,cex = 1.2, 
     xlab = "Standard length (cm)",
     ylab = "Muscle mass (g)",
     cex.lab = 1.5)

points(SL, A1, col="tomato2", pch=15, cex = 1.2)
points(SL, A2, col="cornflowerblue", pch=19, cex = 1.2)
points(SL, A3, col="darkgoldenrod2", pch=18, cex = 1.5)
curve(exp(-11.57406+3.53326*log(x)), col="tomato2", lty=2, lwd=2, add=TRUE)
curve(exp(-12.22022+4.25710*log(x)), col="cornflowerblue", lty=2,lwd=2, add=TRUE)
curve(exp(-12.5631+4.0805*log(x)), col="darkgoldenrod2", lty=2,lwd=2, add=TRUE)
curve(exp(-11.12177+4.05835*log(x)), col="seagreen", lty=2, lwd=2,add=TRUE)
legend("topleft", inset=0.05, legend=c("Malaris","Rictalis", "Stegalis","Total"),
       col=c("tomato2", "cornflowerblue", "darkgoldenrod2", "seagreen"), 
       lwd=1,lty=2, pch=c(15, 19, 18, 17))

# log version of that?
plot(logSL, log(A1+A2+A3), col="grey", pch=17,
     main = "Log-transformed adductor mandibulae masses",
     ylab = "ln(A1 + A2 + A3) (mg)",
     xlab = "ln(Standard length) (mm)",
     cex.lab = 1.5)
legend("topleft", inset=0.05, legend=c("A1", "A2", "A3", "Total"),
       col=c("blue", "darkgreen", "red", "grey"), lwd=1,
       lty=2, pch=c(15, 19, 18, 17))
points(logSL, logA1, col="blue", pch=15)
points(logSL, logA2, col="darkgreen", pch=19)
points(logSL, logA3, col="red", pch=18)
abline(fitAM, col="grey", lty=2)
abline(fitA1, col="blue", lty=2)
abline(fitA2, col="darkgreen", lty=2)
abline(fitA3, col="red", lty=2)

# MBF and CBF?

logMBF1=log(MBF)
logMBF2=log(MBF2)
MBFavg=(MBF+MBF2)/2
logMBF=log(MBFavg)

CBFavg=(CBF+CBF2)/2
logCBF=log(CBFavg)

fitCBF=lm(logCBF~logSL)
summary(fitCBF)

fitMBF=lm(logMBF~logSL)
summary(fitMBF)


par(mfrow=c(1,1))
plot(SL, MBFavg, col="cornflowerblue", pch=19,
     main = "Bite force scaling with size",
     ylab = "Bite force (N)",
     xlab = "Standard length (cm)")
legend("topleft", inset=0.05, legend=c("Molar BF", "Canine BF"),
       col=c("cornflowerblue", "seagreen"), lwd=1,
       lty=2, pch=c(19, 18))
points(SL, CBFavg, col="seagreen", pch=18)
curve(exp(-6.53367+3.38225*log(x)), col="cornflowerblue", lty=2, add=TRUE)
curve(exp(-7.29437+3.37915*log(x)), col="seagreen", lty=2, add=TRUE)


plot(logSL, logMBF, col="blue", pch=19,
     main = "Bite force scaling with size",
     ylab = "ln Bite force (N)",
     xlab = "ln Standard length (cm)")
legend("topleft", inset=0.05, legend=c("Molariform bite force", "Conical bite force"),
       col=c("blue", "red"), lwd=1,
       lty=2, pch=c(19, 18))
points(logSL, logCBF, col="red", pch=18)
abline(fitMBF, col="blue", lty=2)
abline(fitCBF, col="red", lty=2)


#comparisons?
plot(logSL, logMBF, col="coral2", pch=19,
     main = "Bite force scaling with size",
     ylab = "ln Bite force (N)",
     xlab = "ln Standard length (cm)")
abline(fitMBF, col="coral2", lty=2)

# W/L relationships

length = c(4.8, 3.8, 15.0, 9.6, 7.5,
           16.8, 17.4, 16.3, 16.4, 12.4,
           9.5, 19.0, 15.9, 12.2, 8.3,
           9.5, 11.3, 5.6, 5.3, 5.4,
           7.6, 7.4)
weight = c(2.188, 0.989, 94.666, 20.322, 10.967,
           138.470, 146.559, 141.360, 161.300, 60.260,
           24.982, 165.010, 119.965, 53.860, 14.611,
           23.755, 40.084, 4.131, 2.759, 3.360,
           11.310, 9.817)

plot(length, weight)
plot(log(length), log(weight))
loglength=log(length)
logweight=log(weight)
fitWL = lm(logweight~loglength)
summary(fitWL)

lost_souls = data.frame(loglength=c(
  log(30.0), log(26.5), log(26), log(26.5), log(28.0),
  log(22.0), log(25.5), log(30.5), log(19.6), log(18.4),
  log(15.4), log(16.1), log(16.8), log(14.3), log(17.4),
  log(17.1)))

WL_predicts=predict(fitWL, lost_souls)
WL_predicts

# using Monotaxis_RformattedCSV
MLength = Monotaxis_RformattedCSV$Length
MWeight = Monotaxis_RformattedCSV$Weight
MMBF = Monotaxis_RformattedCSV$MBF

plot(MLength, MWeight)

plot(length, weight/length)

MG_SL = MG_WL$SL
MG_W = MG_WL$Weight
MG_BF = MG_WL$MBF

sum(MG_BF/MG_W)

#BFQ stuff
BFQ = read.csv("~/Google Drive/Lethrinids (Hannah)/Lethrinids/data/R files/FishBFQ.csv")
BFQM = BFQ$Mass
BFQB = BFQ$ABF
Species=BFQ$Species


plot(BFQM, BFQB)
logBM = log10(BFQM)
logBB = log10(BFQB)
plot(logBM, logBB)
text(logBM, logBB, labels=row.names(BFQ))
fitBFQ = lm(logBB~logBM)
summary(fitBFQ)

BFQpred = data.frame(logBM)
BFQ_predicts=exp(predict(fitBFQ, BFQpred))
BFQ_predicts

BFQ_final = (BFQB/BFQ_predicts)*100
BFQ_final

plot(BFQ_final[14])
Mg=Species[14]
text(BFQ_final[14], labels=Mg)
Others=BFQ_final[c(1:13,15:20)]
points(BFQ_final[1:13])
points(BFQfinal[15:20)])
text(BFQ_final, labels=Mg)
text(BFQ_final, labels=row.names(BFQ))



## CVA ##
CVAscores <- read.delim("~/Dropbox/Westneat Lab/Lethrinids/MonotaxisCVAscores.txt")
CV2 <- CVAscores$CV2
SL <- CVAscores$SL
CV2small <- CV2[SL =< 80]
CV2mid <- CV2[SL > 80 & SL < 187]
CV2large <- CV2[SL >= 187]

ks.test(CV2small, CV2mid)
ks.test(CV2small, CV2large)
ks.test(CV2large, CV2mid)
plot(SL, CVAscores$CV2)
SL <- MonotaxisPCscores$SL
fitprelim <- lm(CV2~SL)

fitsegmented <- segmented(fitprelim, seg.Z=~SL, psi=80)

library('segmented')
PCscores <- read.delim("~/Dropbox/Westneat Lab/MonotaxisPCscores.txt")
plot(PCscores$SL, PCscores$PC1, pch=19)
plot(log(PCscores$SL), PCscores$PC1, pch=19)

fit1 <- lm(PCscores$PC1~PCscores$SL)
summary(fit1)
fit2 <- lm(PCscores$PC1~log(PCscores$SL))
summary(fit2)
anova(fit1, fit2)

# fit2 is better

fit3 <- lm(PCscores$PC2~PCscores$SL)
fit4 <- lm(PCscores$PC2~log(PCscores$SL))
summary(fit3)
summary(fit4)

SL <- PCscores$SL
PC2 <- PCscores$PC2
lin.mod <- lm(PC2~SL)
segmented.mod <- segmented.lm(lin.mod, seg.Z=~SL, psi=c(50, 150))
# segmented.mod <- segmented.lm(lin.mod, seg.Z=~SL, psi=c(80))
plot(SL, PC2, pch=19, col='blue')
plot(segmented.mod, lty=2, add=T)
summary(segmented.mod)

fitprelim <- lm(CV2~SL)
fitsegment <- segmented(fitprelim, seg.Z=~SL, psi=c(80, 170))
plot(SL, CV2, pch=20, col='red')
plot(fitsegment, lty=2, add=T)


prelimfitA1 <- lm(A1~SLmuscle)
plot(SLmuscle, A1, pch=19, col='blue')
curve(6.0874*x - 540.5814, add=TRUE) 
segmentfitA1 <- segmented(prelimfitA1, seg.Z=~SLmuscle, psi=c(50, 150))
plot(SLmuscle, A1, pch=19, col='blue')
plot(segmentfitA1, lty=2, add=T)



BFQ <- read.csv("~/Dropbox/Westneat Lab/Lethrinids/Data/BFQ.csv", na.strings="NaN")
BFQ <- rbind(BFQ[1:12,], BFQ[14:15,])
ABF <- BFQ$Anterior.BF..N.
BFQMass <- BFQ$Mass..g.
plot(BFQMass, ABF)
plot(log10(BFQMass), log10(ABF))
BFQfit <- lm(log10(ABF)~log10(BFQMass))
abline(BFQfit, lty=2)
residuals <- BFQfit$residuals
mean(residuals)
scalefactor <- 100/mean(residuals)
logBFQs <- residuals*scalefactor

plot(BFQMass, ABF)
BFQfit2 <- lm(ABF~BFQMass)
abline(BFQfit2, lty=2)
residuals <- BFQfit2$residuals
BFQresids <- residuals+100 #sets 100 as the "average" (before it was 0)
# if you ask me that's really stupid but...it's what other papers do
# but smh honestly. why not leave it as zero

WL.monotaxis <- read.csv("~/Dropbox/Westneat Lab/Lethrinids/Data/WL monotaxis.csv")
plot(WL.monotaxis$Length..cm., WL.monotaxis$Mass..g., pch=19)
plot(log(WL.monotaxis$Length..cm.), log(WL.monotaxis$Mass..g.), pch=19)
WLmass <- log(WL.monotaxis$Mass..g.)
WLlength <- log(WL.monotaxis$Length..cm.)
WLlogfit <- lm(WLmass~WLlength)
abline(WLlogfit, lty=2)
newdata <-data.frame(log(SLmuscle))

WL_predicts <- predict.lm(WLlogfit, newdata)
points(log(SLmuscle), WL_predicts, col='red')
transformed_WLpredicts <- exp(WL_predicts)

# oh fuck the predict function honestly
WL_predicts <- exp(WL_predicts)
plot(BFQMass, ABF, pch=19)
abline(BFQfit2, lty=2)
points(WL_predicts, CBF2, pch=18, col='blue')
