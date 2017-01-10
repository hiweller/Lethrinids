setwd("C:/Users/Hannah/Desktop/Documents/Westneat Lab/data")
Mgteeth=read.csv('Mg teeth.csv')

C1=Mgteeth$C1
C2=Mgteeth$C2
C3=Mgteeth$C3
C4=Mgteeth$C4
C5=Mgteeth$C5
M1=Mgteeth$M1
M2=Mgteeth$M2
M3=Mgteeth$M3
M4=Mgteeth$M4
SL=Mgteeth$SL

par(mfrow=c(3,3), mar=c(3,3,1.5,1), mgp=c(1.5, 0.5, 0))

plot(SL, C1, pch=19, col="darkgreen")
abline(lm(C1~SL), lty=2)

plot(SL, C2, pch=17, col="darkblue")
abline(lm(C2~SL), lty=2)

plot(SL, C3, pch=18, col="red")
abline(lm(C3~SL), lty=2)

plot(SL, C4, pch=19, col="purple")
abline(lm(C4~SL), lty=2)

plot(SL, C5, pch=17, col="darkgrey")
abline(lm(C5~SL), lty=2)

plot(SL, M1, pch=18, col="violet")
abline(lm(M1~SL), lty=2)

plot(SL, M2, pch=19, col="grey")
abline(lm(M2~SL), lty=2)

plot(SL, M3, pch=17, col="brown")
abline(lm(M3~SL), lty=2)

plot(SL, M4, pch=18, col="yellow")
abline(lm(M4~SL), lty=2)


dentition=cbind(C1, C2, C3, C4, C5, M1, M2, M3, M4)
conical=cbind(C1, C2, C3, C4, C5)
molar=cbind(M1, M2, M3, M4)

matplot(SL, dentition, type=c("b"), pch=19, col=1:8)
legend("topleft", legend=c("C1", "C2", "C3", "C4", "C5",
                           "M1", "M2", "M3", "M4"), col=1:8)

matplot(SL, conical, type=c("b"), pch=19, col=1:5)
legend("topleft", legend=c("C1", "C2", "C3", "C4", "C5"), col=1:5)
matplot(SL, molar, type=c("b"), pch=19, col=1:4)