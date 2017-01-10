source('/Users/hannah/Dropbox/Westneat_Lab/Lethrinids/Code/sourceMe.R')

# W-L relationships
# taking W-L from Kulbicki et al. 2005 (http://sfi.mnhn.fr/cybium/numeros/2005/293/03-Kulbicki%20282.pdf)
# W = aL^b
# W = 0.0230(L)^3.022, n = 63, range = 4.0-45.0cm
muscles <- read.csv('./Spreadsheets/Monotaxis_muscle_masses.csv')
SL <- muscles$SL..mm.[is.na(muscles$X)==FALSE]/10
weights <- vector()

measuredWL <- read.csv('./Spreadsheets/Monotaxis_WL_measured.csv')
a <- 0.0230
b <- 3.022
for (i in 1:length(measuredWL$Length)) {
  weights[i] <- 0.0230*(measuredWL$Length[i]^b)
}

fit <- lm(log(measuredWL$Weight)~log(measuredWL$Length))
plot(measuredWL$Length, measuredWL$Weight)
points(x=measuredWL$Length, y=weights, pch=21, col="tomato")
WLfit <- lm(weights~measuredWL$Weight) # how well are values predicted by fit matched to values not used to make the fit?
plot(log(WLfit$fitted.values), WLfit$residuals) # as expected - heteroskedasticity (larger residuals for larger sizes, since you'd expect more variance there anyways)
abline(h=0)

# plotting bite force per size
muscles <- read.csv('./Spreadsheets/Monotaxis_muscle_masses.csv')
SL <- muscles$SL..mm.[is.na(muscles$X)==FALSE]
AM <- muscles$X[is.na(muscles$X)==FALSE]/1000
muscleDF <- data.frame(SL=SL, AM=AM)

BFfit <- lm(data=muscleDF, log(AM)~log(SL))
muscleDF$predicted <- exp(BFfit$fitted.values)

m <- ggplot(data=muscleDF, aes(x=SL, y=AM))
m <- m + geom_point(data=muscleDF, aes(x=SL, y=AM), shape=21, stroke = 0.5, fill="#c02fe4", size=4)
m <- m + xlab("Standard length (mm)") + ylab("(not actually) Bite force (N)")
m <- m + ggtitle("TEMPORARY GRAPH", subtitle = "Subbing in total adductor mandibulae mass (g)")
m <- m + theme(axis.title=element_text(size=15), axis.text = element_text(size=10))
m <- m + geom_line(aes(x=SL, y=predicted), linetype=2)
m

ggsave(filename = '../Manuscript/Figures/Fig4.tiff', plot = m, dpi = 600)


# read in dataframes (and remove archosargus outgroup)
lethrinids <- read.csv('./PCA/Lethrinids_all_40pts.csv')
lethrinids <- lethrinids[(lethrinids$Genus=="Archosargus")==FALSE,]
monotaxis <- lethrinids[lethrinids$Genus=="Monotaxis",]
t <- 6
names(lethrinids)[1:t]

# plot by SL:
plotBy(monotaxis, 4)

# lethrinids plot by genus:
plotBy(lethrinids, t, 2)
plotBy(lethrinids, t, 6)

# adults (?) only
sizeThresh <- 120
lethAdults <- lethrinids[lethrinids$SL >= sizeThresh,]
plotBy(lethAdults, t, 2) # genus
plotBy(lethAdults, t, 3) # species
plotby(lethAdults, t, 4) # SL

# include only 3 monotaxis?
# very artificially picked
lethLimit <- lethrinids[c(1:17, 23, 28, 31, 46:63),]
plotBy(lethLimit, t, 2)
monotaxisSpecIDs <- monotaxis$V1
monotaxisClassifiers <- monotaxis[,1:t]

monotaxisProcrustes <- gpagen(arrayspecs(monotaxis[,7:86], 40, 2))
monotaxisPCA <- plotTangentSpace(monotaxisProcrustes$coords)
monotaxisPCscores <- as.data.frame(monotaxisPCA$pc.scores)
df <- cbind(monotaxisClassifiers, monotaxisPCscores)


# plot PC1 vs PC2, color by SL
p <- ggplot(data = df, aes(x=PC1, y=PC2))
p <- p + geom_point(data=df, size=4, aes(col=SL))
# p <- p + scale_color_gradient(low = "#e9baff", high = "#6b0086") # purple
# p <- p + scale_color_gradient(low = "#ffd5be", high = "#d64000") # orange
p <- p + scale_color_gradient(low = "#b6f1aa", high = "#004e00") # green
p


# all species (monotaxis, lethrinus/gymnocranius, archosargus)
lethrinids <- read.delim('./PCA/Lethrinids_all_40pts.csv', sep = ",", header=TRUE)
lethrinidClassifiers <- lethrinids[,1:t]
lethrinidProcrustes <- gpagen(arrayspecs(lethrinids[,7:dim(lethrinids)[2]], 40, 2))
lethrinidPCA <- plotTangentSpace(lethrinidProcrustes$coords)
lethrinidDF <- as.data.frame(cbind(lethrinidClassifiers, lethrinidPCA$pc.scores))

# color by genus?
p <- ggplot(data=lethrinidDF, aes(x=PC1, y=PC2))
p <- p + geom_point(data=lethrinidDF, aes(col=Genus))
p

# only adults?
sizeThresh <- 100
lethAdults <- lethrinids[lethrinids$SL >= sizeThresh,]

# takes dataframe and number of columns at the start with classifiers (so c+1 = first coordinate)


# classifiers?
classifiers <- read.csv('./PCA/CLASSIFIERS_Lethrinids_01-04-2017.csv')
# monotaxis classifiers
monotaxisClassifiers <- classifiers[classifiers$Species=="Monotaxis grandoculis",]

# relationship between shape and length?
procD.lm(monotaxis[,7:86]~monotaxisClassifiers$SL)
# I am a big enough person to admit that I don't know what this means

# plotting in ggplot:
# matrix of PC scores, size, specID