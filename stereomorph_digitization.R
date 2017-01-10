setwd('/Users/hannah/Dropbox/Westneat_Lab/Lethrinids/Data/PCA/')
library('geomorph')
MgCoords <- read.csv('../Old/MorphoJ digitizations/Dataset txt files/Monotaxis_take_04.txt', header=FALSE)[,2:dim(MgCoords)[2]]
A <- arrayspecs(MgCoords, 40, 2)
procrustes <- gpagen(A)
ref <- mshape(procrustes$coords)
plotTangentSpace(procrustes$coords)

test <- prcomp(as.matrix(MgCoords[,2:dim(MgCoords)[2]]), scale.=TRUE, center=TRUE)
test <- cov(as.matrix(MgCoords[,2:dim(MgCoords)[2]]))
test <- prcomp(test)
plot(-test$x[,1], test$x[,2])
PCA <- read.delim('PC scores, CovMatrix, Monotaxis_12-16-2016, Procrustes coordinates.txt', sep="\t")
plot(PCA$PC1, PCA$PC2)
