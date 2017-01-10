setwd('/Users/hannah/Dropbox/Westneat_Lab/Lethrinids/Data/')
library(StereoMorph)
library(geomorph)
library(ggplot2)

getProcrustes <- function(DF, classifierCount) {
  coords <- DF[,(classifierCount+1):dim(DF)[2]]
  nPoints <- dim(coords)[2]/2
  procrustesFit <- gpagen(arrayspecs(coords, nPoints, 2))
  return(procrustesFit)
}
getPCA <- function(DF, classifierCount) {
  # coords <- DF[,(classifierCount+1):dim(DF)[2]]
  # nPoints <- dim(coords)[2]/2
  procrustesFit <- getProcrustes(DF, classifierCount)
  PCA <- plotTangentSpace(procrustesFit$coords)
  return(PCA)
}
getPCAdf <- function(DF, classifierCount) {
  classifiers <- DF[,1:classifierCount]
  PCA <- getPCA(DF, classifierCount)
  return(as.data.frame(cbind(classifiers, PCA$pc.scores)))
}
plotBy <- function(DF, classifierCount, n) {
  # classifiers <- DF[,1:classifierCount]
  newDF <- getPCAdf(DF, classifierCount)
  p <- ggplot(data=newDF, aes(x=PC1, y=PC2))
  p <- p + geom_point(data=newDF, aes(col=newDF[,n]))
  return(p)
}
BFQ <- function(bodySize, maxABF) {
  fit <- lm(log10(maxABF)~log10(bodySize))
  predicted <- 10^(fit$fitted.values)
  BFQ <- (maxABF/predicted)*100
  
  return(list(BFQ=BFQ, fit=fit))
}