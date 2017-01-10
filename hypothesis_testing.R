source('/Users/hannah/Dropbox/Westneat_Lab/Lethrinids/Code/sourceMe.R')

# read in dataframes (and remove archosargus outgroup)
t <- 9 # number of traits
sizeThresh <- 130
lethrinids <- read.csv('./PCA/Lethrinids_all_40pts.csv')
lethrinids <- lethrinids[(lethrinids$Genus=="Archosargus")==FALSE,]
monotaxis <- lethrinids[lethrinids$Genus=="Monotaxis",]
lethAdults <- lethrinids[lethrinids$SL >= sizeThresh,]
names(lethrinids)[1:t]

# get PCA
monotaxisPCA <- getPCA(monotaxis, t)
monotaxis.gpa <- getProcrustes(monotaxis, t)

# these are exactly equivalent (just that PC data doesn't represent exact coordinates)
# size of course is a significant predictor
procD.lm(two.d.array(monotaxis.gpa$coords)~monotaxis$SL)
procD.lm(monotaxisPCA$pc.scores~monotaxis$SL)
procD.lm(monotaxisPCA$pc.scores~monotaxis$SL_50mmInterval)

# test whether genus is significant predictor when you include juvenile monotaxis
# would assume that including juveniles might artificially weight PC results, making monotaxis look more different than it really is, so genus would be a less significant predictor when you take them out because variation is due to size and not genus
# control for size + genus?
lethrinidPCA <- getPCA(lethrinids, t)
lethrinidProcrustes <- two.d.array(getProcrustes(lethrinids, t)$coords) 
lethAdultPCA <- getPCA(lethAdults, t)
lethAdultProcrustes <- two.d.array(getProcrustes(lethAdults, t)$coords)

# with juveniles:
procD.lm(lethrinidProcrustes~lethrinids$SL)
procD.lm(lethrinidProcrustes~lethrinids$Genus)
lmAll <- procD.lm(lethrinidProcrustes~lethrinids$SL*lethrinids$Genus)

# without juveniles:
lmAdults <- procD.lm(lethAdultProcrustes~lethAdults$SL*lethAdults$Genus)

# SL is less significant (as expected) but genus is equivalent as a predictor even when controlling for size variable - accounts for variation regardless

# try regressing just PC1...
summary(lm((lethrinids$SL~lethrinidPCA$pc.scores[,2]+lethrinidPCA$pc.scores[,1])))
summary(lm((lethAdults$SL~lethAdultPCA$pc.scores[,2]+lethAdultPCA$pc.scores[,1])))

# for PC2 SL is significant (so is genus) for both
summary(lm((lethrinidPCA$pc.scores[,2]~lethrinids$Genus+lethrinids$SL)))
summary(lm((lethAdultPCA$pc.scores[,2]~lethAdults$Genus+lethAdults$SL)))

# SL is no longer significant if we remove juveniles, but genus still is
summary(lm((lethrinidPCA$pc.scores[,1]~lethrinids$Genus+lethrinids$SL)))
summary(lm((lethAdultPCA$pc.scores[,1]~lethAdults$Genus+lethAdults$SL)))
