source('/Users/hannah/Dropbox/Westneat_Lab/Lethrinids/Code/sourceMe.R')

# read in dataframes (and remove archosargus outgroup)
t <- 9 # number of traits
sizeThresh <- 130
lethrinids <- read.csv('./PCA/Lethrinids_all_40pts.csv')
lethrinids <- lethrinids[(lethrinids$Genus=="Archosargus")==FALSE,]
monotaxis <- lethrinids[lethrinids$Genus=="Monotaxis",]
lethAdults <- lethrinids[lethrinids$SL >= sizeThresh,]
names(lethrinids)[1:t]

# monotaxis ontogeny
# plot PC1 vs PC2, color by SL
# mess with specs (axis ticks, legend, etc) later
monotaxisPCA <- getPCA(monotaxis, t) 
# PC1 = 56.9% variance, PC2 = 11.8% variance

monotaxisDF <- getPCAdf(monotaxis, t)
g <- ggplot(data=monotaxisDF, aes(x=PC1, y=PC2))
g <- g + geom_point(data=monotaxisDF, shape=21, stroke = 0.4, size=5, aes(fill=SL))
g <- g + scale_fill_gradient(low = "#f1b8ff", high = "#810191", name = "SL (mm)")
g <- g + xlab("PC1 (56.9% explained var.)")
g <- g + ylab("PC2 (11.8% explained var.)")
g <- g + coord_fixed(ratio=1.3)
g <- g + theme(axis.title=element_text(size=20), legend.title=element_text(size=12), legend.text = element_text(size=8))
g


# lethrinid plot; includes juvenile monotaxis
# lethrinid colors
# order: G. elongatus, G. griseus, L. erythropterus, L. harak, L. lentjan, L. mahsena, L. miniatus, L. nebulosus, L. reticulatus, L. rubrioperculatus, L. semicinctus, M. grandoculis
lethrinidDF <- getPCAdf(lethrinids, t)
lethColorPalette <- c("#ff5400", "#ffb000", "#00c6f6", "#013acc", "#008cff", "#9cdefa", "#01f1e3", "#005be4", "#50cc03", "#b0ffa3", "#008642", "#c02fe4")
l <- ggplot(data = lethrinidDF, aes(x=PC1, y=PC2))
l <- l + geom_point(data=lethrinidDF, shape=21, size=4.5, stroke=0.4, aes(fill=Species))
l <- l + scale_fill_manual(values=lethColorPalette)
l <- l + xlab("PC1 (37.1% explained var.)") + ylab("PC2 (27.2% explained var.)")
l <- l + coord_fixed(ratio=0.70)
l <- l + theme(axis.title=element_text(size=20), legend.title=element_text(size=12), legend.text = element_text(size=10, face="italic"))
l

# ggsave(filename = '../Manuscript/Figures/Fig2A.tiff', plot=g, dpi=600)
# ggsave(filename = '../Manuscript/Figures/Fig2B.tiff', plot=l, dpi=600)
