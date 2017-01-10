setwd('/Users/hannah/Dropbox/Westneat_Lab/Lethrinids/Data/Phylogeny/')
library(rfishbase)

family <- "Lethrinidae"
lethrinids <- species_list(Family = family)

speciesDF <- data.frame(Species = lethrinids)

write.csv(x = speciesDF, file = paste(family, 'Species.csv', sep=""), row.names = FALSE)
