source('/Users/hannah/Dropbox/Westneat_Lab/Lethrinids/Code/sourceMe.R')

df <- read.csv('./Spreadsheets/BFQ.csv')
df <- df[df$Extant==TRUE,]

getBFQ <- BFQ(bodySize = df$Body.mass..g., maxABF = df$Max.ABF..N.)
df$BFQ <- getBFQ$BFQ
df$Residuals <- getBFQ$fit$residuals
df$logMass <- log10(df$Body.mass..g.)

q <- ggplot(data=df, aes(x=logMass, y=Residuals))
q <- q + geom_point(data=df, aes(color=Taxon))
q


b <- ggplot(data=df, aes(x=Species, y=BFQ))
b <- b + geom_bar(stat="identity", position="dodge")
b <- b + coord_flip()
b
