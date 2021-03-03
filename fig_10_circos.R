# Code to create Figure 10_Circos in Kelleher and Braswell 2021
#Code by Christa Kelleher, 3/2/2021

# form for this visualization was heavily drawn from:
# Yan Holz, R Graph Gallery
# https://www.r-graph-gallery.com/122-a-circular-plot-with-the-circlize-package.html

# Data originally gathered from:
# https://www.mrlc.gov/data?f%5B0%5D=region%3Aalaska (Change, 2001 to 2011)

# load libraries
library(circlize)
library(readr)
library(tidyverse)
library(viridis)
library(patchwork)

aklcchange <- read_csv("aklcchange.csv")

# Find all land cover types
origclass <- unique(aklcchange$`2011_Code`)
origclass <- sort(origclass)
ord <- c("WAT","ICSN", "ODEV", "LDEV", "MDEV", "HDEV","BAR", "DECF", "EVERF", "MIXF", "DS", "SS", "GRAS", "SEDGE","MOSS", "PAST", "CROP", "WOODW","HERBW")

# Make a table that shows from (2001 - rows) and to (2011 - columns)
lctable <- matrix(data = NA, nrow = length(origclass), ncol = length(origclass))
for (i in 1:length(origclass)){
  z <- which(aklcchange$`2011_Code` == origclass[i])
  
  lcout <-data.frame("count" = aklcchange$Count[z], "orig" = aklcchange$'2001_Code'[z])
  lcout <- lcout[order(lcout$orig),]
  lctable[,i] <- lcout$count
}
colnames(lctable) <- c("WAT","ICSN", "ODEV", "LDEV", "MDEV", "HDEV","BAR", "DECF", "EVERF", "MIXF", "DS", "SS", "GRAS", "SEDGE","MOSS", "PAST", "CROP", "WOODW","HERBW")
rownames(lctable) <- colnames(lctable)
lctab <- data.frame(lctable)

# Identify what land use shows no change
lcnochange <- 0
for (i in 1:dim(lctable)[1]){
  lcnochange[i] <- lctable[i,i]
}

# Read in NLCD rgb codes
nlcdcolor <- read_csv("nlcdcolor.csv", col_names = FALSE)

perc2001 <- rowSums(lctab)
perc2001 <- 100 * perc2001/sum(perc2001)

perc2011 <- colSums(lctab)
perc2011 <- 100 * perc2011/sum(perc2011)

df1 <- data.frame("xmin" = 0, "xmax" = rowSums(lctab) + colSums(lctab),"types" = ord)
df1$types <- factor(df1$types, levels = df1$types)
df1$rcol <- rgb(nlcdcolor$X2,nlcdcolor$X3,nlcdcolor$X4, max = 255)
df1$lcol <- rgb(nlcdcolor$X2,nlcdcolor$X3,nlcdcolor$X4, max = 255)
lctab <- m[levels(df1$types),levels(df1$types)]

par(mar = rep(0, 4))
circos.clear()
circos.par(cell.padding=c(0,0,0,0), track.margin=c(0,0.15), start.degree = 90, gap.degree = 2)

circos.initialize(factors = df1$types, xlim = cbind(df1$xmin,df1$xmax))

circos.trackPlotRegion(factors = df1$types, ylim = c(0,1), track.height = 0.1, panel.fun = function(x, y) {
                       name = get.cell.meta.data("sector.index")
                       i = get.cell.meta.data("sector.numeric.index")
                       xlim = get.cell.meta.data("xlim")
                       ylim = get.cell.meta.data("ylim")
                       circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2], 
                                   col = df1$rcol[i], border=df1$rcol[i])})

df1$sum1 <- colSums(lctab)
df1$sum2 <- 0
n <- length(ord)

df2 <- cbind(as.data.frame(lctab),"LC2001"=rownames(lctab),  stringsAsFactors=FALSE)
df2 <- reshape(df2, idvar="LC2001code", varying=list(1:n), direction="long",timevar="LC2011", time=rownames(lctab),  v.names = "lctab")
df2 <- arrange(df2,desc(lctab))

### Keep only the largest flows to avoid clutter
df2 <- subset(df2, lctab > quantile(lctab,0.8))

for(k in 1:nrow(df2)){
  if (df2$LC2001[k] == df2$LC2011[k]){
  } else {
  #i,j reference of flow matrix
  i<-match(df2$LC2001[k],df1$types)
  j<-match(df2$LC2011[k],df1$types)
  
  circos.link(sector.index1=df1$types[i], point1=c(df1$sum1[i], df1$sum1[i] + abs(lctab[i, j])),
              sector.index2=df1$types[j], point2=c(df1$sum2[j], df1$sum2[j] + abs(lctab[i, j])),
              col = df1$rcol[i],directional = 1,arr.width = 0.1)
  
  df1$sum1[i] = df1$sum1[i] + abs(lctable[i, j])
  df1$sum2[j] = df1$sum2[j] + abs(lctable[i, j])
  }
}