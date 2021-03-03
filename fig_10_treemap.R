# Code to create Figure 10_treemap in Kelleher and Braswell 2021
#Code by Christa Kelleher, 3/2/2021


# form for this visualization was heavily drawn from:
# https://stackoverflow.com/questions/50163072/different-colors-with-gradient-for-subgroups-on-a-treemap-ggplot2-r

# Data originally gathered from:
# https://www.mrlc.gov/data?f%5B0%5D=region%3Aalaska (Change, 2001 to 2011)

# load libraries
library(circlize)
library(readr)
library(tidyverse)
library(patchwork)
library(treemap)
library(ggplot2)
library(treemapify)

aklcchange <- read_csv("aklcchange.csv")

# Find all land cover types
origclass <- unique(aklcchange$`2011_Code`)
ord <- c("WAT","ICSN", "ODEV", "LDEV", "MDEV", "HDEV","BAR", "DECF", "EVERF", "MIXF", "DS", "SS", "GRAS", "SEDGE","MOSS", "PAST", "CROP", "WOODW","HERBW")

# Make a table that shows from (2001 - rows) and to (2011 - columns)
lctable <- matrix(data = NA, nrow = length(origclass), ncol = length(origclass))
for (i in 1:length(origclass)){
  z <- which(aklcchange$`2011_Code` == origclass[i])
  
  lcout <-data.frame("count" = aklcchange$Count[z], "orig" = aklcchange$'2001_Code'[z])
  lcout <- lcout[order(lcout$orig),]
  lctable[,i] <- lcout$count
}
colnames(lctable) <- c("11","12", "21", "22", "23", "24","31", "41", "42", "43", "51", "52", "71", "72","74", "81", "82", "90","92")
rownames(lctable) <- colnames(lctable)
lctab <- data.frame(lctable)

nlcdcolor <- read_csv("nlcdcolor.csv", col_names = FALSE)
colorpal <- data.frame("r" = nlcdcolor$X2,"g" = nlcdcolor$X3, "b" = nlcdcolor$X4)
colorpal$r <- as.character(colorpal$r)
colorpal$g <- as.character(colorpal$g)
colorpal$b <- as.character(colorpal$b)

n <- length(ord)
df2 <- cbind(as.data.frame(lctab),"LC2001"=rownames(lctab),  stringsAsFactors=FALSE)
df2$color <- rgb(nlcdcolor$X2,nlcdcolor$X3,nlcdcolor$X4, max = 255)
df2 <- reshape(df2, idvar="LC2001code", varying=list(1:n), direction="long",timevar="LC2011", time=rownames(lctab),  v.names = "lctab")

#Plot figure
ggplot(df2, aes(area = lctab, fill = color, label=LC2001, subgroup=LC2011)) +
  geom_treemap() +  scale_fill_identity() +
  geom_treemap_subgroup_border(colour="white") +
  geom_treemap_text(place = "centre",
                    grow = T,
                    alpha = 0.5,
                    colour = "#FAFAFA",
                    min.size = 0)
