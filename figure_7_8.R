# Code to create Figures 7 and 8 in Kelleher and Braswell 2021
#Code by Christa Kelleher, 3/2/2021

# load libraries
library(GGally)
library(ggplot2)
library(readr)
library(viridis)
library(tidyverse)
library(gridExtra)

# Data originally gathered from:
# https://ral.ucar.edu/solutions/products/camels

#load data
camels_hydro <- read_delim("camels_hydro.csv",";", escape_double = FALSE, trim_ws = TRUE)
camels_name <- read_delim("camels_name.csv",";", escape_double = FALSE, trim_ws = TRUE)
camels_clim <- read_delim("camels_clim.csv",";", escape_double = FALSE, trim_ws = TRUE)

camels_hydro$huc <- camels_name$huc_02
camels_hydro$precseas <- camels_clim$p_seasonality
camels_hydro$aridity <- camels_clim$aridity
camels_hydro$prec <- camels_clim$p_mean * 365/1000 # mm/day to m/yr

#create plots
p1a <- ggplot(camels_hydro, aes(x = runoff_ratio, y = slope_fdc)) + geom_point() + theme_classic() +
  xlab("Runoff Ratio") + ylab("FDC Slope")

p2a <- ggplot(camels_hydro, aes(x = runoff_ratio, y = slope_fdc, color = prec)) + geom_point() + 
  scale_color_viridis_c() + theme_classic() + guides(color = "none") + 
  xlab("Runoff Ratio") + ylab("FDC Slope") + labs(color = "Precipitation (m)\n")

grid.arrange(p1a, p2a, ncol=2)

camels_hydro1 <- arrange(camels_hydro,runoff_ratio)
camels_hydro1$order <- seq(1,671,1)
p1 <- ggplot(camels_hydro1, aes(y = order, x = runoff_ratio, color = huc)) + geom_point() +
  xlab("Runoff Ratio") + ylab("") + theme_classic() + guides(color = "none") + 
  theme(axis.text.y=element_blank())

camels_hydro2 <- arrange(camels_hydro,slope_fdc)
camels_hydro2$order <- seq(1,671,1)
p2 <- ggplot(camels_hydro2, aes(x = slope_fdc, y = order, color = huc)) + geom_point() +
  xlab("FDC Slope") + ylab("") + theme_classic() + guides(color = "none") + 
  theme(axis.text.y=element_blank())

grid.arrange(p1, p2, ncol=2)

ggparcoord(camels_hydro,columns = 2:7, groupColumn = 18, scale="uniminmax", alphaLines = 0.3) + 
  scale_color_viridis_c() + theme_classic() + 
  scale_x_discrete(labels=c("Qmean","RR","SFDC","BFI","EQ","Q5")) + 
  ylab("Normalized Signature Value") + labs(color = "Precipitation (m)\n")

camels_hydro %>%
  arrange((huc)) %>%ggparcoord(camels_hydro,columns = 2:7, scale="uniminmax",groupColumn = 15, alphaLines = 0.3) +
  scale_color_manual(values=c("#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#B7B7B7", "#49A3EA","#B7B7B7") ) +
  scale_x_discrete(labels=c("Qmean","RR","SFDC","BFI","EQ","Q5")) + 
  theme_classic() + xlab("") + ylab("Normalized Signature Value") + guides(color = "none")

ggparcoord(camels_hydro,columns = 2:14, groupColumn = 18, scale="uniminmax", alphaLines = 0.3) + 
  scale_color_viridis_c() + theme_classic() + 
  scale_x_discrete(labels=c("Qmean","RR","SFDC","BFI","EQ","Q5","Q95","HQF","HQD","LQF","LQD","ZeroQ","HFD")) +
  xlab("") + ylab("Normalized Signature Value") + labs(color = "Precipitation (m)\n")