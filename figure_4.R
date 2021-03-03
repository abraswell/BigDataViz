# Code to create Figure 4 in Kelleher and Braswell 2021
#Code by Christa Kelleher, 3/2/2021

# load libraries
library(tidyverse)
library(ggplot2)
library(hexbin)
library(dplyr)
library(gridExtra)

# Data originally gathered from:
# https://pubs.er.usgs.gov/publication/70046617
# https://water.usgs.gov/GIS/metadata/usgswrd/XML/gagesII_Sept2011.xml

# load Data
gages2data<- read_csv("GAGESII_DA_LENGTH.csv")
gages2data$STRAHLER_MAX <- as.factor(gages2data$STRAHLER_MAX)

#plot data
ggplot(gages2data, aes(x=DRAIN_SQKM, y=FLOWYRS_1900_2009)) + geom_point(size=1,shape = 1) + scale_x_continuous(trans='log10') + labs(x=expression ("Drainage Area "~km^2), y="Record Length (yrs)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

ggplot(gages2data,aes(x=log10(DRAIN_SQKM),y=FLOWYRS_1900_2009)) + geom_hex(bins = 25) + 
  scale_fill_continuous(type = "viridis") + labs(x=expression ("Log-transformed Drainage Area "~km^2), y="Record Length (yrs)")+ theme(legend.position="none")+
  theme_bw() + theme(legend.position="none") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

ggplot(gages2data, aes(x=DRAIN_SQKM, y=FLOWYRS_1900_2009)) + geom_point(size=0.5,alpha = 0.2) + scale_x_continuous(trans='log10') + labs(x=expression ("Drainage Area "~km^2), y="Record Length (yrs)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Maximum plotting
# loop to find maximum 
y <- seq(0,6,0.1)
y <- 10^y

damax <- 0
yrmax <- 0
count <- 0
for (i in 1:(length(y)-1)) {
  count = count + 1
  z1 <- gages2data$DRAIN_SQKM[between(gages2data$DRAIN_SQKM, y[i], y[i+1])]
  z2 <- gages2data$FLOWYRS_1900_2009[between(gages2data$DRAIN_SQKM, y[i], y[i+1])]
  z2[is.na(z2)] <- 0
  z1[sort(order(z2)[z1])]
  
  damax[count] <- z1[1]
  yrmax[count] <- max(z2)
}

df1 <- data.frame("da" = damax, "yrmax" = yrmax)

ggplot(aes(x=DRAIN_SQKM, y=FLOWYRS_1900_2009), data=gages2data) + geom_point(size=0.5,color="gray",alpha = 1/2) + 
  geom_point(aes(x=damax, y=yrmax), data=df1) + scale_x_continuous(trans='log10') + labs(x=expression ("Drainage Area "~km^2), y="Record Length (yrs)") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


