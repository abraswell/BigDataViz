# Code to create Figure 9 in Kelleher and Braswell 2021
#Code by Christa Kelleher, 3/2/2021

# load libraries
library(ncdf4) 
library(raster) 
library(rgdal)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(viridis)

# Data originally gathered from:
# https://www.hydroshare.org/resource/474ecc37e7db45baa425cdb4fc1b61e1/

#load data
nc_data <- nc_open('01013500_benchmark_models.nc')
sacsma <- ncvar_get(nc_data, "SAC_SMA")
qobs <- ncvar_get(nc_data, "QObs")
hbvlb <- ncvar_get(nc_data, "HBV_lb")
hbvub <- ncvar_get(nc_data, "HBV_ub")
vic <- ncvar_get(nc_data, "VIC_basin")

dval <- seq(as.Date("1989-10-01"), as.Date("1999-09-30"), by="days")
site1$dval <- as.Date(site1$dval)
vals <- seq(1,3652)

site1 <- data.frame(vals,dval,qobs,sacsma,hbvlb,hbvub,vic)
site1$mo <- month(site1$dval)

qmaxall <- c(max(site1$qobs),max(site1$sacsma),max(site1$hbvlb),max(site1$hbvub),max(site1$vic))
qmaxall <- max(qmaxall)
qmaxall <- round(qmaxall)

# Single plot
ggplot(site1, aes(x=dval)) + 
  geom_line(aes(y = qobs,color = "Obs")) + 
  geom_line(aes(y = vic,color="VIC"), linetype="twodash") +
  geom_line(aes(y = hbvlb, color="HBV-LB"), linetype="twodash") +
  geom_line(aes(y = hbvub, color="HBV-UB"), linetype="twodash") +
  geom_line(aes(y = sacsma, color="SACSMA"), linetype="twodash") + 
  scale_colour_manual("",breaks = c("Obs", "VIC", "HBV-LB", "HBV-UB", "SACSMA"),
                      values = c("black", "darkslategray1", "darkseagreen2","darkseagreen","blue3")) +
  xlab("") + ylab("Streamflow (mm/day)") + theme_minimal()

# Single plot - log transformed
ggplot(site1, aes(x=dval)) + 
  geom_line(aes(y = qobs,color = "Obs")) + 
  geom_line(aes(y = vic,color="VIC"), linetype="twodash") +
  geom_line(aes(y = hbvlb, color="HBV-LB"), linetype="twodash") +
  geom_line(aes(y = hbvub, color="HBV-UB"), linetype="twodash") +
  geom_line(aes(y = sacsma, color="SACSMA"), linetype="twodash") + 
  scale_y_log10() +
  scale_colour_manual("",breaks = c("Obs", "VIC", "HBV-LB", "HBV-UB", "SACSMA"),
                      values = c("black", "darkslategray1", "darkseagreen2","darkseagreen","blue3")) +
  xlab("") + ylab("Streamflow (mm/day)") + theme_minimal()

# Linear transformed y-axis - subplots
p1 <- ggplot(site1, aes(x=dval)) + 
  geom_line(aes(y = qobs), color = "black") + 
  geom_line(aes(y = vic), color="darkslategray1", linetype="twodash") + 
  ylim(0, qmaxall) + theme_minimal() + xlab("") + ylab("Streamflow (mm/day)")
p2 <- ggplot(site1, aes(x=dval)) + 
  geom_line(aes(y = qobs), color = "black") + 
  geom_line(aes(y = hbvlb), color="darkseagreen2", linetype="twodash") + 
  ylim(0, qmaxall) + theme_minimal() + xlab("") + ylab("Streamflow (mm/day)")
p3 <- ggplot(site1, aes(x=dval)) + 
  geom_line(aes(y = qobs), color = "black") + 
  geom_line(aes(y = hbvlb), color="darkseagreen", linetype="twodash") +
  ylim(0, qmaxall) + theme_minimal() + xlab("") + ylab("Streamflow (mm/day)")
p4 <- ggplot(site1, aes(x=dval)) + 
  geom_line(aes(y = qobs), color = "black") + 
  geom_line(aes(y = sacsma), color="blue3", linetype="twodash") + 
  ylim(0, qmaxall) + theme_minimal() + xlab("") + ylab("Streamflow (mm/day)")

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

# Log transformed y-axis
p1 <- ggplot(site1, aes(x=dval)) + 
  geom_line(aes(y = qobs), color = "black") + 
  geom_line(aes(y = vic), color="darkslategray1", linetype="twodash") + 
  ylim(0, qmaxall) + theme_minimal() + xlab("") + scale_y_log10() + ylab("Streamflow (mm/day)")
p2 <- ggplot(site1, aes(x=dval)) + 
  geom_line(aes(y = qobs), color = "black") + 
  geom_line(aes(y = hbvlb), color="darkseagreen2", linetype="twodash") + 
  ylim(0, qmaxall) + theme_minimal() + xlab("") + scale_y_log10() + ylab("Streamflow (mm/day)")
p3 <- ggplot(site1, aes(x=dval)) + 
  geom_line(aes(y = qobs), color = "black") + 
  geom_line(aes(y = hbvlb), color="darkseagreen", linetype="twodash") +
  ylim(0, qmaxall) + theme_minimal() + xlab("") + scale_y_log10() + ylab("Streamflow (mm/day)")
p4 <- ggplot(site1, aes(x=dval)) + 
  geom_line(aes(y = qobs), color = "black") + 
  geom_line(aes(y = sacsma), color="blue3", linetype="twodash") + 
  ylim(0, qmaxall) + theme_minimal() + xlab("") + scale_y_log10() + ylab("Streamflow (mm/day)")

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

p1 <- ggplot(site1, aes(x=qobs,y=sacsma)) + geom_point(alpha=0.2) + 
  xlim(0, qmaxall) + ylim(0, qmaxall)
p2 <- ggplot(site1, aes(x=qobs,y=vic)) + geom_point(alpha=0.2) + 
  xlim(0, qmaxall) + ylim(0, qmaxall)
p3 <- ggplot(site1, aes(x=qobs,y=hbvlb)) + geom_point(alpha=0.2) + 
  xlim(0, qmaxall) + ylim(0, qmaxall)
p4 <- ggplot(site1, aes(x=qobs,y=sacsma)) + geom_point(alpha=0.2) + 
  xlim(0, qmaxall) + ylim(0, qmaxall)

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

p1 <- ggplot(site1, aes(x=qobs,y=sacsma,color=mo)) + geom_point(alpha=0.2) + 
  xlim(0, qmaxall) + ylim(0, qmaxall) + scale_color_gradient2(low="white", high = "blue") + 
  theme_minimal() + xlab("Observed Q") + ylab("Simulated Q (SAC SMA)")
p2 <- ggplot(site1, aes(x=qobs,y=vic,color=mo)) + geom_point(alpha=0.2) + 
  xlim(0, qmaxall) + ylim(0, qmaxall) + scale_color_gradient2(low="white", high = "blue") + 
  theme_minimal() + xlab("Observed Q") + ylab("Simulated Q (VIC)")
p3 <- ggplot(site1, aes(x=qobs,y=hbvlb,color=mo)) + geom_point(alpha=0.2) + 
  xlim(0, qmaxall) + ylim(0, qmaxall) + scale_color_gradient2(low="white", high = "blue") + 
  theme_minimal() + xlab("Observed Q") + ylab("Simulated Q (HBV-LB)")
p4 <- ggplot(site1, aes(x=qobs,y=hbvub,color=mo)) + geom_point(alpha=0.2) + 
  xlim(0, qmaxall) + ylim(0, qmaxall) + scale_color_gradient2(low="white", high = "blue") + 
  theme_minimal() + xlab("Observed Q") + ylab("Simulated Q (HBV-UB)")

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)




