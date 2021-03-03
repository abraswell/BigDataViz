# Code to create Figure 6 in Kelleher and Braswell 2021
#Code by Christa Kelleher, 3/2/2021

# load libraries
library(dataRetrieval)
library(lubridate)
library(dplyr)
library(data.table)
library(xts)
library(readr)

#choose sites
Site_info <- c(5200510, 5211000, 5267000, 5331000, 5344500, 5378500, 5420500, 7010000, 7020500, 7022000, 7032000, 7374000, 7374525, 7289000)
nr_sites=length(Site_info)

#determine period of time 
start_of_period <- as.Date("2008-10-01")
end_of_period <- as.Date("2018-09-30")  

sitelat <- 0
sitelon <- 0
siteda <- 0
da <- 0
inq <- 0

for (i in seq(1,nr_sites)) {
  #read an entire record of daily data
  
  site_numb <- as.character(Site_info[i])
  
  #Check if site number is approprate length
  if (nchar(site_numb)<9) {
    site_numb=paste0("0", site_numb)
  }
  
  # Download Site Info
  siteINFO <- readNWISsite(site_numb)
  sitelat[i] <- siteINFO$dec_lat_va
  sitelon[i] <- siteINFO$dec_long_va
  siteda[i] <- siteINFO$drain_area_va
  
  # Convert DA from mi2 to ft2
  da <- siteINFO$drain_area_va *5280 *5280
  
  # Download mean streamflow
  discharge_data <- readNWISdv(siteNumber=site_numb, parameterCd="00060", startDate= start_of_period, endDate= end_of_period,  statCd <- c("00003"))
  
  #Pulling values needed from discharge_data variable
  site_nr <- as.numeric(discharge_data$site_no) #site number
  dates <- discharge_data$Date#dates corresponding to streamflow values
  meanq <- discharge_data$X_00060_00003 #flow values
  meanqmmday <- (3600*24*10*2.54*12)*meanq/da
  
  if (i == 1) {
    df.q <- cbind(site_nr,dates,meanq,meanqmmday)
    
  } else {
    inq <- cbind(site_nr,dates,meanq,meanqmmday) 
    df.q <- rbind(df.q,inq)
  }
}

#load libraries for plotting
library(ggplot2)
library(gridExtra)
library(viridis)

#plot data
all.q = data.frame(site_nr = df.q[,1], mmhr = df.q[,3], logmmhr = log10(df.q[,3]))
all.q$site_nr <- as.factor(all.q$site_nr)

p1 <- ggplot(all.q, aes(x=site_nr, y=logmmhr)) + geom_violin() + labs(x = "USGS Gage Number", y=expression ("Streamflow mm "~hr^-1 ))

p2 <- ggplot(all.q, aes(x=site_nr, y=logmmhr)) + geom_boxplot() + labs(x = "USGS Gage Number", y=expression ("Streamflow mm "~hr^-1 ))

sample_size = all.q %>% group_by(site_nr) %>% summarize(num=n())

# Plot
p3 <- all.q %>%
  left_join(sample_size) %>%
  mutate(myaxis = site_nr) %>%
  ggplot( aes(x=myaxis, y=logmmhr, fill=site_nr)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  labs(x = "USGS Gage Number", y=expression ("Streamflow mm "~hr^-1 ))

p4 <- all.q %>%
  left_join(sample_size) %>%
  mutate(myaxis = site_nr) %>%
  ggplot( aes(x=myaxis, y=logmmhr)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  labs(x = "USGS Gage Number", y=expression ("Streamflow mm "~hr^-1 )) + theme_classic()

#turn into multi-panel
grid.arrange(p2, p1, p3, p4, nrow = 4)

