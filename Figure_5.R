# Code to create Spatial Figure in Kelleher and Braswell 2021
#Code by Anna Braswell, 3/2/2021
#Dams data from GAGES II dam location dataset (Falcone 2010, 2011)


# load libraries
library(sf)
library(raster)
library(tidyverse)
library(spData)
library(rgdal)
library(tmap)    
library(tmaptools)
library(maptools)


#read state boundaries
states <- readOGR("data/State_Boundaries.shp")

#read dam data
dam_data = read.csv("data/GAGESII_Locs_Dams.csv") 
dam_data = na.omit(dam_data)

#create dams spatial opject through using the lat and long
dam_sf <- st_as_sf(dam_data, coords = c("LNG_GAGE", "LAT_GAGE"), crs = 4269)

#set the region of interest for the map
dam_region = st_bbox(c(xmin = -124.393, xmax = -67.5,
                         ymin = 24.5, ymax = 48.97082),
                       crs = st_crs(dam_sf)) %>% 
  st_as_sfc()

#Panel 1 of figure
map_Dams1 = #tm_shape(states) + tm_polygons(border.col = "black")
  tm_shape(dam_sf, bbox=dam_region) + tm_dots(size=.1, col="black")+ tm_add_legend(type= "symbol", size = .1 , title = "Record Location") +
  tm_layout(legend.position = c("right", "bottom"), legend.bg.color = "white", bg.color = "white")+  tm_layout(legend.outside = TRUE) 

#Panel 2 of figure
map_Dams2 = #tm_shape(states) + tm_polygons(border.col = "black")
  tm_shape(dam_sf, bbox=dam_region) + tm_bubbles(size=.1, col="black", alpha = 0.1)+ tm_add_legend(type= "symbol", size = 2 , title = "Record Location") +
  tm_layout(legend.position = c("left", "bottom"), legend.bg.color = "white", bg.color = "white")+  tm_layout(legend.outside = TRUE) 

#Panel 3 of figure
map_Dams3 = #tm_shape(states) + tm_polygons(border.col = "black")
  tm_shape(dam_sf, bbox=dam_region) + tm_bubbles(size = "NDAMS_2009", col = "NDAMS_2009", title.size = "Number of Dams", title.col = "Number of Dams")+
  tm_layout(legend.position = c("left", "bottom"), legend.bg.color = "white", bg.color = "white")+  tm_layout(legend.outside = TRUE) 


#for Panel 4 of figure - determine kernel density
library(spatstat)
dam_data2 <- dam_data %>% 
  select(NDAMS_2009, LAT_GAGE, LNG_GAGE) 
dam2_sf <- st_as_sf(dam_data2, coords = c("LNG_GAGE", "LAT_GAGE"), crs = 4269)
st_write(dam2_sf, "data/dam_test.shp")
s <- readOGR("data/dam_test.shp")
dams_shp <- as(s, "ppp")
#figure out kernel density for heat map
K1 <- density.ppp(dams_shp, sigma = bw.ppl(dams_shp),edge=T, weights = dam2_sf$NDAMS_2009) # Using the default bandwidth
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)

r1 <- raster(K1)
crs(r1) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
map_Dams4 = tm_shape(r1, bbox=dam_region) + tm_raster(palette = colorRampPalette( c("#ffffcc","#fd8d3c", "#800026"))(7),  #"#edf8fb","#8c96c6", "#810f7c"
                                                      title = "# of Dams", breaks = c(0, 500, 1000, 2000, 3000, 4000, 5000, 6000 ))+
  tm_layout(legend.position = c("left", "bottom"), legend.bg.color = "white", bg.color = "#ffffcc")+  tm_layout(legend.outside = TRUE) 


#Make paneled figure
map_state = tm_shape(states, bbox=dam_region) + tm_borders("navy", lwd = .5)

mappie1 = map_state + map_Dams1
mappie2 = map_state + map_Dams2
mappie3 = map_state + map_Dams3
mappie4 = map_Dams4 + map_state

map_grid = tmap_arrange(mappie2, mappie3, mappie4)


#Save figure file
tmap_save(tm = map_grid, filename = "figures/Spatial_Fig/Dams_Version.8.25.20.png", width=8, height=10, units = "in" )
