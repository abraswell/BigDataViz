# Code to create Color Figure in Kelleher and Braswell 2021
#Code by Anna Braswell, 3/2/2021
#bathymetry from NOAA DEM dataset (CIRES, 2014)

# load libraries
library(sf)
library(raster)
library(dplyr)
library(spData)
library(rgdal)
library(tmap)    # for static and interactive maps
library(tmaptools)


#read state boundaries
#states <- readOGR("data/State_Boundaries.shp")

#read bathymetry raster
bathy <- raster("data/NOAA_SLR_DEM_MA_Cape_clip.tif")

#Create extent of Bathy map
bathy_region = st_bbox(c(xmin = -70.03, xmax = -69.93,
                      ymin = 41.53, ymax = 41.68),
                    crs = st_crs(bathy)) %>% 
  st_as_sfc()

#Create tmap object with the bathmetry - Color choices 1
map_bathy1 = tm_shape(bathy, bbox=bathy_region) + 
  tm_raster(midpoint = NA, 
            style = "fixed", breaks = c(-5, 0, 1, 5, 10, 15, 40), 
            palette = colorRampPalette( c("#2166ac","#67a9cf", "#d1e5f0", "#f7f7f7", "#fddbc7", "#ef8a62", "#b2182b"))(6), 
            title="Meters above Sea Level")+
  tm_layout(legend.position = c("right", "bottom"), legend.bg.color = "white", bg.color = "#2166ac")
#BACKGROUND WATER

#Create tmap object with the bathmetry - Color choices 2
map_bathy2 = tm_shape(bathy, bbox=bathy_region) + 
  tm_raster(midpoint = NA, 
            style = "fixed", breaks = c(-5, 0, 1, 5, 10, 15, 40), 
            palette = colorRampPalette( c("red", "magenta", "navy", "cyan1", "green", "yellow", "orange"))(6), 
            title="Meters above Sea Level")+
  tm_layout(legend.position = c("right", "bottom"), legend.bg.color = "white", bg.color = "red")

#Create tmap object with the bathmetry - Color choices 3
map_bathy3 = tm_shape(bathy, bbox=bathy_region) + 
  tm_raster(midpoint = NA, 
            style = "fixed", breaks = c(-5, 0, 1, 5, 10, 15, 40), 
            palette = colorRampPalette( c("darkblue","dodgerblue", "lightblue", "white"))(6), 
            title="Meters above Sea Level")+
  tm_layout(legend.position = c("right", "bottom"), legend.bg.color = "white", bg.color = "navy")



#To create multiple frames
map_grid = tmap_arrange(map_bathy2, map_bathy1, map_bathy3)

# Save Figure File
tmap_save(tm = map_grid, filename = "figures/Color_Fig/Bathy_Grid_7.13.20.pdf", width=10, height=8, units = "in" )
tmap_save(tm = map_grid, filename = "figures/Color_Fig/Bathy_Grid_7.13.20.jpg", width=10, height=8, units = "in" )

