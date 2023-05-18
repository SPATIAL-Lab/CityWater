library(raster); library(viridis);library(ggplot2); library(dplyr); 
library(tidyr); library(sf); library(terra)

# setup, skip now
#library(censusapi);library(elevatr);library(tigris, options(tigris_use_cache = TRUE)) 
#conus <- counties(cb = TRUE)
#conus$STATEFP <- as.numeric(conus$STATEFP)
#conus <- subset(conus, STATEFP < 60)
#conus <- subset(conus, STATEFP != 02)
#conus <- subset(conus, STATEFP != 15)
#elevation
#elevation <- get_elev_raster(conus, z = 5)
#writeRaster(elevation, "data/elevationRaster.tif", overwrite = T)
# e <- rasterToPolygons(elevation) # Four hours and counting...


# Elevation min max range -------------------------------------------------

elevation <- raster("data/elevationRaster.tif")
e <- rasterToPoints(elevation)
e <- as.data.frame(e) %>% 
  rename(elevation = layer)

e_spdf <- SpatialPointsDataFrame(e[,1:2], proj4string = elevation@crs, e)

#this is taking 2+ hours, might want to do overnight
e_spdf$elevation_min <- raster::extract(elevation, e_spdf,
                                   buffer = 20, #20m I think?
                                   weights = F, 
                                   fun = min)

e_spdf$elevation_min[e_spdf$elevation_min <0] <- 0

e_spdf$elevation_max <- raster::extract(elevation, e_spdf,
                                        buffer = 20,
                                        weights = F, 
                                        fun = max)

e$elevation_range <- e$elevation_max - e$elevation_min

# Terra might be faster??
elevation <- rast("data/elevationRaster.tif")
e <- as.points(elevation)
#convert to points
sp <- SpatialPoints(e)
#create a buffer around the points
sp_buffer <-st_buffer(st_as_sf(e),2000) #this...is not any faster. Just getting the buffer is taking forever. 


