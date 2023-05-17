library(raster); library(censusapi);library(elevatr);
library(tigris, options(tigris_use_cache = TRUE)); library(viridis); 
library(ggplot2); library(dplyr); library(tidyr); library(sf); library(terra)

# setup, skip now
conus <- counties(cb = TRUE)
conus$STATEFP <- as.numeric(conus$STATEFP)
conus <- subset(conus, STATEFP < 60)
conus <- subset(conus, STATEFP != 02)
conus <- subset(conus, STATEFP != 15)
#elevation
elevation <- get_elev_raster(conus, z = 5)
writeRaster(elevation, "data/elevationRaster.tif", overwrite = T)
# e <- rasterToPolygons(elevation) # Four hours and counting...


# Elevation min max range -------------------------------------------------

elevation <- raster("data/elevationRaster.tif")
e <- rasterToPoints(elevation)
e <- as.data.frame(e) %>% 
  rename(elevation = file3f9023746e68)

e_spdf <- SpatialPointsDataFrame(e[,1:2], proj4string = elevation@crs, e)

e_spdf$elevation_min <- raster::extract(elevation, e_spdf,
                                   buffer = 20000, #20k I think?
                                   weights = F, 
                                   fun = min)

e_spdf$elevation_min[e_spdf$elevation_min <0] <- 0
e$elevation_max <- raster::extract(elevation, e,
                                              weights = F, fun = max)

e$elevation_range <- e$elevation_max - e$elevation_min