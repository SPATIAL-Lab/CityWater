# No previous scripts needed, but this isn't working. 
library(raster); library(viridis);library(ggplot2); library(dplyr); 
library(tidyr); library(sf); library(terra); library(tictoc)

# setup, skip now
library(censusapi);library(elevatr);library(tigris, options(tigris_use_cache = TRUE)) 
conus <- counties(cb = TRUE)
conus$STATEFP <- as.numeric(conus$STATEFP)
conus <- subset(conus, STATEFP < 60)
conus <- subset(conus, STATEFP != 02)
conus <- subset(conus, STATEFP != 15)

# Setup, skip now ---------------------------------------------------------

elevation <- get_elev_raster(conus, z = 5, neg_to_na = T)
precip <- raster("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
crs(elevation) <- "EPSG:9822"
crs(precip) <- "EPSG:9822"
elevation <- raster::resample(elevation, precip)
elevation <- mask(elevation, precip)
plot(elevation)
writeRaster(elevation, file = "data/elevationRaster.tif", overwrite = T)


# Trying to get min-max ---------------------------------------------------

elevation <- raster("data/elevationRaster.tif")

crs(elevation) <- "EPSG:9822"
plot(elevation)
e <- rasterToPoints(elevation)
e <- as.data.frame(e) %>% 
  rename(elevation = elevationRaster)

e <- SpatialPointsDataFrame(e[,1:2], proj4string = elevation@crs, e)
e$elevation_min <- 0
e$elevation_min <- raster::extract(elevation, e,
                                         buffer = 2000,
                                         weights = F, 
                                         fun = min)
