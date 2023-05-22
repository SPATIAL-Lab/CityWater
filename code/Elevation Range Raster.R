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
#define number of data frames to split into
smol <- head(e, 60000)

e_spdf <- SpatialPointsDataFrame(smol[,1:2], proj4string = elevation@crs, smol)

#doing this in a smol chunk is working, may need to think about splitting and then recombining if parallel processing isn't working. 
e_spdf$elevation_min <- 0
e_spdf$elevation_min <- raster::extract(elevation, e_spdf,
                                   buffer = 2000, #2k I think?
                                   weights = F, 
                                   fun = min)

e_spdf$elevation_min[e_spdf$elevation_min <0] <- 0

e_spdf$elevation_max <- 0
e_spdf$elevation_max <- raster::extract(elevation, e_spdf,
                                        buffer = 2000,
                                        weights = F, 
                                        fun = max)

e_spdf$elevation_range <- e_spdf$elevation_max - e_spdf$elevation_min

# Terra might be faster??
elevation <- rast("data/elevationRaster.tif")
e <- as.points(elevation)
#convert to points
sp <- SpatialPoints(e) # broken
#create a buffer around the points
sp_buffer <-st_buffer(st_as_sf(e),2000) #sf package 
sp_buffer <- buffer(e, 2000) # terra
# trying parallel processing

library(parallel)
# Detect the number of available cores and create cluster
cl <- parallel::makeCluster(detectCores())
# Run parallel computation. This uhhhh might not be the correct way to write this
sp_buffer <- parLapply(cl = cl, fun = terra::buffer(e, 2000))

# Close cluster
parallel::stopCluster(cl)

# furrr????
# there is a StackOverflow specifically about raster::extract using furrr https://stackoverflow.com/questions/74739153/how-to-use-parallelization-with-rasterextract-in-r-using-furrr

library(furrr)
furrr::furrr_options(packages = ("raster"))
future_map(1:2, function(x) {
  raster::extract(elevation, e_spdf,
                  buffer = 2000,
                  weights = F, 
                  fun = max)
})
