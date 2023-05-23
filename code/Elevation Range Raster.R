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
e$group <- sample(10, size = nrow(e), replace = TRUE)
split_names <- c("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10")

mysplits <- split(e, e$group)


for (i in 1:length(mysplits)) {        # Run for-loop
   assign(split_names[i], mysplits[[i]])
}

for (i in 1:length(split_names)) {
  paste0(i, "_spdf") <- SpatialPointsDataFrame(i[,1:2], proj4string = "+proj=longlat +datum=NAD83 +no_defs", i)
}

e1_spdf <- SpatialPointsDataFrame(e1[,1:2], proj4string = elevation@crs, e1)
e2_spdf <- SpatialPointsDataFrame(e2[,1:2], proj4string = elevation@crs, e2)
e3_spdf <- SpatialPointsDataFrame(e3[,1:2], proj4string = elevation@crs, e3)
e4_spdf <- SpatialPointsDataFrame(e4[,1:2], proj4string = elevation@crs, e4)
e5_spdf <- SpatialPointsDataFrame(e5[,1:2], proj4string = elevation@crs, e5)
e6_spdf <- SpatialPointsDataFrame(e6[,1:2], proj4string = elevation@crs, e6)
e7_spdf <- SpatialPointsDataFrame(e7[,1:2], proj4string = elevation@crs, e7)
e8_spdf <- SpatialPointsDataFrame(e8[,1:2], proj4string = elevation@crs, e8)
e9_spdf <- SpatialPointsDataFrame(e9[,1:2], proj4string = elevation@crs, e9)
e10_spdf <- SpatialPointsDataFrame(e10[,1:2], proj4string = elevation@crs, e10)

# Let's do a smol one too to test
smol <- head(e, 6000)
smol_spdf <- SpatialPointsDataFrame(smol[,1:2], proj4string = elevation@crs, smol)
#doing this in a smol chunk is working, may need to think about splitting and then recombining if parallel processing isn't working. 
spdf_names <- c("e1_spdf", "e2_spdf", "e3_spdf", "e4_spdf", "e5_spdf", "e6_spdf", "e7_spdf", "e8_spdf", "e9_spdf", "e10_spdf")

for(i in 1:length(spdf_names)) {
  i$elevation_min <- 0
  i$elevation_max <- 0

}

e1_spdf$elevation_min <- 0
e1_spdf$elevation_min <- raster::extract(elevation, e1_spdf,
                                   buffer = 2000, #2k I think?
                                   weights = F, 
                                   fun = min)

e1_spdf$elevation_min[e1_spdf$elevation_min <0] <- 0

e1_spdf$elevation_max <- 0
e1_spdf$elevation_max <- raster::extract(elevation, e1_spdf,
                                        buffer = 2000,
                                        weights = F, 
                                        fun = max)

e1_spdf$elevation_range <- e1_spdf$elevation_max - e1_spdf$elevation_min

# Terra might be faster??
elevation <- rast("data/elevationRaster.tif")
e <- as.points(elevation)
#convert to points
sp <- SpatialPoints(e1) # broken
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


library(furrr); library(tictoc)
tic()
smol_spdf$elevation_max = 0
smol_spdf$elevation_max <- raster::extract(elevation, smol_spdf,
                  buffer = 2000,
                  weights = F, 
                  fun = max)
toc()
# 178.66 sec elapsed
rm(smol_spdf)
smol_spdf <- SpatialPointsDataFrame(smol[,1:2], proj4string = elevation@crs, smol)
tic()
furrr::furrr_options(packages = ("raster"))
smol_spdf$elevation_max <- future_map(1:2, function(x) {
  raster::extract(elevation, smol_spdf,
                  buffer = 2000,
                  weights = F, 
                  fun = max)
})
toc()
# 365.96 sec elapsed okay this takes longer as it's making odd lists 

tic()
beginCluster()
smol_spdf$elevation_max = 0
smol_spdf$elevation_max <- raster::extract(elevation, smol_spdf,
                                           buffer = 2000,
                                           weights = F, 
                                           fun = max)
endCluster()
toc()
# 195.29 sec elapsed