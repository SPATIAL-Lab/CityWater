library(raster); library(viridis);library(ggplot2); library(dplyr); 
library(tidyr); library(sf); library(terra); library(tictoc)

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

# Current Setup
# Elevation min max range -------------------------------------------------

elevation <- raster("data/elevationRaster.tif")
#e <- rasterToPoints(elevation)
#e <- as.data.frame(e) %>% 
#  rename(elevation = layer)
#define number of data frames to split into
# e$group <- sample(10, size = nrow(e), replace = TRUE)

split_names <- c("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10")
#mysplits <- split(e, e$group)

#save(mysplits, file = "mysplits.Rda")
load("mysplits.Rda")
for (i in 1:length(mysplits)) {        # Run for-loop
   assign(split_names[i], mysplits[[i]])
}

rasterOptions()
# default max memory is 5e+09
rasterOptions(maxmemory = 5e+20)

# e1 DONE
e1_spdf <- SpatialPointsDataFrame(e1[,1:2], proj4string = elevation@crs, e1)

e1_spdf$elevation_min <- 0
e1_spdf$elevation_min <- raster::extract(elevation, e1_spdf,
                                         buffer = 2000,
                                         weights = F, 
                                         fun = min)
e1_spdf$elevation_min[e1_spdf$elevation_min <0] <- 0
e1 <- as.data.frame(e1_spdf)
save(e1, file = "data/e1.Rda")
load(e1)
e1_spdf <- SpatialPointsDataFrame(e1[,1:2], proj4string = elevation@crs, e1)
e1_spdf$elevation_max <- 0
e1_spdf$elevation_max <- raster::extract(elevation, e1_spdf,
                                         buffer = 2000,
                                         weights = F,
                                         fun = max)
e1_spdf$elevation_max[e1_spdf$elevation_max <0] <- 0
e1_spdf$elevation_range <- e1_spdf$elevation_max - e1_spdf$elevation_min
e1finished <- as.data.frame(e1_spdf)
save(e1finished, file = "data/e1finished.Rda")

# e2 MAX WEIRD RE-RUN LATER
load("data/e2.Rda")
e2_spdf <- SpatialPointsDataFrame(e2[,1:2], proj4string = elevation@crs, e2)
e2_spdf$elevation_max <- 0
e2_spdf$elevation_max <- raster::extract(elevation, e2_spdf,
                                         buffer = 2000,
                                         fun = max)
e2_spdf$elevation_max[e2_spdf$elevation_max <0] <- 0
e2_spdf$elevation_range <- e2_spdf$elevation_max - e2_spdf$elevation_min #RERUN THIS, ERROR BEFORE
e2 <- as.data.frame(e2_spdf)
save(e2, file = "data/e2finished.Rda")

#e3Start here
e3_spdf <- SpatialPointsDataFrame(e3[,1:2], proj4string = elevation@crs, e3)
e3_spdf$elevation_min <- 0
e3_spdf$elevation_min <- raster::extract(elevation, e3_spdf,
                                         buffer = 2000,
                                         weights = F, 
                                         fun = min)
e3_spdf$elevation_min[e3_spdf$elevation_min <0] <- 0
e3 <- as.data.frame(e3_spdf)
save(e3, file = "data/e3wip.Rda")
e3_spdf$elevation_max <- 0
e3_spdf$elevation_max <- raster::extract(elevation, e3_spdf,
                                         buffer = 2000,
                                         weights = F,
                                         fun = max)
e3_spdf$elevation_max[e3_spdf$elevation_max <0] <- 0
e3_spdf$elevation_range <- e3_spdf$elevation_max - e3_spdf$elevation_min
e3 <- as.data.frame(e3_spdf)
save(e3, file = "data/e3finished.Rda")


e4_spdf <- SpatialPointsDataFrame(e4[,1:2], proj4string = elevation@crs, e4)
e5_spdf <- SpatialPointsDataFrame(e5[,1:2], proj4string = elevation@crs, e5)
e6_spdf <- SpatialPointsDataFrame(e6[,1:2], proj4string = elevation@crs, e6)
e7_spdf <- SpatialPointsDataFrame(e7[,1:2], proj4string = elevation@crs, e7)
e8_spdf <- SpatialPointsDataFrame(e8[,1:2], proj4string = elevation@crs, e8)
e9_spdf <- SpatialPointsDataFrame(e9[,1:2], proj4string = elevation@crs, e9)
e10_spdf <- SpatialPointsDataFrame(e10[,1:2], proj4string = elevation@crs, e10)

for(i in 1:length(split_names)) {
expression(paste(i, "_rda"))  <- as.data.frame(i,"_spdf")
}


#start here 
rasterOptions()
# default max memory is 5e+09
rasterOptions(maxmemory = 5e+12)

e2_spdf$elevation_min <- 0
# if smol took 178.66 sec to process 6000 points, this should take about...5 hours. 
e2_spdf$elevation_min <- raster::extract(elevation, e2_spdf,
                                         buffer = 2000,
                                         weights = F, 
                                         fun = min)

e2_spdf$elevation_min[e2_spdf$elevation_min <0] <- 0

e3_spdf$elevation_max <- 0
e3_spdf$elevation_max <- raster::extract(elevation, e3_spdf,
                                         buffer = 2000,
                                         fun = max)
e3_spdf$elevation_max[e3_spdf$elevation_max <0] <- 0
e3a <- as.data.frame(e3_spdf) 

e3b <- cbind(e3, e3a)

e1_spdf$elevation_range <- e2_spdf$elevation_max - e2_spdf$elevation_min


e2 <- as.data.frame(e2_spdf)
save(e2, file = "data/e2.Rda")

# experiment in allocating more memory to raster functions
# Let's do a smol one too to test
smol <- head(e, 6000)
smol_spdf <- SpatialPointsDataFrame(smol[,1:2], proj4string = elevation@crs, smol)
# what if we check rasterOptions() and then allot more memory? 
rasterOptions()
# default max memory is 5e+09
rasterOptions(maxmemory = 5e+10)
tic()
smol_spdf$elevation_max = 0
smol_spdf$elevation_max <- raster::extract(elevation, smol_spdf,
                                           buffer = 2000,
                                           weights = F, 
                                           fun = max)
toc()
e1 <- as.data.frame(e1_spdf)
save(e1, file = "e1.Rda")
#comparing base code to expanded memory run on my netbook, we're going from 259 to 152 seconds. 

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

plot(elevation)
plot(e2_spdf)
