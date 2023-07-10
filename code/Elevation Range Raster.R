library(raster); library(viridis);library(ggplot2); library(dplyr); 
library(tidyr); library(sf); library(terra); library(tictoc)

# setup, skip now
library(censusapi);library(elevatr);library(tigris, options(tigris_use_cache = TRUE)) 
conus <- counties(cb = TRUE)
conus$STATEFP <- as.numeric(conus$STATEFP)
conus <- subset(conus, STATEFP < 60)
conus <- subset(conus, STATEFP != 02)
conus <- subset(conus, STATEFP != 15)

elevation <- get_elev_raster(conus, z = 5, neg_to_na = T)
writeRaster(elevation, "data/elevationRaster2.tif", overwrite = T)
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
rasterOptions(maxmemory = 5e+25)

# e1 FINISHED
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

#e3 FINISHED
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

#e4 Finished
e4_spdf <- SpatialPointsDataFrame(e4[,1:2], proj4string = elevation@crs, e4)

e4_spdf$elevation_max <- raster::extract(elevation, e4_spdf,
                                         buffer = 2000,
                                         weights = F,
                                         fun = max)
e4_spdf$elevation_max[e4_spdf$elevation_max <0] <- 0
e4_spdf$elevation_range <- e4_spdf$elevation_max - e4_spdf$elevation_min
e4 <- as.data.frame(e4_spdf)
save(e4, file = "data/e4finished.Rda")

# e5 FINISHED
e5_spdf <- SpatialPointsDataFrame(e5[,1:2], proj4string = elevation@crs, e5)
e5_spdf$elevation_min <- 0
e5_spdf$elevation_min <- raster::extract(elevation, e5_spdf,
                                         buffer = 2000,
                                         weights = F, 
                                         fun = min)
e5_spdf$elevation_min[e5_spdf$elevation_min <0] <- 0
e5 <- as.data.frame(e5_spdf)
save(e5, file = "data/e5wip.Rda")
e5_spdf$elevation_max <- raster::extract(elevation, e5_spdf,
                                         buffer = 2000,
                                         weights = F,
                                         fun = max)
e5_spdf$elevation_max[e5_spdf$elevation_max <0] <- 0
e5_spdf$elevation_range <- e5_spdf$elevation_max - e5_spdf$elevation_min
e5 <- as.data.frame(e5_spdf)
save(e5, file = "data/e5finished.Rda")

# e6 working on work computer
e6_spdf <- SpatialPointsDataFrame(e6[,1:2], proj4string = elevation@crs, e6)


# e7 FINISHED------------------------------------------------------------------
e7_spdf <- SpatialPointsDataFrame(e7[,1:2], proj4string = elevation@crs, e7)
e7_spdf$elevation_min <- 0
e7_spdf$elevation_min <- raster::extract(elevation, e7_spdf,
                                         buffer = 2000,
                                         weights = F, 
                                         fun = min)
e7_spdf$elevation_min[e7_spdf$elevation_min <0] <- 0
e7 <- as.data.frame(e7_spdf)
save(e7, file = "data/e7wip.Rda")
e7_spdf$elevation_max <- raster::extract(elevation, e7_spdf,
                                         buffer = 2000,
                                         weights = F,
                                         fun = max)
e7_spdf$elevation_max[e7_spdf$elevation_max <0] <- 0
e7_spdf$elevation_range <- e7_spdf$elevation_max - e7_spdf$elevation_min
e7 <- as.data.frame(e7_spdf)
save(e7, file = "data/e7finished.Rda")


# e8 FINISHED -------------------------------------------------------------------

e8_spdf <- SpatialPointsDataFrame(e8[,1:2], proj4string = elevation@crs, e8)

e8_spdf$elevation_min <- 0
e8_spdf$elevation_min <- raster::extract(elevation, e8_spdf,
                                         buffer = 2000,
                                         weights = F, 
                                         fun = min)
e8_spdf$elevation_min[e8_spdf$elevation_min <0] <- 0
e8 <- as.data.frame(e8_spdf)
save(e8, file = "data/e8wip.Rda")


load("data/e8wip.Rda")
e8_spdf$elevation_max <- 0
e8_spdf$elevation_max <- raster::extract(elevation, e8_spdf,
                                         buffer = 2000,
                                         weights = F,
                                         fun = max)
e8_spdf$elevation_max[e8_spdf$elevation_max <0] <- 0
e8_spdf$elevation_range <- e8_spdf$elevation_max - e8_spdf$elevation_min
e8 <- as.data.frame(e8_spdf)
save(e8, file = "data/e8finished.Rda")


# e9 elevation_min messing up ---------------------------------------------

e9_spdf <- SpatialPointsDataFrame(e9[,1:2], proj4string = elevation@crs, e9)
e9_spdf$elevation_min <- 0
e9_spdf$elevation_min <- raster::extract(elevation, e9_spdf,
                                         buffer = 2000,
                                         weights = F, 
                                         fun = min)
e9_spdf$elevation_min[e9_spdf$elevation_min <0] <- 0
e9 <- as.data.frame(e9_spdf)
save(e9, file = "data/e9wip.Rda")


load("data/e9wip.Rda")
e9_spdf$elevation_max <- 0
e9_spdf$elevation_max <- raster::extract(elevation, e9_spdf,
                                         buffer = 2000,
                                         weights = F,
                                         fun = max)
e9_spdf$elevation_max[e9_spdf$elevation_max <0] <- 0
e9_spdf$elevation_range <- e9_spdf$elevation_max - e9_spdf$elevation_min
e9 <- as.data.frame(e9_spdf)
save(e9, file = "data/e9finished.Rda")

# e10FINISHED -------------------------------------------------------------

e10_spdf <- SpatialPointsDataFrame(e10[,1:2], proj4string = elevation@crs, e10)
e10_spdf$elevation_min <- 0
e10_spdf$elevation_min <- raster::extract(elevation, e10_spdf,
                                         buffer = 2000,
                                         weights = F, 
                                         fun = min)
e10_spdf$elevation_min[e10_spdf$elevation_min <0] <- 0
e10 <- as.data.frame(e10_spdf)
save(e10, file = "data/e10wip.Rda")

load("data/e10wip.Rda")
e10_spdf$elevation_max <- 0
e10_spdf$elevation_max <- raster::extract(elevation, e10_spdf,
                                         buffer = 2000,
                                         weights = F,
                                         fun = max)
e10_spdf$elevation_max[e10_spdf$elevation_max <0] <- 0
e10_spdf$elevation_range <- e10_spdf$elevation_max - e10_spdf$elevation_min
e10 <- as.data.frame(e10_spdf)
save(e10, file = "data/e10finished.Rda")

# wait I've made a huge mistake
load("data/e1finished.Rda")
load("data/e2finished.Rda")
load("data/e3finished.Rda")
load("data/e4finished.Rda")
load("data/e5finished.Rda")
#load("data/e6finished.Rda")
load("data/e7finished.Rda")
load("data/e8finished.Rda")
load("data/e9finished.Rda")
load("data/e10finished.Rda")

e1 <- e1finished

e1 <- e1 %>% 
  select(c(x, y, elevation_range, group))

e3 <- e3 %>% 
  select(c(x, y, elevation_range, group))
e4 <- e4 %>% 
  select(c(x, y, elevation_range, group))
e5 <- e5 %>% 
  select(c(x, y, elevation_range, group))
e7 <- e7 %>% 
  select(c(x, y, elevation_range, group))
e8 <- e8 %>% 
  select(c(x, y, elevation_range, group))
e9 <- e9 %>% 
  select(c(x, y, elevation_range, group))
e10 <- e10 %>% 
  select(c(x, y, elevation_range, group))

er <- rbind(e1,e3,e4,e5,e7,e8,e9,e10)
er <- er %>% 
  rename(g = group)
er$group <- as.character(er$group)

ggplot(data = er, aes(x = elevation_range, fill = group))+ 
  geom_density(alpha = 0.2) + 
  xlim(0,50)

summary(aov(data = er, elevation_range ~ group))
# so yeah, no difference...
e1 <- e1 %>% 
  rename(lat = y, 
         lon = x)
transformed_data <- usmap_transform(e1)

ggplot() + 
  geom_point(data = e1, aes(x = lon, y = lat, color = elevation_range))

# Let's give this one more shot of actually calculating what I want. 
crs(elevation) <- "EPSG:9822"
plot(elevation)
e <- rasterToPoints(elevation)
e <- as.data.frame(e) %>% 
  rename(elevation = file58742f9e411)

e <- SpatialPointsDataFrame(e[,1:2], proj4string = elevation@crs, e)
e$elevation_min <- 0
e$elevation_min <- raster::extract(elevation, e,
                                         buffer = 2000,
                                         weights = F, 
                                         fun = min)
emat <- as.matrix(e)
