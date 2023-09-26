# No previous scripts needed, but this isn't working. 
library(raster); library(viridis);library(ggplot2); library(dplyr); 
library(tidyr); library(sf); library(terra); library(tictoc)

# setup, skip now
library(elevatr)

precip <- raster("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
elevation2 <- get_elev_raster(precip, z = 5, neg_to_na = T)

crs(elevation) <- "EPSG:9822"
crs(precip) <- "EPSG:9822"
elevation <- raster::resample(elevation, precip)
elevation <- mask(elevation, precip)
plot(elevation)
writeRaster(elevation, file = "data/elevationRaster.tif", overwrite = T)


# Trying to get min-max ---------------------------------------------------

library(terra); library(tidyr); library(dplyr)
elevation <- rast("data/elevationRaster2.tif")

e <- terra::as.data.frame(elevation, xy = TRUE, na.rm = T)

evect <- vect(e, geom=c("x", "y"), crs = elevation)

b <- buffer(evect, 20000)
e_min <- extract(elevation, b, min, na.rm=TRUE)
emin <- e_min %>% 
  rename(e_min = elevationRaster2)
a <- cbind(e, emin)
write.csv(a,file = 'data/elevation_min')


e_max <- terra::extract(elevation, b, max, na.rm=TRUE)

e_max <- e_max %>% 
  rename(e_max = elevationRaster2)
a <- cbind(e, e_max)
write.csv(a,file = 'data/elevation_max')
