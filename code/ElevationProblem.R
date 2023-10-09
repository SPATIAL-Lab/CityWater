library(terra); library(tidyr); library(dplyr); library(ggplot2); library(tidyterra); 
library(dplyr)

# This is how I generate min and max values -------------------------------

elevation <- rast("data/elevationRaster2.tif")
e <- terra::as.data.frame(elevation, xy = TRUE, na.rm = T)
#create buffered zone for each point, 20km
evect <- vect(e, geom=c("x", "y"), crs = elevation)
b <- buffer(evect, 40000)

#find minimum value within area
e_min <- terra::extract(elevation, b, min, na.rm=TRUE)
emin <- e_min %>% 
  rename(e_min = elevationRaster2)
e_min <- e_min %>% select(x, y, e_min)
write.csv(e_min,file = 'data/elevation_min.csv')
#find maximum value within area
e_max <- terra::extract(elevation, b, max, na.rm=TRUE)
e_max <- e_max %>% 
  rename(e_max = elevationRaster2)
emax <- cbind(e, e_max) %>% select(x, y, e_max)
write.csv(emax,file = 'data/elevation_max.csv')
# Once assessed, this is the code for generating the SpatRaster -----------

#Combining min and max to get range of values within 20km areas
emin <- read.csv('data/elevation_min.csv')
emax <- read.csv('data/elevation_max.csv')
elevation <- rast("data/elevationRaster2.tif")
#dataframe with both values to figure out range
elevation_range <- left_join(emax, emin, join_by(x, y))
elevation_range$elerange <- elevation_range$e_max - elevation_range$e_min
elevation_range <- elevation_range %>% select(x, y, elerange)

#create vector of ranges
eleVect <- vect(elevation_range, geom=c("x", "y"), crs = elevation)

#as a SpatVector it's...fine. It's when I rasterize it to prepare for linear modelling that it gets really weird
ggplot(eleVect)+ 
  geom_spatvector(aes(color = elerange))

eleRast <- rasterize(eleVect, elevation, 'elerange')
#not when its set to 40km????
writeRaster(eleRast, "data/eleRast.tif")
