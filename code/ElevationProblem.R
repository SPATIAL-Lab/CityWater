library(terra); library(tidyr); library(dplyr); library(ggplot2); library(tidyterra)

# This is how I generate min and max values -------------------------------

elevation <- rast("data/elevationRaster2.tif")
e <- terra::as.data.frame(elevation, xy = TRUE, na.rm = T)
#create buffered zone for each point, 20km
evect <- vect(e, geom=c("x", "y"), crs = elevation)
b <- buffer(evect, 20000)

#find minimum value within area
e_min <- terra::extract(elevation, b, min, na.rm=TRUE)
emin <- e_min %>% 
  rename(e_min = elevationRaster2)
e_min <- cbind(e, emin)

#find maximum value within area
e_max <- terra::extract(elevation, b, max, na.rm=TRUE)
e_max <- e_max %>% 
  rename(e_max = elevationRaster2)
e_max <- cbind(e, e_max)

# Once assessed, this is the code for generating the SpatRaster -----------

#Combining min and max to get range of values within 20km areas
e_min <- read.csv('data/elevation_min.csv')
e_max <- read.csv('data/elevation_max.csv')
elevation <- rast("data/elevationRaster2.tif")
#dataframe with both values to figure out range
elevation_range <- left_join(e_max, e_min, join_by(x, y))
elevation_range$elerange <- elevation_range$maximum - elevation_range$minimum
elevation_range <- elevation_range %>% select(x, y, elerange)

#create vector of ranges
eleVect <- vect(elevation_range, geom=c("x", "y"), crs = elevation)
eleVect <- project(eleVect, 'EPSG:4326')

#as a SpatVector it's...fine. It's when I rasterize it to prepare for linear modelling that it gets really weird
ggplot(eleVect)+ 
  geom_spatvector(aes(color = range))

precip <- rast("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
eleRast <- rasterize(eleVect, precip, 'elerange')
#weird dots! 
