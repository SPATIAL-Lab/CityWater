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

precip <- rast("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
eleRast <- rasterize(eleVect, precip, 'elerange')
#weird dots! 

eleRast <- rasterize(eleVect, elevation, 'elerange')
#not when its set to 40km????
writeRaster(eleRast, "data/eleRast.tif")

# Gabe's Code ----

## Get data, using precip layer as target
precip <- rast("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
elevation <- get_elev_raster(precip, z = 5)
elev = rast(elevation)

## Set negative values to zero
ev = values(elev)
ev[ev < 0] = 0
values(elev) = ev

## Project to equal AEA
elev = project(elev, "ESRI:102003")
plot(elev, colNA = "red")

## For cropping and plotting
library(assignR)
s = project(states, "ESRI:102003")
elev = crop(elev, ext(s))
plot(elev, colNA = "red")
lines(s)

## Convert to vector
e <- terra::as.data.frame(elev, xy = TRUE, na.rm = T)
evect <- vect(e, geom=c("x", "y"), crs = elev)

## Buffer; had to break this up due to memory limitations
b1 <- buffer(evect[1:2000000], 100000)
b2 <- buffer(evect[2000001:length(evect)], 100000)
b = rbind(b1, b2)

## Find minimum value within area
e_min <- terra::extract(elev, b, min, na.rm=TRUE)

## Find maximum value within area
e_max <- terra::extract(elev, b, max, na.rm=TRUE)

## Difference
e_diff = e_max[, 2] - e_min[, 2]
summary(e_diff)

## Back on the raster
elev_diff = elev
values(elev_diff) = e_diff
plot(elev_diff)
lines(s)

## Write
writeRaster(elev_diff, "out/elev_diff.tif")
