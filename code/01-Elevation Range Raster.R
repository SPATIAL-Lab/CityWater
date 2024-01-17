# Needed for Cluster Location Info Scraping. No previous scripts needed. Note that this takes a while (hours/a day) depending on your processing power
library(terra); library(elevatr); library(assignR); library(raster)

## Get data, using precip layer as target
precip <- rast("maps/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
elevation <- get_elev_raster(raster(precip), z = 5)
elev = rast(elevation)

## Set negative values to zero
ev = values(elev)
ev[ev < 0] = 0
values(elev) = ev

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

## Write
writeRaster(elev_diff, "out/elev_diff.tif")