# Needed for Cluster Location Info Scraping. No previous scripts needed. Note that this takes a while (hours/a day) depending on your processing power
library(terra); library(assignR)

## Get data
p <- rast("maps/precip_mean.tif")
elev = rast("maps/dem.tif")
s = project(states, p)

elev = project(elev, p)

plot(elev, colNA = "red")
lines(s)

## Convert to vector
e <- as.data.frame(elev, xy = TRUE, na.rm = FALSE)
evect <- vect(e, geom=c("x", "y"), crs = elev)

## Buffer
b = buffer(evect, 100000)

## Find minimum value within area
e_min <- extract(elev, b, min, na.rm=TRUE)

## Find maximum value within area
e_max <- extract(elev, b, max, na.rm=TRUE)

## Difference
e_diff = e_max[, 2] - e_min[, 2]
summary(e_diff)

## Back on the raster
elev_diff = elev
values(elev_diff) = e_diff
plot(elev_diff)

## Write
writeRaster(elev_diff, "maps/elev_diff.tif", overwrite = TRUE)
