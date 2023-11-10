
library(terra); library(elevatr); library(assignR)

precip <- rast("maps/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
elev_diff <- rast("maps/elev_diff.tif")

s = project(states, "ESRI:102003")
precip = crop(precip, ext(s))

p <- terra::as.data.frame(precip, xy = TRUE, na.rm = T)
pvect <- vect(p, geom=c("x", "y"), crs = precip)

## Buffer; had to break this up due to memory limitations
p1 <- buffer(pvect[1:2000000], 20000)
p2 <- buffer(pvect[2000001:length(pvect)], 20000)
p = rbind(p1, p2)

## Find minimum value within area
p_min <- terra::extract(precip, p, min, na.rm=TRUE)

## Find maximum value within area
p_max <- terra::extract(precip, p, max, na.rm=TRUE)

## Difference
p_diff = p_max[, 2] - p_min[, 2]
summary(p_diff)

## Back on the raster
precip_diff = precip
values(precip_diff) = p_diff
plot(precip_diff)

## Write
writeRaster(precip_diff, "maps/precip_diff.tif")