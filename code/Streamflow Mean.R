
library(terra); library(elevatr); library(assignR)

streamflow <- rast("maps/fa_qs_ann.tif")

s = project(states, "ESRI:102003")
streamflow = crop(streamflow, ext(s))

p <- terra::as.data.frame(streamflow, xy = TRUE, na.rm = T)
pvect <- vect(p, geom=c("x", "y"), crs = streamflow)



## Find minimum value within area
p_mean <- terra::extract(streamflow, pvect, mean, na.rm=TRUE)

streamflow_mean = streamflow
values(streamflow_mean) = p_mean
plot(streamflow_mean)
## Write
writeRaster(p_mean, "maps/precip_mean.tif", overwrite = T)