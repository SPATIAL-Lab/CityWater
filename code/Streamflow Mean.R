
library(terra); library(elevatr); library(assignR)

streamflow <- rast("maps/fa_qs_ann.tif")

s = project(states, "ESRI:102003")
streamflow = project(streamflow, "ESRI:102003")
streamflow = crop(streamflow, ext(s))
streamflow = mask(streamflow, s)

stream <- terra::as.data.frame(streamflow, xy = TRUE, na.rm = T)
svect <- vect(stream, geom=c("x", "y"), crs = streamflow)

s1 <- buffer(svect[1:6000000], 20000)
s2 <- buffer(svect[6000001:length(svect)], 20000)
svect1 <- rbind(s1, s2)
#writeVector(b, "maps/streamflowint.shp", overwrite = T)
## Find mean value within area
#s1 <- buffer(svect, 20000)
gc()
#writeVector(s1, "maps/sfNA.shp", overwrite = T)
#s1 <- vect("maps/streamflowint.shp")
s_mean <- terra::extract(streamflow, svect1, mean, na.rm = T)
gc()
svect2 = svect
values(svect2) = s_mean[, 2]

#rasterize result
streamflow_mean = rasterize(svect2, streamflow, field = "value")

## Write
writeRaster(streamflow_mean, "maps/streamflow_mean.tif", overwrite = T)