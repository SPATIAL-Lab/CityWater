
library(terra); library(elevatr); library(assignR)

# Prep streamflow dataset
streamflow <- rast("maps/sf.tif")
s = project(states, "ESRI:102003")

streamflow = project(streamflow, s)
streamflow = crop(streamflow, ext(s))
streamflow = mask(streamflow, s)

# Streamflow is in units 10^3 m3 / yr, convert to km3/yr
streamflow = streamflow / 1e6

# Create vector version
stream <- as.data.frame(streamflow, xy = TRUE, na.rm = TRUE)
svect <- vect(stream, geom=c("x", "y"), crs = streamflow)

# Buffer
s1 <- buffer(svect[1:6000000], 20000)
s2 <- buffer(svect[6000001:length(svect)], 20000)
svect1 <- rbind(s1, s2)
gc()

s_mean <- extract(streamflow, svect1, mean, na.rm = TRUE)
gc()
svect2 = svect
values(svect2) = s_mean[, 2]

#rasterize result
streamflow_mean = rasterize(svect2, streamflow, field = "value")

## Write
writeRaster(streamflow_mean, "maps/streamflow_mean.tif", overwrite = TRUE)
