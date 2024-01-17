
library(terra); library(elevatr); library(assignR)

precip <- rast("maps/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")

s = project(states, "ESRI:102003")
pt = rast(extent = ext(s), resolution = 5e3, crs = crs(s))
precip = project(precip, pt)

p <- as.data.frame(precip, xy = TRUE, na.rm = TRUE)
pvect <- vect(p, geom=c("x", "y"), crs = precip)
pvect1 <- buffer(pvect, 20000)
## Find mean value within area
p_mean <- extract(precip, pvect1, mean, na.rm = TRUE)

#put buffered values back on point geom
pvect2 = pvect
values(pvect2) = p_mean[, 2]

#rasterize result
precip_mean = rasterize(pvect2, precip, field = "value")
plot(precip_mean)

## Write
writeRaster(precip_mean, "maps/precip_mean.tif", overwrite = TRUE)
