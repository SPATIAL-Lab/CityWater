
library(terra); library(elevatr); library(assignR)

precip <- rast("maps/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")

s = project(states, "ESRI:102003")
precip = project(precip, "ESRI:102003")
precip = crop(precip, ext(s))

p <- terra::as.data.frame(precip, xy = TRUE, na.rm = F)
pvect <- vect(p, geom=c("x", "y"), crs = precip)
pvect1 <- buffer(pvect, 20000)
## Find mean value within area
p_mean <- terra::extract(precip, pvect1, mean, na.rm=F)

precip_mean = precip
values(precip_mean) = p_mean[, 2]
plot(precip_mean)
## Write
writeRaster(precip_mean, "maps/precip_mean.tif", overwrite = T)
