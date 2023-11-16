
library(terra); library(elevatr); library(assignR)

precip <- rast("maps/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")

s = project(states, "ESRI:102003")
precip = crop(precip, ext(s))

p <- terra::as.data.frame(precip, xy = TRUE, na.rm = T)
pvect <- vect(p, geom=c("x", "y"), crs = precip)

## Buffer; had to break this up due to memory limitations
#p1 <- buffer(pvect[1:200000], 20000)
#p2 <- buffer(pvect[200001:length(pvect)], 20000)
#p = rbind(p1, p2)

# I don't think I need to divide and conquer
p3 <- buffer(pvect, 20000)
## Find minimum value within area
p_mean <- terra::extract(precip, p3, mean, na.rm=TRUE)

precip_mean = precip
values(precip_mean) = p_mean
plot(precip_mean)
## Write
writeRaster(p_mean, "maps/precip_mean.tif", overwrite = T)

