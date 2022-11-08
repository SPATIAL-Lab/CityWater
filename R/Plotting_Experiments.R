#Let's try creating shapefiles matching the US Census using tigris, a package which directly pulls from Census data and plays well with tidyverse 
#This assumed G0 has been run. 
library(tigris)
library(ggplot2)
library(viridis)
library(raster)

#census data is nad83/ EPSG:4269
# We want to use LAEA projection 

slc_counties <- c("Davis", "Salt Lake")
dfw_counties <- c("Collin County", "Dallas", "Denton", 
                  "Ellis", "Hunt", "Kaufman", "Rockwall", 
                  "Johnson", "Parker", "Tarrant", "Wise")
abq_counties <- c("Bernalillo", "Sandoval", "Torrance", "Valencia")
sf_counties <- c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco",
                 "San Mateo", "Santa Clara", "Solano", "Sonoma")

SLC <- block_groups("UT", slc_counties, cb = T)
SLC2 <- geo_join(SLC, covariates, 'COUNTYFP', 'COUNTYFP')
DFW <- block_groups("TX", dfw_counties, cb = T)
DFW <- left_join(DFW, covariates, 'COUNTYFP', 'COUNTYFP')
ABQ <- block_groups("NM", abq_counties, cb = T)
SF <- block_groups("CA", sf_counties, cb = T)

ggplot() + 
  geom_sf(data = SLC2, aes(fill = COUNTYFP), alpha = 0.5) + 
  geom_point(data = subset(tapData, Cluster_Location == "Salt Lake City"), aes(x = Long, y = Lat)) +
  scale_fill_viridis(option = "mako", discrete = T) +
  theme_void()

ggplot() + 
  geom_sf(data = DFW, aes(fill = COUNTYFP), alpha = 0.5) + 
  geom_point(data = subset(tapData, Cluster_Location == "Dallas Fort Worth"), aes(x = Long, y = Lat)) +
  scale_fill_viridis(option = "mako", discrete = T) +
  theme_void()

ggplot() + 
  geom_sf(data = ABQ, aes(fill = COUNTYFP), alpha = 0.5) + 
  geom_point(data = subset(tapData, Cluster_Location == "Albuquerque"), aes(x = Long, y = Lat)) +
  scale_fill_viridis(option = "mako", discrete = T) +
  theme_void()

ggplot() + 
  geom_sf(data = SF, aes(fill = MEDINCOME), alpha = 0.8) + 
  geom_point(data = subset(tapData, Cluster_Location == "San Francisco"), aes(x = Long, y = Lat)) +
  scale_fill_viridis(option = "mako", discrete = T) +
  theme_void()

table(subset(tapData, Cluster_Location == "LaCrosse")$County)

#Okay so we can pull shape files for every county that exists in a target city. That's not too too difficult. 


#######################
##Precipitation Data
#####################

#from PRISM
#While I can map this, I can't pull this data into anything useful. Going to try with another data file option later
#then just ask Gabe.
precipMap <- raster("data/PRISM_rainflow.png")
plot(precipMap, main = "Annual Precipitation last 30 years",
     xlab = "Longitude", ylab= "Latitude", 
     breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 
                1000, 1500, 2000, 3000, 4000, 5000, 6000))

xy <- tapData %>% 
  dplyr::select(Lat, Long)
names(xy) <- c('x','y')


cent_max <- raster::extract(precipMap,             # raster layer
                            xy,   # SPDF with centroids for buffer
                            buffer = 20,     # buffer size, units depend on CRS
 #                           fun = max,         # what to value to extract
                            df = TRUE)         # return a dataframe? 
raster::extract(precipMap, SpatialPoints(xy), sp = T)
r <- cbind(raster::extract(precipMap, xy, df = T),xy)

#Because the lat and long are going to be slightly different across the precipitation data and our tables,
# we can go with assigning precip based off what's closest. 

ggplot() + 
  geom_sf(data = precipMap) + 
  geom_point(data = subset(tapData, Cluster_Location == "San Francisco"), aes(x = Long, y = Lat)) +
#  scale_fill_viridis(option = "mako", discrete = T) +
  theme_void()


# Let's try rasterizing with another shapefile
streamflow <- raster("C:/Users/u6047585/Dropbox/IRB/TapWaterCities Analysis/gis/fa_qs_ann.tif")
plot(streamflow)
#this is laea projection, can we easily change it?
newproj = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
precip <- projectRaster(precipMap, crs=newproj, res=0.1)


raster::extract(streamflow, SpatialPoints(xy), sp = T)

r <- cbind(raster::extract(streamflow, xy, df = T),xy)
