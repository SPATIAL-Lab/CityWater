#Let's try creating shapefiles matching the US Census using tigris, a package which directly pulls from Census data and plays well with tidyverse 
#This assumed G0 has been run. 
library(tigris)
library(viridis)
library(raster)
library(tmap)

#census data is nad83/ EPSG:4269
# We want to use LAEA projection 

#only counties included in our data
slc_counties <- c("Davis", "Salt Lake")
dfw_counties <- c("Collin County", "Dallas", 
                  #"Denton", 
                  "Ellis", 
                  #"Hunt", "Kaufman", "Rockwall", 
                  "Johnson",
                  #"Parker",
                  "Tarrant"
                  #, "Wise"
                  )
abq_counties <- c("Bernalillo"
                  #, "Sandoval", "Torrance", "Valencia"
                  )
sf_counties <- c("Alameda", "Contra Costa", "Marin", 
                 #"Napa",
                 "San Francisco", "San Mateo", "Santa Clara"
                 #, "Solano", "Sonoma"
                 )
SLC <- counties("UT", slc_counties, cb = T, resolution = "20m", year = c("2020"))
SLC <- block_groups("UT", slc_counties, cb = T)
#note: either one gets us the county-based data we want in terms of ALAND, AWATER. 
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

table(subset(tapData, Cluster_Location == "Wooster")$County)

#Okay so we can pull shape files for every county that exists in a target city. That's not too too difficult. 


#######################
##Precipitation Data
#####################

#from PRISM
#While I can map this, I can't pull this data into anything useful. Going to try with another data file option later
#then just ask Gabe.

#xy <- data.frame("x" = tapData$Long, "y" = tapData$Lat)
#read in raster
precipMap <- raster("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")

#make spatial object from xy
xy.sp = SpatialPoints(xy, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#extract
r = raster::extract(precipMap, xy.sp, sp = TRUE)

#values
r@data

#plot
spplot(r)

#creating dataframe of precip data pulled from rastering
result <- raster::extract(r, xy, cellnumbers = T)
r2 <- as.data.frame(r)
precipData <- as.data.frame(cbind(result,coordinates(precipMap)[result[,2],]))
r2 <- rename(r2, 
                     precip = PRISM_ppt_30yr_normal_4kmM3_annual_asc, 
                     Long = x, 
                     Lat = y)

#setting up our data for multivariate regression, and adding the precip data
regressData <- subset(tapData, select = c(Cluster_Location, 
                                      Season, 
                                      Elevation_mabsl, 
                                      Month, 
                                      d2H, 
                                      d18O, 
                                      County, 
                                      State, 
                                      Lat, 
                                      Long))
regressData <- cbind(regressData, r2, by = c("Lat", "Long"))
colnames(regressData)[12] = "x"
colnames(regressData)[13] = "y"



# Let's try rasterizing with another shapefile
streamflow <- raster("C:/Users/u6047585/Dropbox/IRB/TapWaterCities Analysis/gis/fa_qs_ann.tif")
plot(streamflow)
#this is laea projection, can we easily change it?
newproj = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
precip <- projectRaster(precipMap, crs=newproj, res=0.1)


raster::extract(streamflow, SpatialPoints(xy), sp = T)

r <- cbind(raster::extract(streamflow, xy, df = T),xy)
