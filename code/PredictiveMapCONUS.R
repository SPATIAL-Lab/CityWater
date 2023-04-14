library(raster); library(censusapi);library(elevatr);
library(tigris, options(tigris_use_cache = TRUE)); library(viridis); 
library(ggplot2); library(dplyr); library(tidyr); library(sf)

# I suspect the first step is to build rasters of the predictive variables. 
# we want all eight variables for d18O

# pulling shapefile from census of counties and paring down to CONUS. 
# ALAND and AWATER in this list
conus <- counties(cb = TRUE)
conus$STATEFP <- as.numeric(conus$STATEFP)
conus <- subset(conus, STATEFP < 60)
conus <- subset(conus, STATEFP != 02)
conus <- subset(conus, STATEFP != 15)
conus$landlog <- log(conus$ALAND)
conus$waterlog <- log(conus$AWATER)
#elevation
elevation <- get_elev_raster(conus, z= 5)

#precipitation 
precip <- raster("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")

# streamflow
streamflow <- raster("data/fa_qs_ann.tif")

# latitude is hopeful part of the projection so that's easy...

# median income
Sys.setenv(CENSUS_KEY = "7d9a4b25e4c9d0cced63abc32010591eac577c4e")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

acs_simple <- getCensus(
  name = "acs/acs5",
  vintage = 2020,
  vars = c("NAME", "B01001_001E", "B19013_001E"),
  region =  "county:*") %>% 
  rename(pop = B01001_001E, 
         medincome = B19013_001E) %>% 
  filter(state < 60, state != '02', state != '15') %>% 
  unite(GEOID, c("state", "county"), sep = '')
# Jeff Davis County, Texas has an odd glitch right now with the census, showing median income as -666666666.
# Let's fix that. 

#acs_simple$medincome[acs_simple$GEOID == '48243'] <- 38659
# quick test of medincome
hist(acs_simple$medincome)

conus <- inner_join(conus, acs_simple, by = c("GEOID"))
conus$popdensity <- conus$pop/(conus$ALAND*0.000001)

# this doesn't matter, but create breaks to plot population density
conus$densitybreaks <- cut(conus$popdensity, breaks = c(0, 1, 10, 25, 100, 500, 1000, Inf), 
                        labels = c("0 - 1", "1.1 - 10", "10.1 - 25",
                                   "25.1 - 100", "100.1 - 500", "500.1 - 1000", "> 1000")
)

ggplot() + 
  geom_sf(data = conus, aes(fill = densitybreaks)) + 
  theme_void() + 
  scale_fill_viridis(discrete = T)

ggplot() + 
  geom_sf(data = conus, aes(fill = medincome)) + 
  theme_void() + 
  scale_fill_viridis(discrete = F)
#okay, so now we have a sf with land area, water area, pop density, and median income
# let's check all the projections are correct. Matching to precip crs
landarea <- rasterize(conus, precip, conus$landlog)
waterarea <- rasterize(conus, precip, conus$waterlog)
medincome <- rasterize(conus, precip, conus$medincome)
popdensity <- rasterize(conus, precip, conus$popdensity)

#crs <- "+proj=longlat +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
# precip <- projectRaster(precip, crs = crs)
# we also have rasters of precip, elevation, and streamflow
streamflow <- projectRaster(streamflow, precip)
extent(streamflow) <- extent(precip)
streamflow2 <- resample(streamflow, precip)
streamflow2 <- mask(streamflow2, precip)

elevation2 <- elevation
elevation2 <- projectRaster(elevation2, precip)
extent(elevation2) <- extent(precip)
elevation2 <- resample(elevation2, precip)
elevation2 <- mask(elevation2, precip)
lat <- init(precip, 'y')
lat <- mask(lat, precip)
# grab latitude as data

#stack rasters
s <- stack(landarea, waterarea, medincome, popdensity, precip, streamflow2, 
           elevation2, lat)
names(s) <- c("landlog", "waterlog", "medincome", "popdensity", "precip", 
              "streamflow", "elevation", "lat")

# okay call the model that we want now
tapData <- read.csv("data/cityWater.csv") 
tapData <- subset(tapData, Cluster_Location != "Oahu" & Cluster_Location != "Hawaii")
datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(1, 3:14, 17, 18)]
multivariate <- read.csv("data/multivariate.csv")
multivariate$landlog <- log(multivariate$total_land)
multivariate$waterlog <- log(multivariate$total_water)

model <- left_join(tapData, multivariate, by = "Cluster_Location") %>% 
select(d18O, d_ex, landlog, waterlog, elevation_range, streamflow, precip, 
         Lat, popdensity, medincome, Elevation) %>% 
  rename(elevation = Elevation, 
         lat = Lat)

best.model <- lm(d18O ~ landlog + waterlog + elevation + streamflow + 
                   precip + popdensity + lat + medincome, data = model)

predictedO_model <- predict(s, best.model, progress='text')

plot(predictedO_model, 
     col = viridis(100))
O_hist <- hist(predictedO_model)
O_hist$breaks
O_hist$counts
#should we cut data above 14 per mill or so?

#therer's one cell that's 20 per mill. It looks to be around New York County NY- 
# how can I zoom in on a raster?
plot(predictedO_model, axes = FALSE, 
     xlim = c(-74.72127 ,-72.75382), ylim = c(39.82881 , 41.11662))

plot(predictedO_model, axes = FALSE, 
     xlim = c(-75.59569, -71.07785), ylim = c(39.04493, 41.62054 ))


NewYorkCounty <- subset(conus, NAMELSAD == "New York County")
#did the demographic factors throw Manhattan way out of whack? 

ggplot()+ 
  geom_point(data = conus, aes(x = popdensity, y = medincome), color = 'bisque', size = 3) + 
  geom_point(data = NewYorkCounty, aes(x = popdensity, y = medincome), color = 'hotpink', size = 3) + 
  theme_classic()

# well we know what to blame- NYCounty. It's the densest part of the US by far, with high medincome
# though these two factors don't do much in the linear model, it still nudged it way up
conus %>%                                      # Top N highest values by group
  arrange(desc(popdensity)) %>% 
  select(NAME.y, popdensity) %>% 
  slice(1:5)


