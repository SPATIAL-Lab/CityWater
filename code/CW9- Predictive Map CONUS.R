library(raster); library(censusapi);library(elevatr);
library(tigris, options(tigris_use_cache = TRUE)); library(viridis); 
library(ggplot2); library(dplyr); library(tidyr); library(sf); library(terra); library(readxl)

conus <- counties(cb = TRUE)
conus$STATEFP <- as.numeric(conus$STATEFP)
conus <- subset(conus, STATEFP < 60)
conus <- subset(conus, STATEFP != 02)
conus <- subset(conus, STATEFP != 15)
conus$total_area <- (conus$ALAND + conus$AWATER)*0.000001
conus$perc_water <- round(((conus$AWATER*0.000001)/conus$total_area)*100, 2)

#precipitation 
precip <- raster("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")

# streamflow
streamflow <- raster("data/fa_qs_ann.tif")

# latitude is hopefully part of the projection so that's easy...

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

acs_simple$medincome[acs_simple$GEOID == '48243'] <- 38659
# quick test of medincome
hist(acs_simple$medincome)

conus <- inner_join(conus, acs_simple, by = c("GEOID"))
conus$popdensity <- conus$pop/(conus$ALAND*0.000001)


# water use
water <- read_excel("data/water.xlsx")%>% 
  rename(GEOID = FIPS, 
         water_use = 'TO-WFrTo')
conus <- inner_join(conus, water, by = c("GEOID"))
water_use <- rasterize(conus, precip, conus$water_use)
#okay, so now we have a sf with land area, water area, pop density, and median income
# let's check all the projections are correct. Matching to precip crs
total_area <- rasterize(conus, precip, conus$total_area)
perc_water <- rasterize(conus, precip, conus$perc_water)
medincome <- rasterize(conus, precip, conus$medincome)
popdensity <- rasterize(conus, precip, conus$popdensity)

#crs <- "+proj=longlat +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
# precip <- projectRaster(precip, crs = crs)
# we also have rasters of precip, elevation, and streamflow
streamflow <- projectRaster(streamflow, precip)
extent(streamflow) <- extent(precip)
streamflow2 <- resample(streamflow, precip)
streamflow2 <- mask(streamflow2, precip)

lat <- init(precip, 'y')# grab latitude as data
lat <- mask(lat, precip)

#stack rasters
s <- stack(total_area, perc_water, medincome, popdensity, precip, streamflow2, lat, water_use)
names(s) <- c("total_area", "perc_water", "medincome", "popdensity", "precip", 
              "streamflow", "lat", "water_use")

# okay call the model that we want now
datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(1, 3:5, 7:14, 17, 18)]
multivariate <- read.csv("data/multivariate.csv")
multilevel <- left_join(multivariate, datasummary, by = 'Cluster_Location') %>% 
  rename('idr' = 'IDR_O')

model <- multilevel %>% 
  dplyr::select(idr, total_area, perc_water, streamflow, popdensity, medincome)

best_model <- lm(sqrt(idr) ~ ., data = model)

predictedO_model <- predict(s, best_model)^2

predproj = rast(predictedO_model)
predproj = project(predproj, "EPSG:5070")

plot(min(predproj, 10), 
     col = viridis(100), 
     axes = F, 
     box = F)

# what about a sensical model? 
sensical <- multilevel %>% 
  dplyr::select(idr, streamflow, precip, perc_water,
                popdensity, medincome, water_use)
sensical_model <- lm(sqrt(idr) ~ ., data = sensical)

predictedO_model <- predict(s, sensical_model)^2

predproj = rast(predictedO_model)
predproj = project(predproj, "EPSG:5070")

plot(min(predproj, 10), 
     col = viridis(100), 
     axes = F, 
     box = F)

O_hist <- hist(predictedO_model)
O_hist$breaks
O_hist$counts