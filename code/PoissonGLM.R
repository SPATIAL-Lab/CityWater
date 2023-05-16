# To combat the negative estimated values, what if we try a Poisson model? 


# Setup -------------------------------------------------------------------

library(raster); library(censusapi);library(elevatr);
library(tigris, options(tigris_use_cache = TRUE)); library(viridis); 
library(ggplot2); library(dplyr); library(tidyr); library(sf)
#These are specific for Poisson it seems
library(msm); library(sandwich)

# I suspect the first step is to build rasters of the predictive variables. 
# we want all eight variables for d18O

# pulling shapefile from census of counties and paring down to CONUS. 
# ALAND and AWATER in this list
conus <- counties(cb = TRUE)
conus$STATEFP <- as.numeric(conus$STATEFP)
conus <- subset(conus, STATEFP < 60)
conus <- subset(conus, STATEFP != 02)
conus <- subset(conus, STATEFP != 15)
conus$total_area <- (conus$ALAND + conus$AWATER)*0.000001
conus$perc_water <- round(((conus$AWATER*0.000001)/conus$total_area)*100, 2)

#elevation
elevation <- get_elev_raster(conus, z= 5)

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

elevation2 <- elevation
elevation2 <- projectRaster(elevation2, precip)
extent(elevation2) <- extent(precip)
elevation2 <- resample(elevation2, precip)
elevation2 <- mask(elevation2, precip)
lat <- init(precip, 'y')# grab latitude as data
lat <- mask(lat, precip)

#stack rasters
s <- stack(total_area, perc_water, medincome, popdensity, precip, streamflow2, 
           elevation2, lat)
names(s) <- c("total_area", "perc_water", "medincome", "popdensity", "precip", 
              "streamflow", "elevation", "lat")

# okay call the model that we want now
tapData <- read.csv("data/cityWater.csv") 
tapData <- subset(tapData, Cluster_Location != "Oahu" & Cluster_Location != "Hawaii")
datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(1, 3:5, 7:14, 17, 18)]
multivariate <- read.csv("data/multivariate.csv")
multilevel <- left_join(multivariate, datasummary, by = 'Cluster_Location') %>% 
  rename('sd' = 'd18O_sd')

model <- left_join(tapData, multilevel, by = "Cluster_Location") %>% 
  dplyr::select(sd, total_area, perc_water, elevation_range, streamflow, precip, 
                Lat, popdensity, medincome, Elevation) %>% 
  rename(elevation = Elevation, lat = Lat)


# Poisson -----------------------------------------------------------------
#have to round sd to whole number (make it a count)
model2 <- model
model2$sd <- round(model2$sd, digits = 0)
m2 <- glm(sd ~ total_area + perc_water + elevation + streamflow + 
            precip + popdensity + lat + medincome, data = model2, family="poisson")

summary(m2)

predictedO_model <- predict(s, m2)

plot(predictedO_model, 
     col = viridis(100), 
     axes = F, 
     box = F)
# Okay now this is almost all negative...
with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))


model2$predictedsd <- predict(m1, type="response")

