# Predictive map of contiguous US. Note: you'll need a Census API  --------

library(censusapi);library(tigris, options(tigris_use_cache = TRUE)); library(viridis); 
library(ggplot2); library(dplyr); library(tidyr); library(terra); library(readxl); 
library(usmap)

datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(1, 3:5, 7:14, 17, 18)]
multivariate <- read.csv("data/multivariate.csv")
multilevel <- left_join(multivariate, datasummary, by = 'cluster_location') %>% 
  rename('idr' = 'IDR_O')

model <- multilevel %>% 
  dplyr::select(idr, streamflow, medincome, water_use,
                  ruggedness)
model$precip_pop = multilevel$precip / multilevel$pop
model$sf_pop = multilevel$streamflow / multilevel$pop

conus <- counties(cb = TRUE)
conus$STATEFP <- as.numeric(conus$STATEFP)
conus <- subset(conus, STATEFP < 60)
conus <- subset(conus, STATEFP != 02)
conus <- subset(conus, STATEFP != 15)
conus$total_area <- (conus$ALAND + conus$AWATER)*0.000001
conus$perc_water <- round(((conus$AWATER*0.000001)/conus$total_area)*100, 2)

#precipitation 
precip <- rast("maps/precip_mean.tif")

# streamflow
streamflow <- rast("maps/streamflow_mean.tif")

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
vect_conus <- project(vect(conus), precip)

total_area <- rasterize(vect_conus, precip, vect_conus$total_area)
water_use <- rasterize(vect_conus, precip, vect_conus$water_use)
perc_water <- rasterize(vect_conus, precip, vect_conus$perc_water)
medincome <- rasterize(vect_conus, precip, vect_conus$medincome)
popdensity <- rasterize(vect_conus, precip, vect_conus$popdensity)
pop <- rasterize(vect_conus, precip, vect_conus$pop)

# we also have rasters of precip, elevation, and streamflow
streamflow <- project(streamflow, precip)

lat <- init(precip, 'y')# grab latitude as data
lat <- mask(lat, precip)

ruggedness <- mask(project(rast("maps/elev_diff.tif"), precip), precip)
precip_pop = precip/pop
sf_pop = streamflow/pop

#stack rasters
s <- c(streamflow, medincome, water_use,
         ruggedness,precip_pop,sf_pop)
names(s) <- c("streamflow", "medincome", "water_use", 
              "ruggedness", "precip_pop", "sf_pop")

# okay call the model that we want now


best_model <- lm(sqrt(idr) ~ ., data = model)

predictedO_model <- predict(s, best_model)^2

st <- vect("maps/cb_2018_us_state_5m.shp")
st <- project(st, precip)
st <- crop(st, precip)
st <-  terra::project(st, "ESRI:102003")

predictedO_model <- project(predictedO_model, "ESRI:102003")

tiff(filename = "figures/Fig5.tif", width = 600, height = 480, units = 'px', compression = c('lzw'))

terra::plot(min(predictedO_model, 10), 
     col = viridis(100), 
     axes = F, 
     box = F)
terra::plot(st, col= NA, border = 'white', add = T)
#north(type = 2, label = '', xy = 'bottomleft') #testing north and scale
#sbar(500, 'bottomleft', type="bar", below="km", label=c(0,250,500), cex=.8)
dev.off()



