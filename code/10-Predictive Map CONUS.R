# Predictive map of contiguous US. Note: you'll need a Census API  --------

library(censusapi);library(tigris, options(tigris_use_cache = TRUE)); library(viridis); 
library(ggplot2); library(dplyr); library(terra); library(readxl); 
library(usmap)

datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(1, 3:5, 7:14, 17, 18)]
multivariate <- read.csv("data/multivariate.csv")
multilevel <- left_join(multivariate, datasummary, by = 'cluster_location') %>% 
  rename('idr' = 'IDR_O')

model <- multilevel %>% 
  select(idr, streamflow, lon, medincome)

model$streamflow = log(model$streamflow)

conus <- counties(cb = TRUE)
conus$STATEFP <- as.numeric(conus$STATEFP)
conus <- subset(conus, STATEFP < 60)
conus <- subset(conus, STATEFP != 02)
conus <- subset(conus, STATEFP != 15)

# streamflow
streamflow <- rast("maps/streamflow_mean.tif")
streamflow = log(streamflow)

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
  filter(state < 60, state != '02', state != '15')
acs_simple$GEOID = paste0(acs_simple$state, acs_simple$county)

# Jeff Davis County, Texas has an odd glitch right now with the census, showing median income as -666666666.
# Let's fix that. 
acs_simple$medincome[acs_simple$GEOID == '48243'] <- 38659

conus <- inner_join(conus, acs_simple, by = c("GEOID"))
conus <- project(vect(conus), streamflow)
medincome <- rasterize(conus, streamflow, conus$medincome)

# Longitude
lon = streamflow
lon = project(lon, "WGS84")
lc = crds(lon, na.rm = FALSE)
values(lon) = lc[, 1]
lon = project(lon, streamflow)
lon = mask(lon, streamflow)

# Stack rasters
s <- c(streamflow, lon, medincome)
names(s) <- c("streamflow", "lon", "medincome")

# Fit the model
best_model <- lm(sqrt(idr) ~ streamflow + lon + medincome,
                 data = model)

# Predict
O_pred <- max(predict(s, best_model), min(model$idr))^2

# Plot
st <- vect("maps/cb_2018_us_state_5m.shp")
st <-  project(st, "ESRI:102003")
st <- crop(st, O_pred)
usa = aggregate(st)

#plot outcome
png(filename = "figures/Fig6.png", width = 7, height = 4.8, units = 'in', 
    res = 600)
plot(O_pred, col = viridis(100), mar = c(2, 2, 2, 6), 
     axes = FALSE, box = FALSE)
plot(st, col= NA, border = 'light grey', add = TRUE)
plot(usa, col = NA, add = TRUE, lw = 2)
mtext(expression("Tap water "*delta^{18}*"O IDR"), side = 4, line = 3.5)
dev.off()
