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

#okay, so now we have a sf with land area, water area, pop density, and median income
# let's check all the projections are correct. Matching to precip crs
landarea <- rasterize(conus, precip, conus$ALAND)


crs <- "+proj=longlat +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
"
precip <- projectRaster(precip, crs = crs)
# we also have rasters of precip, elevation, and streamflow
r <- raster(ncol=1633, nrow = 697)



streamflow <- projectRaster(streamflow, precip)
extent(streamflow) <- extent(precip)
streamflow2 <- resample(streamflow, precip)
streamflow2 <- mask(streamflow2, precip)

elevation2 <- elevation
elevation2 <- projectRaster(elevation2, precip)
extent(elevation2) <- extent(precip)
elevation2 <- resample(elevation2, precip)
elevation2 <- mask(elevation2, precip)


waterarea <- rasterize(conus, precip, conus$AWATER)
medincome <- rasterize(conus, precip, conus$medincome)
popdensity <- rasterize(conus, precip, conus$popdensity)
# grab latitude as data

#stack rasters
s <- stack(precip, streamflow2, elevation2)
