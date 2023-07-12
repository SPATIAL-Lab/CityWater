# what if we look at water usage by county instead of total water (AWATER) from the Census? 

library(raster); library(censusapi);library(tigris, options(tigris_use_cache = TRUE)); 
library(viridis); library(ggplot2); library(dplyr); library(tidyr); library(sf); 
library(readxl)

# data from https://www.sciencebase.gov/catalog/item/get/5af3311be4b0da30c1b245d8, lightly cleaned
water <- read_excel("data/water.xlsx")

# We've got county FIPS codes concatenated with states so this will be easy to bind to census data

#TO-Wtotl (Total withdrawals, total (fresh+saline), in Mgal/d)
#DO-WDelv (Domestic, total use (withdrawals + deliveries), in Mgal/d)
#IN-Wtotl (Industrial, self-supplied total withdrawals, total (fresh+saline), in Mgal/d)
#IC-CUsFr (Irrigation-Crop, total consumptive use, fresh, in Mgal/d)
#IG-WFrTo (Irrigation-Golf, total withdrawals, fresh, in Mgal/d)
#LI-WFrTo (Livestock, total withdrawals, fresh, in Mgal/d)
#AQ-WTotl (Aquaculture, total withdrawals, total (fresh+saline), in Mgal/d)
#MI-Wtotl (Mining, total withdrawals, total (fresh+saline), in Mgal/d)
#TO-WGWFr (Total groundwater withdrawals, fresh, in Mgal/d)
#TO-WSWFr (Total surface-water withdrawals, fresh, in Mgal/d)

#Some setup for other demographic comparisons
conus <- counties(cb = TRUE)
conus$STATEFP <- as.numeric(conus$STATEFP)
conus <- subset(conus, STATEFP < 60)
conus <- subset(conus, STATEFP != 02)
conus <- subset(conus, STATEFP != 15)
conus$total_area <- (conus$ALAND + conus$AWATER)*0.000001
conus$perc_water <- round(((conus$AWATER*0.000001)/conus$total_area)*100, 2)

Sys.setenv(CENSUS_KEY = "7d9a4b25e4c9d0cced63abc32010591eac577c4e")
# Reload .Renviron
readRenviron("~/.Renviron")
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

conus <- inner_join(conus, acs_simple, by = c("GEOID"))
conus$popdensity <- conus$pop/(conus$ALAND*0.000001)

df <- water %>% 
  rename(GEOID = FIPS, 
         total_withdrawals = 'TO-Wtotl', 
         domestic_total = 'DO-WDelv',
         industrial_total = 'IN-Wtotl',
         crop_total= 'IC-CUsFr', 
         golf_total = 'IG-WFrTo', 
         livestock_total = 'LI-WFrTo', 
         aquaculture_total = 'AQ-Wtotl', 
         mining_total = 'MI-Wtotl', 
         groundwater_total = 'TO-WGWFr', 
         surfacewater_total = 'TO-WSWFr', 
         fresh_total = 'TO-WFrTo') 

df <- df %>% 
  select(GEOID, total_withdrawals, domestic_total, 
         industrial_total,
         crop_total,
         golf_total,
         livestock_total,
         aquaculture_total,
         mining_total,
         groundwater_total,
         surfacewater_total, 
         fresh_total
           )

df <- inner_join(conus, df, by = c("GEOID"))

ggplot() + 
  geom_sf(data = df, aes(fill = total_withdrawals)) + 
  theme_void() + 
  scale_fill_viridis()

ggplot() + 
  geom_sf(data = df, aes(fill = domestic_total)) + 
  theme_void() + 
  scale_fill_viridis(discrete = F)

ggplot() + 
  geom_sf(data = df, aes(fill = crop_total)) + 
  theme_void() + 
  scale_fill_viridis(discrete = F)

ggplot() + 
  geom_sf(data = df, aes(fill = golf_total)) + 
  theme_void() + 
  scale_fill_viridis(discrete = F)

#converting use to liters per day, per person
df$wateruse_capita <- (df$domestic_total*3785412)/df$pop
df$wateruse_capitabreaks <- cut(df$wateruse_capita, breaks = c(0, 100, 200, 400, 600, 800, Inf), 
                           labels = c("0 - 100", "100 - 200", "200 - 400",
                                      "401 - 600", "600 - 800", "> 800")
)

ggplot() + 
  geom_sf(data = df, aes(fill = wateruse_capitabreaks)) + 
  theme_void() + 
  scale_fill_viridis(discrete = T) + 
  labs(fill = "Water use per person (L/day)") +
  theme(legend.position = "top")


# irrigation use to 1M liters per day
df$irrigation <- (df$crop_total*3.785412)
waterhist <- hist(df$irrigation)
waterhist$breaks
waterhist$counts
df$irrigation_breaks <- cut(df$irrigation, breaks = c(0, 50, 100, 200, 400, 600, Inf), 
                                labels = c("0 - 50", "50 - 100", "101 - 200", "201 - 400",
                                           "401 - 600", "> 600"))
ggplot() + 
  geom_sf(data = df, aes(fill = irrigation_breaks)) + 
  theme_void() + 
  scale_fill_viridis(discrete = T) + 
  labs(fill = "Water use (1M L/day)") +
  theme(legend.position = "top")


ggplot() + 
  geom_sf(data = df, aes(fill = wateruse_capita)) + 
  theme_void() + 
  scale_fill_viridis(discrete = F)

ggplot() + 
  geom_sf(data = df, aes(fill = fresh_total)) + 
  theme_void() + 
  scale_fill_viridis(discrete = F)

hist(df$fresh_total)

#add this data to the urban area info? 
multivariate <- read_csv("data/multivariate.csv")

multivariate$water_use <- NA
multivariate$water_use[multivariate$Cluster_Location == "Ann Arbor"] <- subset(df, GEOID == "26161")$fresh_total
multivariate$water_use[multivariate$Cluster_Location == "Albuquerque"] <- subset(df, GEOID == "35001")$fresh_total
multivariate$water_use[multivariate$Cluster_Location == "Athens"] <- sum(subset(df, 
                                                                             GEOID == "13219" |
                                                                             GEOID == "13059")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "Atlanta"] <- sum(subset(df, 
                                                                              GEOID == "13121" |
                                                                              GEOID == "13089" | 
                                                                              GEOID == "13067")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "Bellingham"] <- subset(df, GEOID == "53073")$fresh_total
multivariate$water_use[multivariate$Cluster_Location == "Cedar City"] <- subset(df, GEOID == "49021")$fresh_total
multivariate$water_use[multivariate$Cluster_Location == "Colorado Springs"] <- subset(df, GEOID == "08041")$fresh_total
multivariate$water_use[multivariate$Cluster_Location == "Denver"] <- sum(subset(df, 
                                                                             GEOID == "08005" |
                                                                             GEOID == "08001" | 
                                                                             GEOID == "08013" |
                                                                             GEOID == "08014" |
                                                                             GEOID == "08059" |
                                                                             GEOID == "08031")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "Dallas Fort Worth"] <- sum(subset(df, 
                                                                                        GEOID == "48085" |
                                                                                        GEOID == "48113" | 
                                                                                        GEOID == "48139" |
                                                                                        GEOID == "48251" |
                                                                                        GEOID == "48439")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "Flagstaff"] <- subset(df, GEOID == "04005")$fresh_total
multivariate$water_use[multivariate$Cluster_Location == "Gainesville"] <- subset(df, GEOID == "12001")$fresh_total
multivariate$water_use[multivariate$Cluster_Location == "Lawrence"] <- subset(df, GEOID == "20045")$fresh_total
multivariate$water_use[multivariate$Cluster_Location == "Los Angeles"] <- sum(subset(df, 
                                                                                     GEOID == "06037" |
                                                                                     GEOID == "06065" | 
                                                                                     GEOID == "06071" |
                                                                                     GEOID == "06073")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "La Crosse"] <- sum(subset(df, 
                                                                                   GEOID == "55063" |
                                                                                   GEOID == "27055" | 
                                                                                   GEOID == "27169")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "Morristown"] <- sum(subset(df, 
                                                                                    GEOID == "34013" |
                                                                                    GEOID == "34027" | 
                                                                                    GEOID == "34035" |
                                                                                    GEOID == "34039")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "Minneapolis"] <- sum(subset(df, 
                                                                                      GEOID == "27003" |
                                                                                      GEOID == "27053" | 
                                                                                      GEOID == "27123")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "Nashville"] <- sum(subset(df, 
                                                                                   GEOID == "47021" |
                                                                                   GEOID == "47037" | 
                                                                                   GEOID == "47149" |
                                                                                   GEOID == "47187")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "Phoenix"] <- sum(subset(df, 
                                                                                 GEOID == "04013" |GEOID == "04021")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "Portland"] <- sum(subset(df, 
                                                                                  GEOID == "41005" |
                                                                                  GEOID == "53011" | 
                                                                                  GEOID == "27123")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "State College"] <- subset(df, GEOID == "42027")$fresh_total
multivariate$water_use[multivariate$Cluster_Location == "San Diego"] <- subset(df, GEOID == "06073")$fresh_total
multivariate$water_use[multivariate$Cluster_Location == "San Francisco"] <- sum(subset(df, 
                                                                                       GEOID == "06001" |
                                                                                       GEOID == "06013" | 
                                                                                       GEOID == "06041" |
                                                                                       GEOID == "06075" | 
                                                                                       GEOID == "06081" | 
                                                                                       GEOID == "06085")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "Salt Lake City"] <- sum(subset(df, 
                                                                                        GEOID == "49011" |
                                                                                        GEOID == "49035")$fresh_total)
multivariate$water_use[multivariate$Cluster_Location == "San Marcos"] <- subset(df, GEOID == "48209")$fresh_total
multivariate$water_use[multivariate$Cluster_Location == "St Petersburg"] <- subset(df, GEOID == "12103")$fresh_total
multivariate$water_use[multivariate$Cluster_Location == "Wooster"] <- subset(df, GEOID == "39169")$fresh_total

write.csv(multivariate, "data/multivariate.csv")


# Predictive Map with water use -------------------------------------------

library(raster); library(censusapi);library(elevatr);
library(tigris, options(tigris_use_cache = TRUE)); library(viridis); 
library(ggplot2); library(dplyr); library(tidyr); library(sf)


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

water <- read_excel("data/water.xlsx")
water <- water %>% 
  rename(GEOID = FIPS, 
         water_use = 'TO-WFrTo') 

water <- water %>% 
  select(GEOID, water_use)
  
conus <- inner_join(conus, water, by = c("GEOID"))
#okay, so now we have a sf with land area, water area, pop density, and median income
# let's check all the projections are correct. Matching to precip crs
total_area <- rasterize(conus, precip, conus$total_area)
water_use <- rasterize(conus, precip, conus$water_use)
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
s <- stack(total_area, water_use, medincome, popdensity, precip, streamflow2, lat)
names(s) <- c("total_area", "water_use", "medincome", "popdensity", "precip", 
              "streamflow", "lat")

# okay call the model that we want now
tapData <- read.csv("data/cityWater.csv") 
tapData <- subset(tapData, Cluster_Location != "Oahu" & Cluster_Location != "Hawaii")
datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(1, 3:5, 7:14, 17, 18)]
multivariate <- read.csv("data/multivariate.csv")
multilevel <- left_join(multivariate, datasummary, by = 'Cluster_Location') %>% 
  rename('idr' = 'IDR_O')

model <- multilevel %>% 
  dplyr::select(idr, total_area, water_use, streamflow, 
                popdensity, medincome)

best_model <- lm(sqrt(idr) ~ total_area + water_use + streamflow + 
                   popdensity + medincome, data = model)

predictedO_model <- predict(s, best_model)^2

library(terra)
predproj = rast(predictedO_model)
predproj = project(predproj, "EPSG:5070")

plot(min(predproj, 10), 
     col = viridis(100), 
     axes = F, 
     box = F)

O_hist <- hist(predictedO_model)
O_hist$breaks
O_hist$counts

modeldf <- as.data.frame(predictedO_model, xy=TRUE) %>% 
  rename(idr = layer)

# New York County being odd
plot(predictedO_model, axes = FALSE, 
     xlim = c(-74.72127 ,-72.75382), ylim = c(39.82881 , 41.11662))

plot(predictedO_model, axes = FALSE, 
     xlim = c(-75.59569, -71.07785), ylim = c(39.04493, 41.62054), 
     col = viridis(100))


NewYorkCounty <- subset(conus, NAMELSAD == "New York County")
#did the demographic factors throw Manhattan way out of whack? 

ggplot()+ 
  geom_point(data = conus, aes(x = popdensity, y = medincome), color = 'bisque', size = 3) + 
  geom_point(data = NewYorkCounty, aes(x = popdensity, y = medincome), color = '#003f5c', size = 3) + 
  theme_classic()

# well we know what to blame- NYCounty. It's the densest part of the US by far, with high medincome
# though these two factors don't do much in the linear model, it still nudged it way up
conus %>%                                      # Top N highest values by group
  arrange(desc(popdensity)) %>% 
  select(NAME.y, popdensity) %>% 
  slice(1:5)

subset <- predictedO_model
subset[subset<100] <- NA
O_hist <- hist(subset)
O_hist$breaks
O_hist$counts
plot(subset, 
     col = viridis(100), 
     axes = F, 
     box = F)
# New York is still the hilarious outlier even with sqrt, good to know. 


