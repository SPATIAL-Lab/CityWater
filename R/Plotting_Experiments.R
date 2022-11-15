#Let's try creating shapefiles matching the US Census using tigris, a package which directly pulls from Census data and plays well with tidyverse 
#This assumed G0 has been run. 
library(tigris, options(tigris_use_cache = TRUE))
library(viridis)
library(raster)

#census data is nad83/ EPSG:4269
# We want to use LAEA projection 
#using only those counties we have data for
ABQ <- counties("NM", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Bernalillo"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Albuquerque")

AA <- counties("MI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Washtenaw"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Ann Arbor")

ATH <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clarke","Oconee")) %>% 
  st_union() %>% 
  as_Spatial(IDs = "Athens")

ATL <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cobb", "DeKalb", "Fulton"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Atlanta")

BEL <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Whatcom"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Bellingham")

CED <- counties("UT", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Iron")) %>% 
  st_union() %>% 
  as_Spatial(IDs = "Cedar City")

COL <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("El Paso")) %>% 
  st_union() %>% 
  as_Spatial(IDs = "Colorado Springs")

DFW <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Collin County", "Dallas", "Ellis", "Johnson",
                     "Tarrant"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Dallas Fort Worth")

DEN <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Adams", "Arapahoe", "Boulder","Broomfield",
                     "Denver", "Jefferson"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Denver")

FLG <- counties("AZ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Coconino"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Flagstaff")

GNV <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alachua"))%>%
  st_union() %>% 
  as_Spatial(IDs = "Gainsville")

HI <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hawaii"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Hawaii")

LCR <- counties("WI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Houston", "La Crosse", "Winona"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "LaCrosse")

LAW <- counties("KS", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Douglas"))%>% 
  as_Spatial(IDs = "Lawrence")

LAX <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Los Angeles", "Riverside",
                     "San Bernardino","San Diego"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Los Angeles")

MSP <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Anoka", "Hennepin", "Ramsey"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Minneapolis")

MOR <- counties("NJ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Essex", "Morris", "Somerset", "Union"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Morristown")

NAS <- counties("TN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cheatham", "Davidson", "Rutherford", "Williamson"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Nashville")

OA <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Honolulu"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Oahu")

PHX <- counties("AZ", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Maricopa", "Pinal")) %>% 
  st_union() %>% 
  as_Spatial(IDs = "Phoenix")

PTD <- counties("OR", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Clackmas", "Clark", "Multnomah"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Portland")

SLC <- counties("UT", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Salt Lake", "Davis")) %>% 
  st_union() %>% 
  as_Spatial(IDs = "Salt Lake City")

SD <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("San Diego")) %>% 
  st_union() %>% 
  as_Spatial(IDs = "San Diego")

SF <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alameda","Contra Costa", "Marin", "San Francisco",
                       "San Mateo","Santa Clara")) %>% 
  st_union() %>% 
  as_Spatial(IDs = "San Francisco")

SM <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hays"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "San Mateo")

SP <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Pinellas"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "St Petersburg")

SC <- counties("PA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Centre"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "State College")

WOO <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Wayne"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Wooster")

clusterLocations <- bind(SLC, DFW, ABQ, SF, AA, ATH, ATL, BEL, CED, COL, 
                          DEN, FLG, GNV, LCR, LAW, LAX, MSP, MOR, NAS, PHX,
                          PTD, SD, SM, SP, SC, WOO, keepnames = T)
#removing one of the San Diego Counties
clusterLocations <- clusterLocations[!duplicated(clusterLocations), ]

#######################
##Precipitation Data
#####################

#from PRISM
#While I can map this, I can't pull this data into anything useful. Going to try with another data file option later
#then just ask Gabe.
library(tidyverse);library(readxl);library(sp);library(maps);library(maptools);
library(raster)
#read in raster
precip <- raster("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
#make spatial object from xy
#xy.sp = SpatialPoints(xy, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#######################
##Streamflow
#####################
# Let's try rasterizing with another shapefile
streamflow <- raster("C:/Users/u6047585/Dropbox/IRB/TapWaterCities Analysis/gis/fa_qs_ann.tif")
plot(streamflow)


clusterLocations$streamflow <- raster::extract(streamflow, clusterLocations, weights = F, fun = sum)
clusterLocations$precip <- raster::extract(precip, clusterLocations, weights = F, fun = mean)

#streamflow isn't producing data for large swathes of California or Washington, which is frustrating. 
# Or New Jersey, which is just odd. Poor New Jersey.
ggplot() + 
  geom_sf(data = ATH, aes(fill = precip)) + 
  theme_void()


##############
#GRAVEYARD 
#################
#below are tracts, but we want counties
slc_counties <- c("Davis", "Salt Lake")
dfw_counties <- c("Collin County", "Dallas", 
                  "Ellis", "Johnson", "Tarrant")
sf_counties <- c("Alameda", "Contra Costa", "Marin", "San Francisco",
                 "San Mateo", "Santa Clara")
ABQ <- block_groups("NM", "Bernalillo", cb = T)
AA <- block_groups("MI", "Washtenaw", cb = T)
ATH <- block_groups("GA", c("Clarke", "Oconee"), cb = T)
ATL <- block_groups("GA", c("Cobb", "DeKalb", "Fulton"), cb = T)
BEL <- block_groups("WA", "Whatcom", cb = T)
CED <- block_groups("UT", "Iron", cb = T)
COL <- block_groups("CO", "El Paso", cb = T)
DFW <- block_groups("TX", dfw_counties, cb = T)
DEN <- block_groups("CO", c("Adams", "Arapahoe", "Boulder", "Broomfield",
                            "Denver", "Jefferson"), cb = T)
FLG <- block_groups("AZ", "Coconino", cb = T)  
GNV <- block_groups("FL", "Alachua", cb = T)
HI <- block_groups("HI", "Hawaii", cb = T)
LCR <- block_groups("WI", c("Houston", "La Crosse", "Winona"), cb = T)
LAW <- block_groups("KS", "Douglas", cb = T)
#LAX counties are huge please enjoy. 
LAX <- block_groups("CA", c("Los Angeles", "Riverside", "San Bernardino", "San Diego"), cb = T)
MSP <- block_groups("MN", c("Anoka", "Hennepin", "Ramsey"), cb = T)
MOR <- block_groups("NJ", c("Essex", "Morris", "Somerset", "Union"), cb = T)
NAS <- block_groups("TN", c("Cheatham", "Davidson", "Rutherford", 
                            "Williamson"), cb = T)
OA <- block_groups("HI", "Honolulu", cb = T)
PHX <- block_groups("AZ", c("Maricopa", "Pinal"), cb = T)
PTD <- block_groups("OR", c("Clackamas", "Clark", "Multnomah"), cb = T)
SLC <- block_groups("UT", slc_counties, cb = T)
SD <- block_groups("CA", "San Diego", cb = T)
SF <- block_groups("CA", sf_counties, cb = T)
SM <- block_groups("TX", "Hays", cb = T)
SP <- block_groups("FL", "Pinellas", cb = T)
SC <- block_groups("PA", "Centre", cb = T)
WOO <- block_groups("OH", "Wayne", cb = T)
#This gives me the data for each point in tapData, rather than by county
#extract
r = raster::extract(precipMap, xy.sp, sp = TRUE)
#plot
spplot(r)
precipData <- as.data.frame(r)
precipData <- rename(precipData, 
                     precip = PRISM_ppt_30yr_normal_4kmM3_annual_asc)
multilevel <- cbind(precipData, multilevel)

#checking that the coordinates from precipData continue to match Lat/Long from tapData
plot(multilevel$x, multilevel$Long)
plot(multilevel$y, multilevel$Lat)

multilevel <- multilevel %>% 
  select(-c(x, y))

streamflow <- projectRaster(streamflow, crs=projection(xy.sp), res=0.1)
#extracts by datapoint. BUT, we want it by county. 
r = raster::extract(streamflow, xy.sp, sp = TRUE)
#plot
spplot(r)

streamflowData <- as.data.frame(r)
streamflowData <- rename(streamflowData, 
                         streamflow = layer)
multilevel <- cbind(streamflowData, multilevel)
plot(multilevel$x, multilevel$Long)
plot(multilevel$y, multilevel$Lat)
multilevel <- multilevel %>% 
  select(-c(x, y))

#Leaving out Hawaii. This does have the potentially unfortunate outcome of having two 
# San Diego Counties
clusterLocations <- rbind(SLC, DFW, ABQ, SF, AA, ATH, ATL, BEL, CED, COL, 
                          DEN, FLG, GNV, LCR, LAW, LAX, MSP, MOR, NAS, PHX,
                          PTD, SD, SM, SP, SC, WOO)
#removing one of the San Diego Counties
clusterLocations <- clusterLocations[!duplicated(clusterLocations), ]
