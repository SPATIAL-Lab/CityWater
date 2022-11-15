#Let's try creating shapefiles matching the US Census using tigris, a package 
#which directly pulls from Census data and plays well with tidyverse 
#This assumes G0 has been run. 
library(tigris, options(tigris_use_cache = TRUE))
library(viridis);library(raster)

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
  st_union() %>%
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

#read in raster
precip <- raster("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
streamflow <- raster("C:/Users/u6047585/Dropbox/IRB/TapWaterCities Analysis/gis/fa_qs_ann.tif")

clusterLocations$streamflow <- raster::extract(streamflow, clusterLocations, weights = F, fun = sum)
clusterLocations$precip <- raster::extract(precip, clusterLocations, weights = F, fun = mean)

#streamflow isn't producing data for large swathes of California or Washington, which is frustrating. 
# Or New Jersey, which is just odd. Poor New Jersey.
multivariate <- as.data.frame(clusterLocations) %>% 
  select(c(streamflow, precip))
multivariate$Cluster_ID <- getSpPPolygonsIDSlots(clusterLocations)


############
# ALAND AND AWATER SUMS
############

ABQ <- counties("NM", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Bernalillo"))%>%
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = 'Albuquerque') 


AA <- counties("MI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Washtenaw"))%>%
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Ann Arbor")

ATH <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clarke","Oconee")) %>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Athens")

ATL <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cobb", "DeKalb", "Fulton"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Atlanta")

BEL <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Whatcom"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Bellingham")

CED <- counties("UT", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Iron")) %>%
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Cedar City")

COL <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("El Paso")) %>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Colorado Springs")

DFW <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Collin County", "Dallas", "Ellis", "Johnson",
                     "Tarrant"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Dallas Fort Worth")

DEN <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Adams", "Arapahoe", "Boulder","Broomfield",
                     "Denver", "Jefferson"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Denver")

FLG <- counties("AZ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Coconino"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Flagstaff")

GNV <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alachua"))%>%
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Gainsville")

HI <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hawaii"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Hawaii")

LCR <- counties("WI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Houston", "La Crosse", "Winona"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "LaCrosse")

LAW <- counties("KS", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Douglas"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Lawrence")

LAX <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Los Angeles", "Riverside",
                     "San Bernardino","San Diego"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Los Angeles")

MSP <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Anoka", "Hennepin", "Ramsey"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Minneapolis")

MOR <- counties("NJ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Essex", "Morris", "Somerset", "Union"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Morristown")

NAS <- counties("TN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cheatham", "Davidson", "Rutherford", "Williamson"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Nashville")

OA <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Honolulu"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Oahu")

PHX <- counties("AZ", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Maricopa", "Pinal")) %>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Phoenix")

PTD <- counties("OR", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Clackmas", "Clark", "Multnomah"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Portland")

SLC <- counties("UT", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Salt Lake", "Davis")) %>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Salt Lake City")

SD <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("San Diego")) %>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "San Diego")

SF <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alameda","Contra Costa", "Marin", "San Francisco",
                     "San Mateo","Santa Clara")) %>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "San Francisco")

SM <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hays"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "San Mateo")

SP <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Pinellas"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "St Petersburg")

SC <- counties("PA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Centre"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "State College")

WOO <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Wayne"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Wooster")

clusterLocations <- rbind(SLC, DFW, ABQ, SF, AA, ATH, ATL, BEL, CED, COL, 
                         DEN, FLG, GNV, LCR, LAW, LAX, MSP, MOR, NAS, PHX,
                         PTD, SD, SM, SP, SC, WOO)
multivariate <- left_join(multivariate, clusterLocations, by = 'Cluster_ID')


################
# CENSUS DATA
################

# tidycensus doesn't do income, only population by race. So, off we go
library(censusapi)
Sys.setenv(CENSUS_KEY = "7d9a4b25e4c9d0cced63abc32010591eac577c4e")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

apis <- listCensusApis()
apis <- subset(apis, temporal == '2020/2020')


acs_simple <- getCensus(
  name = "acs/acs5",
  vintage = 2020,
  vars = c("NAME", "B01001_001E", "B19013_001E"),
  region =  "county:*") %>% 
  rename(pop = B01001_001E, 
         medincome = B19013_001E) %>% 
  mutate(GEOID = str)
  


#K202302_001E median income?

##############
#GRAVEYARD 
#################
library(tidycensus)
census_api_key("7d9a4b25e4c9d0cced63abc32010591eac577c4e", install = TRUE)
readRenviron("~/.Renviron")

decennial_2020_vars <- load_variables(
  year = 2020, 
  "pl", 
  cache = TRUE
)

df <- load_variables(2020, "pl", cache = T)

desired_vars = c(
  all = "P2_001N",
  hisp = "P2_002N",
  white = "P2_005N",
  baa = "P2_006N",
  amin = "P2_007N",
  asian = "P2_008N",
  nhopi = "P2_009N",
  other = "P2_010N",
  multi = "P2_011N"
)

census_data = get_decennial(
  geography = "county",
  variables = desired_vars, 
  summary_var = "P2_001N", 
  year = 2020,
  sumfile = "pl"
)

census_data = get_decennial(
  geography = "county", 
  variables = K2_2302, 
  year = 2020, 
  sumfile = 'pl'
)
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
