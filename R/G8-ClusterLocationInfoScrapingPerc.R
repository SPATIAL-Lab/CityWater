#Let's try creating shapefiles matching the US Census using tigris, a package 
#which directly pulls from Census data and plays well with tidyverse 
#This assumes G0 has been run. 
library(tidyverse);library(tigris, options(tigris_use_cache = TRUE))
library(viridis);library(raster); library(sf);library(maps);library(maptools); 
library(censusapi); library(elevatr); library(rgeos)

#census data is nad83/ EPSG:4269
# We want to use LAEA projection 
############
# ALAND AND AWATER SUMS
############
AA <- counties("MI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Washtenaw"))%>%
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Ann Arbor")
AA$total_area = (AA$total_land + AA$total_water)*0.000001

ABQ <- counties("NM", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Bernalillo"))%>%
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = 'Albuquerque')
ABQ$total_area = (ABQ$total_land + ABQ$total_water)*0.000001

ATH <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clarke","Oconee")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Athens")
ATH$total_area = (ATH$total_land + ATH$total_water)*0.000001

ATL <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cobb", "DeKalb", "Fulton"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Atlanta") 
ATL$total_area = (ATL$total_land + ATL$total_water)*0.000001

BEL <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Whatcom"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Bellingham") 
BEL$total_area = (BEL$total_land + BEL$total_water)*0.000001

CED <- counties("UT", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Iron")) %>%
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Cedar City")  
CED$total_area = (CED$total_land + CED$total_water)*0.000001

COL <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("El Paso")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Colorado Springs")
COL$total_area = (COL$total_land + COL$total_water)*0.000001

DFW <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Collin County", "Dallas", "Ellis", "Johnson",
                     "Tarrant"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Dallas Fort Worth") 
DFW$total_area = (DFW$total_land + DFW$total_water)*0.000001

DEN <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Adams", "Arapahoe", "Boulder","Broomfield",
                     "Denver", "Jefferson"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Denver")
DEN$total_area = (DEN$total_land + DEN$total_water)*0.000001

FLG <- counties("AZ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Coconino"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Flagstaff") 
FLG$total_area = (FLG$total_land + FLG$total_water)*0.000001

GNV <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alachua"))%>%
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Gainesville")
GNV$total_area = (GNV$total_land + GNV$total_water)*0.000001

HI <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hawaii"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Hawaii")
HI$total_area = (HI$total_land + HI$total_water)*0.000001

LAW <- counties("KS", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Douglas"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Lawrence")
LAW$total_area = (LAW$total_land + LAW$total_water)*0.000001

LAX <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Los Angeles", "Riverside",
                     "San Bernardino","San Diego"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Los Angeles") 
LAX$total_area = (LAX$total_land + LAX$total_water)*0.000001

LCR <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Houston", "Winona"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T))
LCR2 <- counties("WI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("La Crosse"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T))
LCR <- rbind(LCR, LCR2) %>% 
  summarize(total_land = sum(total_land), 
            total_water = sum(total_water))%>% 
  add_column(Cluster_Location = "La Crosse")
LCR$total_area = (LCR$total_land + LCR$total_water)*0.000001
rm(LCR2)

MOR <- counties("NJ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Essex", "Morris", "Somerset", "Union"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Morristown")
MOR$total_area = (MOR$total_land + MOR$total_water)*0.000001

MSP <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Anoka", "Hennepin", "Ramsey"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Minneapolis")
MSP$total_area = (MSP$total_land + MSP$total_water)*0.000001

NAS <- counties("TN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cheatham", "Davidson", "Rutherford", "Williamson"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Nashville") 
NAS$total_area = (NAS$total_land + NAS$total_water)*0.000001

OA <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Honolulu"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Oahu") 
OA$total_area = (OA$total_land + OA$total_water)*0.000001

PHX <- counties("AZ", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Maricopa", "Pinal")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Phoenix")
PHX$total_area = (PHX$total_land + PHX$total_water)*0.000001

### This also crosses state borders. I'm dead. 
PTD <- counties("OR", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clackamas", "Multnomah"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T))
PTD2 <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clark"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T))
PTD <- rbind(PTD, PTD2) %>% 
  summarize(total_land = sum(total_land), 
            total_water = sum(total_water))%>% 
  add_column(Cluster_Location = "Portland")
PTD$total_area = (PTD$total_land + PTD$total_water)*0.000001
rm(PTD2)

SLC <- counties("UT", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Salt Lake", "Davis")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Salt Lake City")
SLC$total_area = (SLC$total_land + SLC$total_water)*0.000001

SD <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("San Diego")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "San Diego")
SD$total_area = (SD$total_land + SD$total_water)*0.000001

SF <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alameda","Contra Costa", "Marin", "San Francisco",
                     "San Mateo","Santa Clara")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "San Francisco") 
SF$total_area = (SF$total_land + SF$total_water)*0.000001

SM <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hays"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "San Marcos")
SM$total_area = (SM$total_land + SM$total_water)*0.000001

SC <- counties("PA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Centre"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "State College") 
SC$total_area = (SC$total_land + SC$total_water)*0.000001

SP <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Pinellas"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "St Petersburg") 
SP$total_area = (SP$total_land + SP$total_water)*0.000001

WOO <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Wayne"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Wooster")
WOO$total_area = (WOO$total_land + WOO$total_water)*0.000001

#using only those counties we have data for
#calculating Hawaiian data, but not including in combined datasets as streamflow is missing
AA2 <- counties("MI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Washtenaw")) %>% 
  st_union() %>%
  as_Spatial(IDs = "Ann Arbor")
AA2 <- spTransform(AA2, CRS("+proj=utm +datum=WGS84")) %>% 
  gBuffer(width = (AA$total_area))

ABQ2 <- counties("NM", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Bernalillo")) %>% 
  st_union() %>%
  as_Spatial(IDs = "Albuquerque")
ABQ2 <- spTransform(ABQ2, CRS("+proj=utm +datum=WGS84")) %>% 
  gBuffer(width = (ABQ$total_area))

ATH2 <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clarke","Oconee")) %>% 
  st_union() %>%
  as_Spatial(IDs = "Athens")
ATH2 <- spTransform(ATH2, CRS("+proj=utm +datum=WGS84")) %>% 
  gBuffer(width = (ATH$total_area))

ATL2 <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cobb", "DeKalb", "Fulton"))%>% 
  st_union() %>%
  as_Spatial(IDs = "Atlanta")
ATL2 <- spTransform(ATL2, CRS("+proj=utm +datum=WGS84")) %>% 
  gBuffer(width = (ATL$total_area))

BEL2 <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Whatcom"))%>% 
  st_union() %>%
  as_Spatial(IDs = "Bellingham")
BEL2 <- spTransform(BEL2, CRS("+proj=utm +datum=WGS84")) %>% 
  gBuffer(width = (BEL$total_area))

CED2 <- counties("UT", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Iron")) %>%
  st_union() %>%
  as_Spatial(IDs = "Cedar City")
CED2 <- spTransform(CED2, CRS("+proj=utm +datum=WGS84")) %>% 
  gBuffer(width = (CED$total_area))

COL2 <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("El Paso")) %>% 
  st_union() %>% 
  as_Spatial(IDs = "Colorado Springs")
COL2 <- spTransform(COL2, CRS("+proj=utm +datum=WGS84")) %>% 
  gBuffer(width = (COL$total_area))

DFW2 <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Collin County", "Dallas", "Ellis", "Johnson",
                     "Tarrant"))%>% 
  st_union() %>%
  as_Spatial(IDs = "Dallas Fort Worth")

DEN2 <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Adams", "Arapahoe", "Boulder","Broomfield",
                     "Denver", "Jefferson"))%>% 
  st_union() %>%
  as_Spatial(IDs = "Denver")

FLG2 <- counties("AZ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Coconino"))%>% 
  st_union() %>%
  as_Spatial(IDs = "Flagstaff")

GNV2 <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alachua"))%>%
  st_union() %>%
  as_Spatial(IDs = "Gainesville")

HI2 <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hawaii"))%>% 
  st_union() %>%
  as_Spatial(IDs = "Hawaii")

LCR2 <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Houston", "Winona"))
LCR3 <- counties("WI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("La Crosse"))
LCR4 <- st_union(LCR2, LCR3)
LCR2 <- st_union(LCR4)
LCR2 <- as_Spatial(LCR, IDs = "La Crosse")
rm(LCR3, LCR4)

LAW2 <- counties("KS", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Douglas"))%>% 
  st_union() %>%
  as_Spatial(IDs = "Lawrence")

LAX2 <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Los Angeles", "Riverside",
                     "San Bernardino","San Diego"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Los Angeles")

MSP2 <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Anoka", "Hennepin", "Ramsey"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Minneapolis")

MOR2 <- counties("NJ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Essex", "Morris", "Somerset", "Union"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Morristown")

NAS2 <- counties("TN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cheatham", "Davidson", "Rutherford", "Williamson"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Nashville")

OA2 <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Honolulu"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Oahu")

PHX2 <- counties("AZ", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Maricopa", "Pinal")) %>% 
  st_union() %>% 
  as_Spatial(IDs = "Phoenix")

PTD2 <- counties("OR", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clackamas", "Multnomah"))
PTD3 <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clark"))
PTD4 <- st_union(PTD3, PTD2)
PTD2 <- st_union(PTD4)
PTD2 <-as_Spatial(PTD2, IDs = "Portland")
rm(PTD4, PTD3)

SLC2 <- counties("UT", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Salt Lake", "Davis")) %>% 
  st_union() %>% 
  as_Spatial(IDs = "Salt Lake City")

SD2 <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("San Diego")) %>% 
  st_union() %>% 
  as_Spatial(IDs = "San Diego")

SF2 <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alameda","Contra Costa", "Marin", "San Francisco",
                     "San Mateo","Santa Clara")) %>% 
  st_union() %>% 
  as_Spatial(IDs = "San Francisco")

SM2 <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hays"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "San Marcos")

SP2 <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Pinellas"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "St Petersburg")

SC2 <- counties("PA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Centre"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "State College")

WOO2 <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Wayne"))%>% 
  st_union() %>% 
  as_Spatial(IDs = "Wooster")

clusterLocations <- bind(ABQ, SF, AA, ATH, ATL, BEL, CED, COL, 
                         DEN, FLG, GNV, LCR, LAW, LAX, MSP, MOR, NAS, PHX,
                         PTD, SD, SLC, DFW, SM, SP, SC, WOO, keepnames = T)

clusterLocations <- bind(AA2, ABQ2,ATH2, ATL2, BEL2, CED2, COL2, keepnames = T)

#convert clusterLocations from degress to meters for expanding the borders of the metro areas
clusterLocations <- spTransform(clusterLocations, CRS("+proj=utm +datum=WGS84")) %>% 
  gBuffer(width = 20000)


elevation <- get_elev_raster(clusterLocations, z = 7)
clusterLocations$elevation_min <- raster::extract(elevation, clusterLocations,
                                                  weights = F, fun = min)
clusterLocations$elevation_min[clusterLocations$elevation_min <0] <- 0
clusterLocations$elevation_max <- raster::extract(elevation, clusterLocations,
                                                  weights = F, fun = max)

#read in raster
precip <- raster("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
streamflow <- raster("data/fa_qs_ann.tif")

#some empty data in the tif, and so na.rm = T
clusterLocations$streamflow <- raster::extract(streamflow, clusterLocations,
                                               weights = F, fun = sum, 
                                               na.rm = T)
clusterLocations$precip <- raster::extract(precip, clusterLocations, 
                                           weights = F, fun = mean)


multivariate <- as.data.frame(clusterLocations)
multivariate$Cluster_Location <- getSpPPolygonsIDSlots(clusterLocations)

multivariate <- left_join(multivariate, clusterLocations, by = 'Cluster_Location')




################
# CENSUS DATA
################

# tidycensus doesn't do income, only population by race. So, off we go

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
  unite(GEOID, c("state", "county"), sep = '')

#Let's combine population and average median income. 
AA$pop <- subset(acs_simple, GEOID == "26161")$pop
AA$medincome <- subset(acs_simple, GEOID == "26161")$medincome

ABQ$pop <- subset(acs_simple, GEOID == "35001")$pop
ABQ$medincome <- subset(acs_simple, GEOID == "35001")$medincome

ATH$pop <- sum(subset(acs_simple, 
                      GEOID == "13219" | GEOID == "13059")$pop)
ATH$medincome <- weighted.mean(subset(acs_simple, 
                                      GEOID == "13219" | GEOID == "13059")$medincome,
                               subset(acs_simple, 
                                      GEOID == "13219" | GEOID == "13059")$pop)

ATL$pop <- sum(subset(acs_simple, 
                      GEOID == "13121" |
                        GEOID == "13089" | 
                        GEOID == "13067")$pop)
ATL$medincome <- weighted.mean(subset(acs_simple, 
                                      GEOID == "13121" |
                                        GEOID == "13089" | 
                                        GEOID == "13067")$medincome,
                               subset(acs_simple, 
                                      GEOID == "13121" |
                                        GEOID == "13089" | 
                                        GEOID == "13067")$pop)

BEL$pop <- subset(acs_simple, GEOID == "53073")$pop 
BEL$medincome <- subset(acs_simple, GEOID == "53073")$medincome 

CED$pop <- subset(acs_simple, GEOID == "49021")$pop 
CED$medincome <- subset(acs_simple, GEOID == "49021")$medincome 

COL$pop <- subset(acs_simple, GEOID == "08041")$pop 
COL$medincome <- subset(acs_simple, GEOID == "08041")$medincome 

DEN$pop <- sum(subset(acs_simple, 
                      GEOID == "08005" |
                        GEOID == "08001" | 
                        GEOID == "08013" |
                        GEOID == "08014" |
                        GEOID == "08059" |
                        GEOID == "08031")$pop)
DEN$medincome <- weighted.mean(subset(acs_simple, 
                                      GEOID == "08005" |
                                        GEOID == "08001" | 
                                        GEOID == "08013" |
                                        GEOID == "08014" |
                                        GEOID == "08059" |
                                        GEOID == "08031")$medincome,
                               subset(acs_simple, 
                                      GEOID == "08005" |
                                        GEOID == "08001" | 
                                        GEOID == "08013" |
                                        GEOID == "08014" |
                                        GEOID == "08059" |
                                        GEOID == "08031")$pop)

DFW$pop <- sum(subset(acs_simple, 
                      GEOID == "48085" |
                        GEOID == "48113" | 
                        GEOID == "48139" |
                        GEOID == "48251" |
                        GEOID == "48439")$pop)
DFW$medincome <- weighted.mean(subset(acs_simple, 
                                      GEOID == "48085" |
                                        GEOID == "48113" | 
                                        GEOID == "48139" |
                                        GEOID == "48251" |
                                        GEOID == "48439")$medincome,
                               subset(acs_simple, 
                                      GEOID == "48085" |
                                        GEOID == "48113" | 
                                        GEOID == "48139" |
                                        GEOID == "48251" |
                                        GEOID == "48439")$pop)

FLG$pop <- subset(acs_simple, GEOID == "04005")$pop 
FLG$medincome <- subset(acs_simple, GEOID == "04005")$medincome 

GNV$pop <- subset(acs_simple, GEOID == "12001")$pop 
GNV$medincome <- subset(acs_simple, GEOID == "12001")$medincome 

HI$pop <- subset(acs_simple, GEOID == "15001")$pop 
HI$medincome <- subset(acs_simple, GEOID == "15001")$medincome 

LAW$pop <- subset(acs_simple, GEOID == "20045")$pop 
LAW$medincome <- subset(acs_simple, GEOID == "20045")$medincome 

LAX$pop <- sum(subset(acs_simple, 
                      GEOID == "06037" |
                        GEOID == "06065" | 
                        GEOID == "06071" |
                        GEOID == "06073")$pop)
LAX$medincome <- weighted.mean(subset(acs_simple, 
                                      GEOID == "06037" |
                                        GEOID == "06065" | 
                                        GEOID == "06071" |
                                        GEOID == "06073")$medincome,
                               subset(acs_simple, 
                                      GEOID == "06037" |
                                        GEOID == "06065" | 
                                        GEOID == "06071" |
                                        GEOID == "06073")$pop)
LCR$pop <- sum(subset(acs_simple, 
                      GEOID == "55063" |
                        GEOID == "27055" | 
                        GEOID == "27169")$pop)
LCR$medincome <- weighted.mean(subset(acs_simple, 
                                      GEOID == "55063" |
                                        GEOID == "27055" | 
                                        GEOID == "27169")$medincome,
                               subset(acs_simple, 
                                      GEOID == "55063" |
                                        GEOID == "27055" | 
                                        GEOID == "27169")$pop)

MOR$pop <- sum(subset(acs_simple, 
                      GEOID == "34013" |
                        GEOID == "34027" | 
                        GEOID == "34035" |
                        GEOID == "34039")$pop)
MOR$medincome <- weighted.mean(subset(acs_simple, 
                                      GEOID == "34013" |
                                        GEOID == "34027" | 
                                        GEOID == "34035" |
                                        GEOID == "34039")$medincome,
                               subset(acs_simple, 
                                      GEOID == "34013" |
                                        GEOID == "34027" | 
                                        GEOID == "34035" |
                                        GEOID == "34039")$pop)

MSP$pop <- sum(subset(acs_simple, 
                      GEOID == "27003" |
                        GEOID == "27053" | 
                        GEOID == "27123")$pop)
MSP$medincome <- weighted.mean(subset(acs_simple, 
                                      GEOID == "27003" |
                                        GEOID == "27053" | 
                                        GEOID == "27123")$medincome,
                               subset(acs_simple, 
                                      GEOID == "27003" |
                                        GEOID == "27053" | 
                                        GEOID == "27123")$pop)

NAS$pop <- sum(subset(acs_simple, 
                      GEOID == "47021" |
                        GEOID == "47037" | 
                        GEOID == "47149" |
                        GEOID == "47187")$pop)
NAS$medincome <- weighted.mean(subset(acs_simple, 
                                      GEOID == "47021" |
                                        GEOID == "47037" | 
                                        GEOID == "47149" |
                                        GEOID == "47187")$medincome,
                               subset(acs_simple, 
                                      GEOID == "47021" |
                                        GEOID == "47037" | 
                                        GEOID == "47149" |
                                        GEOID == "47187")$pop)

OA$pop <- subset(acs_simple, GEOID == "15003")$pop 
OA$medincome <- subset(acs_simple, GEOID == "15003")$medincome 

PHX$pop <- sum(subset(acs_simple, 
                      GEOID == "04013" |GEOID == "04021")$pop)
PHX$medincome <- weighted.mean(subset(acs_simple, GEOID == "04013" |
                                        GEOID == "04021")$medincome,
                               subset(acs_simple, 
                                      GEOID == "04013" |GEOID == "04021")$pop) 

PTD$pop <- sum(subset(acs_simple, 
                      GEOID == "41005" |
                        GEOID == "53011" | 
                        GEOID == "27123")$pop)
PTD$medincome <- weighted.mean(subset(acs_simple, 
                                      GEOID == "41005" |
                                        GEOID == "53011" | 
                                        GEOID == "27123")$medincome,
                               subset(acs_simple, 
                                      GEOID == "41005" |
                                        GEOID == "53011" | 
                                        GEOID == "27123")$pop)

SC$pop <- subset(acs_simple, GEOID == "42027")$pop
SC$medincome <- subset(acs_simple, GEOID == "42027")$medincome

SD$pop <- subset(acs_simple, GEOID == "06073")$pop
SD$medincome <- subset(acs_simple, GEOID == "06073")$medincome

SF$pop <- sum(subset(acs_simple, 
                     GEOID == "06001" |
                       GEOID == "06013" | 
                       GEOID == "06041" |
                       GEOID == "06075"| 
                       GEOID == "06081"| 
                       GEOID == "06085")$pop)
SF$medincome <- weighted.mean(subset(acs_simple, 
                                     GEOID == "06001" |
                                       GEOID == "06013" | 
                                       GEOID == "06041" |
                                       GEOID == "06075"| 
                                       GEOID == "06081"| 
                                       GEOID == "06085")$medincome,
                              subset(acs_simple, 
                                     GEOID == "06001" |
                                       GEOID == "06013" | 
                                       GEOID == "06041" |
                                       GEOID == "06075" | 
                                       GEOID == "06081" | 
                                       GEOID == "06085")$pop)

SLC$pop <- sum(subset(acs_simple, 
                      GEOID == "49011" |
                        GEOID == "49035")$pop)
SLC$medincome <- weighted.mean(subset(acs_simple, 
                                      GEOID == "49011" |
                                        GEOID == "49035")$medincome,
                               subset(acs_simple, 
                                      GEOID == "49011" |
                                        GEOID == "49035")$pop)

SM$pop <- subset(acs_simple, GEOID == "48209")$pop 
SM$medincome <- subset(acs_simple, GEOID == "48209")$medincome 

SP$pop <- subset(acs_simple, GEOID == "12103")$pop 
SP$medincome <- subset(acs_simple, GEOID == "12103")$medincome 

WOO$pop <- subset(acs_simple, GEOID == "39169")$pop 
WOO$medincome <- subset(acs_simple, GEOID == "39169")$medincome 

clusterLocations <- rbind(SLC, DFW, ABQ, SF, AA, ATH, ATL, BEL, CED, COL, 
                          DEN, FLG, GNV, LCR, LAW, LAX, MSP, MOR, NAS, PHX,
                          PTD, SD, SM, SP, SC, WOO)

clusterLocations$total_area <- (clusterLocations$total_land + 
                                  clusterLocations$total_water)*0.000001
#we want to create a population density instead of just population, 
#so can do pop/(total_land/1000) for sqkm- bodies of water in regions generally aren't counted towards land total
multivariate$popdensity <- multivariate$pop/(multivariate$total_land*0.000001)

multivariate <- multivariate %>% 
  select(-c("geometry"))
