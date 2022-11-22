#Let's try creating shapefiles matching the US Census using tigris, a package 
#which directly pulls from Census data and plays well with tidyverse 
#This assumes G0 has been run. 
library(tigris, options(tigris_use_cache = TRUE))
library(viridis);library(raster); library(sf)

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

LCR <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Houston", "Winona"))

LCR2 <- counties("WI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("La Crosse"))
LCR3 <- st_union(LCR, LCR2)
LCR <- st_union(LCR3)
LCR <- as_Spatial(LCR, IDs = "La Crosse")
rm(LCR2, LCR3)

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

PTD <- counties("OR", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clackamas", "Multnomah"))

PTD2 <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clark"))
PTD3 <- st_union(PTD, PTD2)
PTD <- st_union(PTD3)
PTD <-as_Spatial(PTD, IDs = "Portland")
rm(PTD2, PTD3)

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

#read in raster
precip <- raster("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
streamflow <- raster("data/fa_qs_ann.tif")

#some empty data in the tif, and so na.rm = T
clusterLocations$streamflow <- raster::extract(streamflow, clusterLocations,
                                               weights = F, fun = sum, 
                                               na.rm = T)
clusterLocations$precip <- raster::extract(precip, clusterLocations, 
                                           weights = F, fun = mean)

multivariate <- as.data.frame(clusterLocations) %>% 
  select(c(streamflow, precip))
multivariate$Cluster_ID <- getSpPPolygonsIDSlots(clusterLocations)

############
# ALAND AND AWATER SUMS
############
AA <- counties("MI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Washtenaw"))%>%
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Ann Arbor")

ABQ <- counties("NM", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Bernalillo"))%>%
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = 'Albuquerque') 

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

### NOTE: AHHHH THIS CROSSES STATE BORDERS!!!!
LCR <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Houston", "Winona"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T))

LCR2 <- counties("WI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("La Crosse"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T))

LCR <- rbind(LCR, LCR2) %>% 
  summarize(total_area = sum(total_area), 
            total_water = sum(total_water))%>% 
  add_column(Cluster_ID = "La Crosse")
rm(LCR2)

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

MOR <- counties("NJ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Essex", "Morris", "Somerset", "Union"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Morristown")

MSP <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Anoka", "Hennepin", "Ramsey"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Minneapolis")

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

### This also crosses state borders. I'm dead. 
PTD <- counties("OR", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clackamas", "Multnomah"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T))

PTD2 <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clark"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T))

PTD <- rbind(PTD, PTD2) %>% 
  summarize(total_area = sum(total_area), 
            total_water = sum(total_water))%>% 
  add_column(Cluster_ID = "Portland")
rm(PTD2)

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

SC <- counties("PA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Centre"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "State College")

SP <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Pinellas"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "St Petersburg")

WOO <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Wayne"))%>% 
  summarize(total_area = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_ID = "Wooster")

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

SM$pop <- subset(acs_simple, GEOID == "06081")$pop 
SM$medincome <- subset(acs_simple, GEOID == "06081")$medincome 

SP$pop <- subset(acs_simple, GEOID == "12103")$pop 
SP$medincome <- subset(acs_simple, GEOID == "12103")$medincome 

WOO$pop <- subset(acs_simple, GEOID == "39169")$pop 
WOO$medincome <- subset(acs_simple, GEOID == "39169")$medincome 


clusterLocations <- rbind(SLC, DFW, ABQ, SF, AA, ATH, ATL, BEL, CED, COL, 
                          DEN, FLG, GNV, LCR, LAW, LAX, MSP, MOR, NAS, PHX,
                          PTD, SD, SM, SP, SC, WOO)


tapData$d_ex <- (tapData$d2H - 8 * tapData$d18O)

multivariate <- left_join(multivariate, clusterLocations, by = 'Cluster_ID')





##############
#GRAVEYARD 
#################
#Okay first what do we want from tapData
multilevel <- tapData %>% 
  dplyr::select(d_ex, d18O, County, Year, Season, Month, Cluster_ID, Elevation_mabsl, Cluster_Location, Lat, Long) %>% 
  rename(NAME = County, City = Cluster_Location) 

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


################
# CITIES (BROKEN)
################
#Let's try to ID cities from coordinates. Note that only cities >40k population or capitals are included
latlong2city <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  cities <- map('us_cities', fill = TRUE, col = "transparent", plot = FALSE)
  IDs <- sapply(strsplit(cities$names, ":"), function(x) x[1])
  cities_sp <- map2SpatialPolygons(cities, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, cities_sp)
  
  # Return the state names of the Polygons object containing each point
  cityNames <- sapply(cities_sp@polygons, function(x) x@ID)
  cityNames[indices]
}

tapData$City <- latlong2city(xy)
tapData$City <- gsub(".*,", "", tapData$City)
tapData$City <- str_to_title(tapData$City) 

################
# ZIPCODES (Takes forever)
################
library(revgeo)
#This is my (Chris's) personal API key so be sure to replace it with your own.
#This takes approximately one millions years, so plan accordingly
Zipcode <- revgeo(longitude = tapData$Long, latitude = tapData$Lat,
                  provider = 'bing',
                  API = 'AtyAEH5aBzGVG8ItlBt6hOjBcp3Jx6sdYQul0L6kV0cLetAxqQAuhK5I9PAe21Iv',
                  output= 'frame'
)

#### Let's prepare CoVariates for import to do some multilevel regression
covariates <- read_excel("data/desc_stats1.xlsx", 
                         sheet = "CoVariates", col_types = c("skip", 
                                                             "text", "skip", "skip", "text", "text", 
                                                             "skip", "skip", "skip", "numeric", 
                                                             "numeric", "skip", "numeric", "numeric", 
                                                             "numeric", "text", "skip", "skip", 
                                                             "skip", "skip"))

covariates$GEOID <- str_pad(covariates$GEOID, 5, pad = "0")
covariates$COUNTYFP <- str_pad(covariates$COUNTYFP, 3, pad = "0")