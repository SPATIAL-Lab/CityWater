library(tidyverse);library(tigris, options(tigris_use_cache = TRUE))
library(viridis);library(raster);library(sf);library(maps);library(maptools); 
library(censusapi);library(elevatr);library(rgeos); library(readxl)

#using only those counties we have data for
#calculating Hawaiian data, but not including in combined datasets as streamflow is missing
AA <- counties("MI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Washtenaw")) %>% 
  st_union() %>%
  as_Spatial(IDs = "Ann Arbor")

ABQ <- counties("NM", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Bernalillo")) %>% 
  st_union() %>%
  as_Spatial(IDs = "Albuquerque")

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
  as_Spatial(IDs = "Gainesville")

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
  as_Spatial(IDs = "San Marcos")

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

expandedArea <- bind(AA, ABQ, ATH, ATL, BEL, CED, COL, DEN, DFW, FLG, 
                         GNV, LAW, LAX, LCR, HI, MSP, MOR, NAS, OA, PHX, PTD, SC, 
                         SD, SF, SLC,  SM, SP,  WOO, keepnames = T)

#convert clusterLocations from degrees to meters for expanding the borders of the metro areas
expandedArea <- spTransform(expandedArea, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +datum=WGS84 +units=m +no_defs")) %>% 
  gBuffer(width = 20000, byid = T )

elevation <- get_elev_raster(expandedArea, z = 7)

expandedArea$elevation_min <- raster::extract(elevation, expandedArea,
                                                  weights = F, fun = min)
expandedArea$elevation_min[expandedArea$elevation_min <0] <- 0
expandedArea$elevation_max <- raster::extract(elevation, expandedArea,
                                                  weights = F, fun = max)
expandedArea$elevation_max <- c(expandedArea$elevation_max)
expandedArea$elevation_min <- c(expandedArea$elevation_min)

#read in raster
precip <- raster("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
streamflow <- raster("data/fa_qs_ann.tif")

#some empty data in the tif, and so na.rm = T
expandedArea$streamflow <- raster::extract(streamflow, expandedArea,
                                               weights = F, fun = sum, 
                                               na.rm = T)
expandedArea$streamflow <- c(expandedArea$streamflow)

expandedArea$precip <- raster::extract(precip, expandedArea, 
                                           weights = F, fun = mean, 
                                           na.rm = T)
expandedArea$precip <- c(expandedArea$precip)

multivariate <- as.data.frame(expandedArea)
multivariate$Cluster_Location <- getSpPPolygonsIDSlots(expandedArea)


# Land and water area totals ----------------------------------------------

AA <- counties("MI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Washtenaw"))%>%
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Ann Arbor")

ABQ <- counties("NM", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Bernalillo"))%>%
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = 'Albuquerque')

ATH <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clarke","Oconee")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Athens")

ATL <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cobb", "DeKalb", "Fulton"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Atlanta") 

BEL <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Whatcom"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Bellingham") 

CED <- counties("UT", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Iron")) %>%
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Cedar City")  

COL <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("El Paso")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Colorado Springs")

DFW <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Collin County", "Dallas", "Ellis", "Johnson",
                     "Tarrant"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Dallas Fort Worth") 

DEN <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Adams", "Arapahoe", "Boulder","Broomfield",
                     "Denver", "Jefferson"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Denver")

FLG <- counties("AZ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Coconino"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Flagstaff") 

GNV <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alachua"))%>%
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Gainesville")

HI <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hawaii"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Hawaii")

LAW <- counties("KS", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Douglas"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Lawrence")

LAX <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Los Angeles", "Riverside",
                     "San Bernardino","San Diego"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Los Angeles") 

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

MOR <- counties("NJ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Essex", "Morris", "Somerset", "Union"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Morristown")

MSP <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Anoka", "Hennepin", "Ramsey"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Minneapolis")

NAS <- counties("TN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cheatham", "Davidson", "Rutherford", "Williamson"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Nashville") 

OA <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Honolulu"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Oahu") 

PHX <- counties("AZ", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Maricopa", "Pinal")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Phoenix")

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

SLC <- counties("UT", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Salt Lake", "Davis")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Salt Lake City")

SD <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("San Diego")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "San Diego")

SF <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alameda","Contra Costa", "Marin", "San Francisco",
                     "San Mateo","Santa Clara")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "San Francisco") 

SM <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hays"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "San Marcos")

SC <- counties("PA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Centre"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "State College") 

SP <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Pinellas"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "St Petersburg") 

WOO <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Wayne"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(Cluster_Location = "Wooster")


# Census demographic info -------------------------------------------------

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
PHX$medincome <- weighted.mean(subset(acs_simple, 
                                      GEOID == "04013" |
                                        GEOID == "04021")$medincome,
                               subset(acs_simple, 
                                      GEOID == "04013" |
                                        GEOID == "04021")$pop) 

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

clusterLocations <- rbind(AA, ABQ, ATH, ATL, BEL, CED, COL, DEN, DFW, FLG, 
                         GNV, LAW, LAX, LCR, HI, MOR, MSP, NAS, OA, PHX, PTD, SC, 
                         SD, SF, SLC, SM, SP, WOO)

clusterLocations$total_area <- (clusterLocations$total_land + 
                                  clusterLocations$total_water)*0.000001

clusterLocations$perc_water <- round(((clusterLocations$total_water*0.000001)/clusterLocations$total_area)*100, 2)
#we want to create a population density instead of just population, 
#so can do pop/(total_land/1000) for sqkm- bodies of water in regions generally aren't counted towards land total
multivariate <- left_join(multivariate, clusterLocations, by = "Cluster_Location")
multivariate$popdensity <- multivariate$pop/(multivariate$total_land*0.000001)
multivariate$elevation_range <- multivariate$elevation_max - multivariate$elevation_min
multivariate <- multivariate[ -c(1, 2, 6, 7, 10) ]

# data from https://www.sciencebase.gov/catalog/item/get/5af3311be4b0da30c1b245d8, lightly cleaned
water <- read_excel("data/water.xlsx")
df <- water %>% 
  rename(GEOID = FIPS, 
         groundwater_total = 'TO-WGWFr', 
         surfacewater_total = 'TO-WSWFr', 
         fresh_total = 'TO-WFrTo') %>% 
  select(GEOID, 
         groundwater_total,
         surfacewater_total, 
         fresh_total
  )

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

multivariate$surfacewater_total <- NA
multivariate$surfacewater_total[multivariate$Cluster_Location == "Ann Arbor"] <- subset(df, GEOID == "26161")$surfacewater_total
multivariate$surfacewater_total[multivariate$Cluster_Location == "Albuquerque"] <- subset(df, GEOID == "35001")$surfacewater_total
multivariate$surfacewater_total[multivariate$Cluster_Location == "Athens"] <- sum(subset(df, 
                                                                                         GEOID == "13219" |
                                                                                           GEOID == "13059")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "Atlanta"] <- sum(subset(df, 
                                                                                          GEOID == "13121" |
                                                                                            GEOID == "13089" | 
                                                                                            GEOID == "13067")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "Bellingham"] <- subset(df, GEOID == "53073")$surfacewater_total
multivariate$surfacewater_total[multivariate$Cluster_Location == "Cedar City"] <- subset(df, GEOID == "49021")$surfacewater_total
multivariate$surfacewater_total[multivariate$Cluster_Location == "Colorado Springs"] <- subset(df, GEOID == "08041")$surfacewater_total
multivariate$surfacewater_total[multivariate$Cluster_Location == "Denver"] <- sum(subset(df, 
                                                                                         GEOID == "08005" |
                                                                                           GEOID == "08001" | 
                                                                                           GEOID == "08013" |
                                                                                           GEOID == "08014" |
                                                                                           GEOID == "08059" |
                                                                                           GEOID == "08031")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "Dallas Fort Worth"] <- sum(subset(df, 
                                                                                                    GEOID == "48085" |
                                                                                                      GEOID == "48113" | 
                                                                                                      GEOID == "48139" |
                                                                                                      GEOID == "48251" |
                                                                                                      GEOID == "48439")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "Flagstaff"] <- subset(df, GEOID == "04005")$surfacewater_total
multivariate$surfacewater_total[multivariate$Cluster_Location == "Gainesville"] <- subset(df, GEOID == "12001")$surfacewater_total
multivariate$surfacewater_total[multivariate$Cluster_Location == "Lawrence"] <- subset(df, GEOID == "20045")$surfacewater_total
multivariate$surfacewater_total[multivariate$Cluster_Location == "Los Angeles"] <- sum(subset(df, 
                                                                                              GEOID == "06037" |
                                                                                                GEOID == "06065" | 
                                                                                                GEOID == "06071" |
                                                                                                GEOID == "06073")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "La Crosse"] <- sum(subset(df, 
                                                                                            GEOID == "55063" |
                                                                                              GEOID == "27055" | 
                                                                                              GEOID == "27169")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "Morristown"] <- sum(subset(df, 
                                                                                             GEOID == "34013" |
                                                                                               GEOID == "34027" | 
                                                                                               GEOID == "34035" |
                                                                                               GEOID == "34039")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "Minneapolis"] <- sum(subset(df, 
                                                                                              GEOID == "27003" |
                                                                                                GEOID == "27053" | 
                                                                                                GEOID == "27123")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "Nashville"] <- sum(subset(df, 
                                                                                            GEOID == "47021" |
                                                                                              GEOID == "47037" | 
                                                                                              GEOID == "47149" |
                                                                                              GEOID == "47187")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "Phoenix"] <- sum(subset(df, 
                                                                                          GEOID == "04013" |GEOID == "04021")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "Portland"] <- sum(subset(df, 
                                                                                           GEOID == "41005" |
                                                                                             GEOID == "53011" | 
                                                                                             GEOID == "27123")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "State College"] <- subset(df, GEOID == "42027")$surfacewater_total
multivariate$surfacewater_total[multivariate$Cluster_Location == "San Diego"] <- subset(df, GEOID == "06073")$surfacewater_total
multivariate$surfacewater_total[multivariate$Cluster_Location == "San Francisco"] <- sum(subset(df, 
                                                                                                GEOID == "06001" |
                                                                                                  GEOID == "06013" | 
                                                                                                  GEOID == "06041" |
                                                                                                  GEOID == "06075" | 
                                                                                                  GEOID == "06081" | 
                                                                                                  GEOID == "06085")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "Salt Lake City"] <- sum(subset(df, 
                                                                                                 GEOID == "49011" |
                                                                                                   GEOID == "49035")$surfacewater_total)
multivariate$surfacewater_total[multivariate$Cluster_Location == "San Marcos"] <- subset(df, GEOID == "48209")$surfacewater_total
multivariate$surfacewater_total[multivariate$Cluster_Location == "St Petersburg"] <- subset(df, GEOID == "12103")$surfacewater_total
multivariate$surfacewater_total[multivariate$Cluster_Location == "Wooster"] <- subset(df, GEOID == "39169")$surfacewater_total

multivariate$groundwater_total <- NA
multivariate$groundwater_total[multivariate$Cluster_Location == "Ann Arbor"] <- subset(df, GEOID == "26161")$groundwater_total
multivariate$groundwater_total[multivariate$Cluster_Location == "Albuquerque"] <- subset(df, GEOID == "35001")$groundwater_total
multivariate$groundwater_total[multivariate$Cluster_Location == "Athens"] <- sum(subset(df, 
                                                                                        GEOID == "13219" |
                                                                                          GEOID == "13059")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "Atlanta"] <- sum(subset(df, 
                                                                                         GEOID == "13121" |
                                                                                           GEOID == "13089" | 
                                                                                           GEOID == "13067")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "Bellingham"] <- subset(df, GEOID == "53073")$groundwater_total
multivariate$groundwater_total[multivariate$Cluster_Location == "Cedar City"] <- subset(df, GEOID == "49021")$groundwater_total
multivariate$groundwater_total[multivariate$Cluster_Location == "Colorado Springs"] <- subset(df, GEOID == "08041")$groundwater_total
multivariate$groundwater_total[multivariate$Cluster_Location == "Denver"] <- sum(subset(df, 
                                                                                        GEOID == "08005" |
                                                                                          GEOID == "08001" | 
                                                                                          GEOID == "08013" |
                                                                                          GEOID == "08014" |
                                                                                          GEOID == "08059" |
                                                                                          GEOID == "08031")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "Dallas Fort Worth"] <- sum(subset(df, 
                                                                                                   GEOID == "48085" |
                                                                                                     GEOID == "48113" | 
                                                                                                     GEOID == "48139" |
                                                                                                     GEOID == "48251" |
                                                                                                     GEOID == "48439")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "Flagstaff"] <- subset(df, GEOID == "04005")$groundwater_total
multivariate$groundwater_total[multivariate$Cluster_Location == "Gainesville"] <- subset(df, GEOID == "12001")$groundwater_total
multivariate$groundwater_total[multivariate$Cluster_Location == "Lawrence"] <- subset(df, GEOID == "20045")$groundwater_total
multivariate$groundwater_total[multivariate$Cluster_Location == "Los Angeles"] <- sum(subset(df, 
                                                                                             GEOID == "06037" |
                                                                                               GEOID == "06065" | 
                                                                                               GEOID == "06071" |
                                                                                               GEOID == "06073")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "La Crosse"] <- sum(subset(df, 
                                                                                           GEOID == "55063" |
                                                                                             GEOID == "27055" | 
                                                                                             GEOID == "27169")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "Morristown"] <- sum(subset(df, 
                                                                                            GEOID == "34013" |
                                                                                              GEOID == "34027" | 
                                                                                              GEOID == "34035" |
                                                                                              GEOID == "34039")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "Minneapolis"] <- sum(subset(df, 
                                                                                             GEOID == "27003" |
                                                                                               GEOID == "27053" | 
                                                                                               GEOID == "27123")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "Nashville"] <- sum(subset(df, 
                                                                                           GEOID == "47021" |
                                                                                             GEOID == "47037" | 
                                                                                             GEOID == "47149" |
                                                                                             GEOID == "47187")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "Phoenix"] <- sum(subset(df, 
                                                                                         GEOID == "04013" |GEOID == "04021")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "Portland"] <- sum(subset(df, 
                                                                                          GEOID == "41005" |
                                                                                            GEOID == "53011" | 
                                                                                            GEOID == "27123")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "State College"] <- subset(df, GEOID == "42027")$groundwater_total
multivariate$groundwater_total[multivariate$Cluster_Location == "San Diego"] <- subset(df, GEOID == "06073")$groundwater_total
multivariate$groundwater_total[multivariate$Cluster_Location == "San Francisco"] <- sum(subset(df, 
                                                                                               GEOID == "06001" |
                                                                                                 GEOID == "06013" | 
                                                                                                 GEOID == "06041" |
                                                                                                 GEOID == "06075" | 
                                                                                                 GEOID == "06081" | 
                                                                                                 GEOID == "06085")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "Salt Lake City"] <- sum(subset(df, 
                                                                                                GEOID == "49011" |
                                                                                                  GEOID == "49035")$groundwater_total)
multivariate$groundwater_total[multivariate$Cluster_Location == "San Marcos"] <- subset(df, GEOID == "48209")$groundwater_total
multivariate$groundwater_total[multivariate$Cluster_Location == "St Petersburg"] <- subset(df, GEOID == "12103")$groundwater_total
multivariate$groundwater_total[multivariate$Cluster_Location == "Wooster"] <- subset(df, GEOID == "39169")$groundwater_total


write.csv(multivariate, "data/multivariate.csv")