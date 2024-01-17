# Scraping information to build predictive models. Note you'll need a Census API key to query some of this info. 
library(tidyr); library(dplyr);library(tigris, options(tigris_use_cache = TRUE))
library(censusapi); library(readxl); library(tibble); library(terra)

#using only those counties we have data for
#calculating Hawaiian data, but not including in combined datasets as streamflow is missing

AA <- counties("MI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Washtenaw")) %>% 
  mutate(id = "Ann Arbor") %>% 
  vect() 

ABQ <- counties("NM", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Bernalillo")) %>% 
  mutate(id = "Albuquerque") %>% 
  vect()

ATH <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clarke","Oconee")) %>% 
  vect() %>% 
  terra::aggregate()
ATH$id <- "Athens"   

ATL <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cobb", "DeKalb", "Fulton"))%>% 
  vect() %>% 
  terra::aggregate()
ATL$id <- "Atlanta"

BEL <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Whatcom"))%>% 
  mutate(id = "Bellingham") %>% 
  vect()

CED <- counties("UT", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Iron")) %>%
  mutate(id = "Cedar City") %>% 
  vect()

COL <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("El Paso")) %>% 
  mutate(id = "Colorado Springs") %>% 
  vect()

DFW <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Collin County", "Dallas", "Ellis", "Johnson",
                     "Tarrant"))%>% 
  vect()%>% 
  terra::aggregate()
DFW$id <- "Dallas Fort Worth"

DEN <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Adams", "Arapahoe", "Boulder","Broomfield",
                     "Denver", "Jefferson")) %>% 
  vect() %>% 
  terra::aggregate()
DEN$id <- "Denver"

FLG <- counties("AZ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Coconino"))%>% 
  mutate(id = "Flagstaff") %>% 
  vect()

GNV <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alachua"))%>%
  mutate(id = "Gainesville") %>% 
  vect()

HI <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hawaii"))%>% 
  mutate(id = "Hawaii") %>% 
  vect()

LCR1 <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Houston", "Winona")) %>% 
  vect() 
LCR2 <- counties("WI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("La Crosse"))  %>% 
  vect()
LCR <- terra::aggregate(terra::union(LCR1, LCR2))
rm(LCR1, LCR2)
LCR$id <- "La Crosse"

LAW <- counties("KS", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Douglas"))%>% 
  mutate(id = "Lawrence") %>% 
  vect()

LAX <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Los Angeles", "Riverside",
                     "San Bernardino","San Diego"))%>%  
  vect() %>% 
  terra::aggregate()
LAX$id <- "Los Angeles"

MSP <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Anoka", "Hennepin", "Ramsey"))%>% 
  vect() %>% 
  terra::aggregate()
MSP$id <- "Minneapolis"

MOR <- counties("NJ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Essex", "Morris", "Somerset", "Union"))%>% 
  vect() %>% 
  terra::aggregate()
MOR$id <- "Morristown"

NAS <- counties("TN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cheatham", "Davidson", "Rutherford", "Williamson"))%>% 
  vect() %>% 
  terra::aggregate()
NAS$id <- "Nashville"

OA <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Honolulu"))%>% 
  mutate(id = "Oahu") %>% 
  vect()

PHX <- counties("AZ", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Maricopa", "Pinal")) %>% 
  vect() %>% 
  terra::aggregate()
PHX$id <- "Phoenix"

PTD1 <- counties("OR", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clackamas", "Multnomah")) %>% 
  vect()
PTD2 <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clark")) %>% 
  vect()
PTD <- terra::aggregate(terra::union(PTD1, PTD2)) 
rm(PTD2, PTD1)
PTD$id <- "Portland"

SLC <- counties("UT", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Salt Lake", "Davis")) %>% 
  vect() %>% 
  terra::aggregate()
SLC$id <- 'Salt Lake City'

SD <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("San Diego")) %>% 
  mutate(id = "San Diego") %>% 
  vect()

SF <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alameda","Contra Costa", "Marin", "San Francisco",
                     "San Mateo","Santa Clara")) %>% 
  vect() %>% 
  terra::aggregate()
SF$id <- "San Francisco"

SM <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hays"))%>% 
  mutate(id = "San Marcos") %>% 
  vect()

SP <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Pinellas"))%>% 
  mutate(id = "St Petersburg") %>% 
  vect()

SC <- counties("PA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Centre"))%>% 
  mutate(id = "State College") %>% 
  vect()

WOO <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Wayne"))%>% 
  mutate(id = "Wooster") %>% 
  vect()

expandedArea <- rbind(AA, ABQ, ATH, ATL, BEL, CED, COL, DEN, DFW, FLG, 
                         GNV, HI, LAW, LAX, LCR, MOR, MSP,NAS, OA, PHX, PTD, SC, 
                         SD, SF, SLC, SM, SP, WOO, keepnames = T)

ruggedness <- rast("maps/elev_diff.tif")
expandedArea <- project(expandedArea, ruggedness) %>% crop(ruggedness)
df = terra::extract(ruggedness, expandedArea, fun = max, na.rm = T)
expandedArea$ruggedness <- round(df$file5c181b575a85, 0)

#convert clusterLocations from degrees to meters for expanding the borders of the metro areas
#expandedArea <- buffer(expandedArea, width = 20000)

#read in raster
precip <- project(rast("maps/precip_mean.tif"), ruggedness)
streamflow <- project(rast("maps/sf.tif"), ruggedness)

#some empty data in the tif, and so na.rm = T
df <- terra::extract(streamflow, expandedArea, weights = F, fun = mean, na.rm = T)
expandedArea$streamflow <- c(df$sf)

df <- terra::extract(precip, expandedArea, weights = F, fun = mean, na.rm = T)
expandedArea$precip <- c(df$PRISM_ppt_30yr_normal_4kmM3_annual_asc)

multivariate <- subset(as.data.frame(expandedArea), !is.na(id)) 
multivariate <- multivariate %>% 
  select(c(id, ruggedness, streamflow, precip)) %>% 
  rename(cluster_location = id)
#ruggedness slotted in manually into the .csv for now
# Land and water area totals ----------------------------------------------

AA <- counties("MI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Washtenaw"))%>%
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Ann Arbor")

ABQ <- counties("NM", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Bernalillo"))%>%
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = 'Albuquerque')

ATH <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clarke","Oconee")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Athens")

ATL <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cobb", "DeKalb", "Fulton"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Atlanta") 

BEL <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Whatcom"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Bellingham") 

CED <- counties("UT", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Iron")) %>%
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Cedar City")  

COL <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("El Paso")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Colorado Springs")

DFW <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Collin County", "Dallas", "Ellis", "Johnson",
                     "Tarrant"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Dallas Fort Worth") 

DEN <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Adams", "Arapahoe", "Boulder","Broomfield",
                     "Denver", "Jefferson"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Denver")

FLG <- counties("AZ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Coconino"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Flagstaff") 

GNV <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alachua"))%>%
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Gainesville")

HI <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hawaii"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Hawaii")

LAW <- counties("KS", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Douglas"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Lawrence")

LAX <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Los Angeles", "Riverside",
                     "San Bernardino","San Diego"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Los Angeles") 

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
  add_column(cluster_location = "La Crosse")
rm(LCR2)
MOR <- counties("NJ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Essex", "Morris", "Somerset", "Union"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Morristown")

MSP <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Anoka", "Hennepin", "Ramsey"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Minneapolis")

NAS <- counties("TN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cheatham", "Davidson", "Rutherford", "Williamson"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Nashville") 

OA <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Honolulu"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Oahu") 

PHX <- counties("AZ", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Maricopa", "Pinal")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Phoenix")

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
  add_column(cluster_location = "Portland")
rm(PTD2)
SLC <- counties("UT", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Salt Lake", "Davis")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Salt Lake City")

SD <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("San Diego")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "San Diego")

SF <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alameda","Contra Costa", "Marin", "San Francisco",
                     "San Mateo","Santa Clara")) %>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "San Francisco") 

SM <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hays"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "San Marcos")

SC <- counties("PA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Centre"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "State College") 

SP <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Pinellas"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "St Petersburg") 

WOO <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Wayne"))%>% 
  summarize(total_land = sum(ALAND, na.rm = T), 
            total_water = sum(AWATER, na.rm = T)) %>% 
  add_column(cluster_location = "Wooster")


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
multivariate <- left_join(multivariate, clusterLocations, by = "cluster_location")
multivariate$popdensity <- multivariate$pop/(multivariate$total_land*0.000001)

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
multivariate$water_use[multivariate$cluster_location == "Ann Arbor"] <- subset(df, GEOID == "26161")$fresh_total
multivariate$water_use[multivariate$cluster_location == "Albuquerque"] <- subset(df, GEOID == "35001")$fresh_total
multivariate$water_use[multivariate$cluster_location == "Athens"] <- sum(subset(df, 
                                                                                GEOID == "13219" |
                                                                                  GEOID == "13059")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "Atlanta"] <- sum(subset(df, 
                                                                                 GEOID == "13121" |
                                                                                   GEOID == "13089" | 
                                                                                   GEOID == "13067")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "Bellingham"] <- subset(df, GEOID == "53073")$fresh_total
multivariate$water_use[multivariate$cluster_location == "Cedar City"] <- subset(df, GEOID == "49021")$fresh_total
multivariate$water_use[multivariate$cluster_location == "Colorado Springs"] <- subset(df, GEOID == "08041")$fresh_total
multivariate$water_use[multivariate$cluster_location == "Denver"] <- sum(subset(df, 
                                                                                GEOID == "08005" |
                                                                                  GEOID == "08001" | 
                                                                                  GEOID == "08013" |
                                                                                  GEOID == "08014" |
                                                                                  GEOID == "08059" |
                                                                                  GEOID == "08031")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "Dallas Fort Worth"] <- sum(subset(df, 
                                                                                           GEOID == "48085" |
                                                                                             GEOID == "48113" | 
                                                                                             GEOID == "48139" |
                                                                                             GEOID == "48251" |
                                                                                             GEOID == "48439")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "Flagstaff"] <- subset(df, GEOID == "04005")$fresh_total
multivariate$water_use[multivariate$cluster_location == "Gainesville"] <- subset(df, GEOID == "12001")$fresh_total
multivariate$water_use[multivariate$cluster_location == "Lawrence"] <- subset(df, GEOID == "20045")$fresh_total
multivariate$water_use[multivariate$cluster_location == "Los Angeles"] <- sum(subset(df, 
                                                                                     GEOID == "06037" |
                                                                                       GEOID == "06065" | 
                                                                                       GEOID == "06071" |
                                                                                       GEOID == "06073")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "La Crosse"] <- sum(subset(df, 
                                                                                   GEOID == "55063" |
                                                                                     GEOID == "27055" | 
                                                                                     GEOID == "27169")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "Morristown"] <- sum(subset(df, 
                                                                                    GEOID == "34013" |
                                                                                      GEOID == "34027" | 
                                                                                      GEOID == "34035" |
                                                                                      GEOID == "34039")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "Minneapolis"] <- sum(subset(df, 
                                                                                     GEOID == "27003" |
                                                                                       GEOID == "27053" | 
                                                                                       GEOID == "27123")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "Nashville"] <- sum(subset(df, 
                                                                                   GEOID == "47021" |
                                                                                     GEOID == "47037" | 
                                                                                     GEOID == "47149" |
                                                                                     GEOID == "47187")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "Phoenix"] <- sum(subset(df, 
                                                                                 GEOID == "04013" |GEOID == "04021")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "Portland"] <- sum(subset(df, 
                                                                                  GEOID == "41005" |
                                                                                    GEOID == "53011" | 
                                                                                    GEOID == "27123")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "State College"] <- subset(df, GEOID == "42027")$fresh_total
multivariate$water_use[multivariate$cluster_location == "San Diego"] <- subset(df, GEOID == "06073")$fresh_total
multivariate$water_use[multivariate$cluster_location == "San Francisco"] <- sum(subset(df, 
                                                                                       GEOID == "06001" |
                                                                                         GEOID == "06013" | 
                                                                                         GEOID == "06041" |
                                                                                         GEOID == "06075" | 
                                                                                         GEOID == "06081" | 
                                                                                         GEOID == "06085")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "Salt Lake City"] <- sum(subset(df, 
                                                                                        GEOID == "49011" |
                                                                                          GEOID == "49035")$fresh_total)
multivariate$water_use[multivariate$cluster_location == "San Marcos"] <- subset(df, GEOID == "48209")$fresh_total
multivariate$water_use[multivariate$cluster_location == "St Petersburg"] <- subset(df, GEOID == "12103")$fresh_total
multivariate$water_use[multivariate$cluster_location == "Wooster"] <- subset(df, GEOID == "39169")$fresh_total

multivariate$surfacewater_total <- NA
multivariate$surfacewater_total[multivariate$cluster_location == "Ann Arbor"] <- subset(df, GEOID == "26161")$surfacewater_total
multivariate$surfacewater_total[multivariate$cluster_location == "Albuquerque"] <- subset(df, GEOID == "35001")$surfacewater_total
multivariate$surfacewater_total[multivariate$cluster_location == "Athens"] <- sum(subset(df, 
                                                                                         GEOID == "13219" |
                                                                                           GEOID == "13059")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "Atlanta"] <- sum(subset(df, 
                                                                                          GEOID == "13121" |
                                                                                            GEOID == "13089" | 
                                                                                            GEOID == "13067")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "Bellingham"] <- subset(df, GEOID == "53073")$surfacewater_total
multivariate$surfacewater_total[multivariate$cluster_location == "Cedar City"] <- subset(df, GEOID == "49021")$surfacewater_total
multivariate$surfacewater_total[multivariate$cluster_location == "Colorado Springs"] <- subset(df, GEOID == "08041")$surfacewater_total
multivariate$surfacewater_total[multivariate$cluster_location == "Denver"] <- sum(subset(df, 
                                                                                         GEOID == "08005" |
                                                                                           GEOID == "08001" | 
                                                                                           GEOID == "08013" |
                                                                                           GEOID == "08014" |
                                                                                           GEOID == "08059" |
                                                                                           GEOID == "08031")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "Dallas Fort Worth"] <- sum(subset(df, 
                                                                                                    GEOID == "48085" |
                                                                                                      GEOID == "48113" | 
                                                                                                      GEOID == "48139" |
                                                                                                      GEOID == "48251" |
                                                                                                      GEOID == "48439")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "Flagstaff"] <- subset(df, GEOID == "04005")$surfacewater_total
multivariate$surfacewater_total[multivariate$cluster_location == "Gainesville"] <- subset(df, GEOID == "12001")$surfacewater_total
multivariate$surfacewater_total[multivariate$cluster_location == "Lawrence"] <- subset(df, GEOID == "20045")$surfacewater_total
multivariate$surfacewater_total[multivariate$cluster_location == "Los Angeles"] <- sum(subset(df, 
                                                                                              GEOID == "06037" |
                                                                                                GEOID == "06065" | 
                                                                                                GEOID == "06071" |
                                                                                                GEOID == "06073")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "La Crosse"] <- sum(subset(df, 
                                                                                            GEOID == "55063" |
                                                                                              GEOID == "27055" | 
                                                                                              GEOID == "27169")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "Morristown"] <- sum(subset(df, 
                                                                                             GEOID == "34013" |
                                                                                               GEOID == "34027" | 
                                                                                               GEOID == "34035" |
                                                                                               GEOID == "34039")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "Minneapolis"] <- sum(subset(df, 
                                                                                              GEOID == "27003" |
                                                                                                GEOID == "27053" | 
                                                                                                GEOID == "27123")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "Nashville"] <- sum(subset(df, 
                                                                                            GEOID == "47021" |
                                                                                              GEOID == "47037" | 
                                                                                              GEOID == "47149" |
                                                                                              GEOID == "47187")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "Phoenix"] <- sum(subset(df, 
                                                                                          GEOID == "04013" |GEOID == "04021")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "Portland"] <- sum(subset(df, 
                                                                                           GEOID == "41005" |
                                                                                             GEOID == "53011" | 
                                                                                             GEOID == "27123")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "State College"] <- subset(df, GEOID == "42027")$surfacewater_total
multivariate$surfacewater_total[multivariate$cluster_location == "San Diego"] <- subset(df, GEOID == "06073")$surfacewater_total
multivariate$surfacewater_total[multivariate$cluster_location == "San Francisco"] <- sum(subset(df, 
                                                                                                GEOID == "06001" |
                                                                                                  GEOID == "06013" | 
                                                                                                  GEOID == "06041" |
                                                                                                  GEOID == "06075" | 
                                                                                                  GEOID == "06081" | 
                                                                                                  GEOID == "06085")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "Salt Lake City"] <- sum(subset(df, 
                                                                                                 GEOID == "49011" |
                                                                                                   GEOID == "49035")$surfacewater_total)
multivariate$surfacewater_total[multivariate$cluster_location == "San Marcos"] <- subset(df, GEOID == "48209")$surfacewater_total
multivariate$surfacewater_total[multivariate$cluster_location == "St Petersburg"] <- subset(df, GEOID == "12103")$surfacewater_total
multivariate$surfacewater_total[multivariate$cluster_location == "Wooster"] <- subset(df, GEOID == "39169")$surfacewater_total

multivariate$groundwater_total <- NA
multivariate$groundwater_total[multivariate$cluster_location == "Ann Arbor"] <- subset(df, GEOID == "26161")$groundwater_total
multivariate$groundwater_total[multivariate$cluster_location == "Albuquerque"] <- subset(df, GEOID == "35001")$groundwater_total
multivariate$groundwater_total[multivariate$cluster_location == "Athens"] <- sum(subset(df, 
                                                                                        GEOID == "13219" |
                                                                                          GEOID == "13059")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "Atlanta"] <- sum(subset(df, 
                                                                                         GEOID == "13121" |
                                                                                           GEOID == "13089" | 
                                                                                           GEOID == "13067")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "Bellingham"] <- subset(df, GEOID == "53073")$groundwater_total
multivariate$groundwater_total[multivariate$cluster_location == "Cedar City"] <- subset(df, GEOID == "49021")$groundwater_total
multivariate$groundwater_total[multivariate$cluster_location == "Colorado Springs"] <- subset(df, GEOID == "08041")$groundwater_total
multivariate$groundwater_total[multivariate$cluster_location == "Denver"] <- sum(subset(df, 
                                                                                        GEOID == "08005" |
                                                                                          GEOID == "08001" | 
                                                                                          GEOID == "08013" |
                                                                                          GEOID == "08014" |
                                                                                          GEOID == "08059" |
                                                                                          GEOID == "08031")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "Dallas Fort Worth"] <- sum(subset(df, 
                                                                                                   GEOID == "48085" |
                                                                                                     GEOID == "48113" | 
                                                                                                     GEOID == "48139" |
                                                                                                     GEOID == "48251" |
                                                                                                     GEOID == "48439")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "Flagstaff"] <- subset(df, GEOID == "04005")$groundwater_total
multivariate$groundwater_total[multivariate$cluster_location == "Gainesville"] <- subset(df, GEOID == "12001")$groundwater_total
multivariate$groundwater_total[multivariate$cluster_location == "Lawrence"] <- subset(df, GEOID == "20045")$groundwater_total
multivariate$groundwater_total[multivariate$cluster_location == "Los Angeles"] <- sum(subset(df, 
                                                                                             GEOID == "06037" |
                                                                                               GEOID == "06065" | 
                                                                                               GEOID == "06071" |
                                                                                               GEOID == "06073")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "La Crosse"] <- sum(subset(df, 
                                                                                           GEOID == "55063" |
                                                                                             GEOID == "27055" | 
                                                                                             GEOID == "27169")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "Morristown"] <- sum(subset(df, 
                                                                                            GEOID == "34013" |
                                                                                              GEOID == "34027" | 
                                                                                              GEOID == "34035" |
                                                                                              GEOID == "34039")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "Minneapolis"] <- sum(subset(df, 
                                                                                             GEOID == "27003" |
                                                                                               GEOID == "27053" | 
                                                                                               GEOID == "27123")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "Nashville"] <- sum(subset(df, 
                                                                                           GEOID == "47021" |
                                                                                             GEOID == "47037" | 
                                                                                             GEOID == "47149" |
                                                                                             GEOID == "47187")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "Phoenix"] <- sum(subset(df, 
                                                                                         GEOID == "04013" |GEOID == "04021")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "Portland"] <- sum(subset(df, 
                                                                                          GEOID == "41005" |
                                                                                            GEOID == "53011" | 
                                                                                            GEOID == "27123")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "State College"] <- subset(df, GEOID == "42027")$groundwater_total
multivariate$groundwater_total[multivariate$cluster_location == "San Diego"] <- subset(df, GEOID == "06073")$groundwater_total
multivariate$groundwater_total[multivariate$cluster_location == "San Francisco"] <- sum(subset(df, 
                                                                                               GEOID == "06001" |
                                                                                                 GEOID == "06013" | 
                                                                                                 GEOID == "06041" |
                                                                                                 GEOID == "06075" | 
                                                                                                 GEOID == "06081" | 
                                                                                                 GEOID == "06085")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "Salt Lake City"] <- sum(subset(df, 
                                                                                                GEOID == "49011" |
                                                                                                  GEOID == "49035")$groundwater_total)
multivariate$groundwater_total[multivariate$cluster_location == "San Marcos"] <- subset(df, GEOID == "48209")$groundwater_total
multivariate$groundwater_total[multivariate$cluster_location == "St Petersburg"] <- subset(df, GEOID == "12103")$groundwater_total
multivariate$groundwater_total[multivariate$cluster_location == "Wooster"] <- subset(df, GEOID == "39169")$groundwater_total

multivariate <- multivariate %>% select(-c(geometry))
write.csv(multivariate, "data/multivariate.csv")
