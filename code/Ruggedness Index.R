# what the fuck is TRI
library(terra); library(dplyr); library(tidyr)

elevation <- rast("data/elevationRaster2.tif")

tri <- function(x, ...) {
  x <- x[!is.na(x)]
  return( sqrt(sum(((median(x) - x)^2))) )
}

t <- terra::extract(elevation, c, tri, na.rm = T) %>% 
  rename(ruggedness = elevationRaster2)

# Each developed area -----------------------------------------------------

AA <- counties("MI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Washtenaw")) %>% 
  mutate(ID = 'AA') %>% 
  select(ID) %>% 
  vect()%>% 
  project(elevation) %>% 
  buffer(20000) 

ABQ <- counties("NM", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Bernalillo")) %>% 
  mutate(ID = 'ABQ') %>% 
  select(ID) %>% 
  vect()%>% 
  project(elevation) %>% 
  buffer(20000)

ATH <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clarke","Oconee")) %>% 
  mutate(ID = 'ATH') %>% 
  select(ID) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

ATL <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cobb", "DeKalb", "Fulton"))%>% 
  mutate(ID = 'ATL') %>% 
  select(ID) %>% 
  vect()%>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

BEL <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Whatcom"))%>% 
  mutate(ID = 'BEL') %>% 
  select(ID) %>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

CED <- counties("UT", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Iron")) %>%
  mutate(ID = 'BEL') %>% 
  select(ID) %>% 
  vect()%>% 
  project(elevation) %>% 
  buffer(20000)

COL <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("El Paso")) %>% 
  mutate(ID = 'COL') %>% 
  select(ID) %>% 
  vect()%>% 
  project(elevation) %>% 
  buffer(20000)

DFW <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Collin County", "Dallas", "Ellis", "Johnson",
                     "Tarrant"))%>% 
  mutate(ID = 'DFW') %>% 
  select(ID) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

DEN <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Adams", "Arapahoe", "Boulder","Broomfield",
                     "Denver", "Jefferson"))%>% 
  mutate(ID = 'DEN') %>% 
  select(ID) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

FLG <- counties("AZ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Coconino"))%>% 
  mutate(ID = 'FLG') %>% 
  select(ID) %>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

GNV <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alachua"))%>%
  mutate(ID = 'GNV') %>% 
  select(ID) %>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

HI <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hawaii"))%>% 
  mutate(ID = 'HI') %>% 
  select(ID) %>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

LCR <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Houston", "Winona")) %>% 
  mutate(ID = 'LCR') %>% 
  select(ID)
LCR2 <- counties("WI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("La Crosse")) %>%   mutate(ID = 'LCR') %>% 
  select(ID)
LCR3 <- rbind(LCR, LCR2)
LCR <-  vect(LCR3) %>% aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)
rm(LCR2, LCR3)

LAW <- counties("KS", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Douglas"))%>% 
  mutate(ID = 'LAW') %>% 
  select(ID) %>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

LAX <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Los Angeles", "Riverside",
                     "San Bernardino","San Diego"))%>% 
  mutate(ID = 'LAX') %>% 
  select(ID) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

MSP <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Anoka", "Hennepin", "Ramsey"))%>% 
  mutate(ID = 'MSP') %>% 
  select(ID) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

MOR <- counties("NJ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Essex", "Morris", "Somerset", "Union"))%>% 
  mutate(ID = 'MOR') %>% 
  select(ID) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

NAS <- counties("TN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cheatham", "Davidson", "Rutherford", "Williamson"))%>% 
  mutate(ID = 'NAS') %>% 
  select(ID) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

OA <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Honolulu"))%>%   
  mutate(ID = 'OA') %>% 
  select(ID) %>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

PHX <- counties("AZ", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Maricopa", "Pinal")) %>% 
  mutate(ID = 'PHX') %>% 
  select(ID) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

PTD <- counties("OR", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clackamas", "Multnomah"))%>% 
  mutate(ID = 'PTD') %>% 
  select(ID)
PTD2 <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clark"))%>% 
  mutate(ID = 'PTD') %>% 
  select(ID)
PTD3 <- rbind(PTD, PTD2) 
PTD <- vect(PTD3) %>% aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)
rm(PTD2, PTD3)

SLC <- counties("UT", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Salt Lake", "Davis")) %>% 
  mutate(ID = 'SLC') %>% 
  select(ID) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

SD <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("San Diego")) %>% 
  mutate(ID = 'SD') %>% 
  select(ID) %>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

SF <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alameda","Contra Costa", "Marin", "San Francisco",
                     "San Mateo","Santa Clara")) %>% 
  mutate(ID = 'SF') %>% 
  select(ID) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

SM <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hays"))%>% 
  mutate(ID = 'SM') %>% 
  select(ID) %>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

SP <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Pinellas"))%>% 
  mutate(ID = 'SP') %>% 
  select(ID) %>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

SC <- counties("PA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Centre"))%>% 
  mutate(ID = 'SC') %>% 
  select(ID) %>% 
  vect()%>% 
  project(elevation) %>% 
  buffer(20000)

WOO <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Wayne"))%>% 
  mutate(ID = 'WOO') %>% 
  select(ID) %>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

expandedArea <- rbind(AA, ABQ, ATH, ATL, BEL, CED, COL, DEN, DFW, FLG, 
                  GNV, LAW, LAX, LCR, MSP, MOR, NAS, PHX, PTD, SC, 
                  SD, SF, SLC,  SM, SP,  WOO)
a <- c("AA", "ABQ", "ATH", "ATL", "BEL", "CED", "COL", "DEN", "DFW", "FLG", 
       "GNV", "LAW", "LAX", "LCR", "MSP", "MOR", "NAS", "PHX", "PTD", "SC", 
       "SD", "SF", "SLC", "SM", "SP", "WOO")
names(expandedArea) <- a


trimean <- terra::extract(elevation, expandedArea, mean, na.rm = T) %>% 
  rename(mean = elevationRaster2) 
trimean$mean <- round(trimean$mean)
trimean <- cbind(trimean, a)

trimad <- terra::extract(elevation, expandedArea, tri, na.rm = T) %>% 
  rename(mad = elevationRaster2) 
trimad$mad <- round(trimad$mad)
compare <- cbind(trimad, trimean) %>% select(-c(ID))

conus <- counties(cb = TRUE)
conus$STATEFP <- as.numeric(conus$STATEFP)
conus <- subset(conus, STATEFP < 60)
conus <- subset(conus, STATEFP != 02)
conus <- subset(conus, STATEFP != 15)
conus <- conus %>% select(geometry, NAME)
conus <- vect(conus)
c <- project(conus, elevation)
c$tri <- data.frame(terra::extract(elevation, conus, tri, na.rm = T))


