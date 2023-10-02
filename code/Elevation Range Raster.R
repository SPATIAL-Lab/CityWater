# No previous scripts needed, but this isn't working. 
library(raster); library(viridis);library(ggplot2); library(dplyr); 
library(tidyr); library(sf); library(terra); library(tictoc)

# setup, skip now
library(elevatr)

precip <- raster("data/PRISM_ppt_30yr_normal_4kmM3_annual_asc.asc")
elevation2 <- get_elev_raster(precip, z = 5, neg_to_na = T)

crs(elevation) <- "EPSG:9822"
crs(precip) <- "EPSG:9822"
elevation <- raster::resample(elevation, precip)
elevation <- mask(elevation, precip)
plot(elevation)
writeRaster(elevation, file = "data/elevationRaster.tif", overwrite = T)


# Trying to get min-max ---------------------------------------------------

library(terra); library(tidyr); library(dplyr); library(ggplot2); library(tidyterra)
elevation <- rast("data/elevationRaster2.tif")
# wait fuck what if I use terra's built in TRI/roughness indices
e1 <- terrain(elevation, "roughness")
  
e1 <- terra::as.data.frame(elevation, xy = TRUE, na.rm = T)
evect <- vect(e, geom=c("x", "y"), crs = elevation)
b <- buffer(evect, 20000)
e_min <- terra::extract(elevation, b, min, na.rm=TRUE)
emin <- e_min %>% 
  rename(e_min = elevationRaster2)
a <- cbind(e, emin)
write.csv(a,file = 'data/elevation_min.csv')

e_max <- terra::extract(elevation, b, max, na.rm=TRUE)
e_max <- e_max %>% 
  rename(e_max = elevationRaster2)
a <- cbind(e, e_max)
write.csv(a,file = 'data/elevation_max.csv')

#Combining min and max to get range of values within 20km areas
e_min <- read.csv('data/elevation_min.csv')
e_max <- read.csv('data/elevation_max.csv')
elevation <- rast("data/elevationRaster2.tif")

elevation_range <- left_join(e_max, e_min, join_by(x, y))
elevation_range$range <- elevation_range$maximum - elevation_range$minimum
elevation_range <- elevation_range %>% select(x, y, range)
write.csv(elevation_range, file = 'data/elevation_range.csv')

elerange <- vect(elevation_range, geom=c("x", "y"), crs = elevation)
elerange <- project(elerange, 'EPSG:4326')

plot(elerange)

ggplot(elerange)+ 
  geom_spatvector(aes(color = range))

# getting these study calculated with terra

library(tidyverse);library(tigris, options(tigris_use_cache = TRUE))
library(viridis);library(terra)

#using only those counties we have data for
#calculating Hawaiian data, but not including in combined datasets as streamflow is missing
AA <- counties("MI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Washtenaw")) %>% 
  vect()%>% 
  project(elevation) %>% 
  buffer(20000) 

ABQ <- counties("NM", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Bernalillo")) %>% 
  vect()%>% 
  project(elevation) %>% 
  buffer(20000)

ATH <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clarke","Oconee")) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

ATL <- counties("GA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cobb", "DeKalb", "Fulton"))%>% 
  vect()%>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

BEL <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Whatcom"))%>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

CED <- counties("UT", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Iron")) %>%
  vect()%>% 
  project(elevation) %>% 
  buffer(20000)

COL <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("El Paso")) %>% 
  vect()%>% 
  project(elevation) %>% 
  buffer(20000)

DFW <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Collin County", "Dallas", "Ellis", "Johnson",
                     "Tarrant"))%>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

DEN <- counties("CO", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Adams", "Arapahoe", "Boulder","Broomfield",
                     "Denver", "Jefferson"))%>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

FLG <- counties("AZ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Coconino"))%>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

GNV <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alachua"))%>%
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

HI <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hawaii"))%>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

LCR <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Houston", "Winona"))
LCR2 <- counties("WI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("La Crosse"))
LCR3 <- rbind(LCR, LCR2)
LCR <- vect(LCR) %>% aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)
rm(LCR2, LCR3)

LAW <- counties("KS", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Douglas"))%>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

LAX <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Los Angeles", "Riverside",
                     "San Bernardino","San Diego"))%>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

MSP <- counties("MN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Anoka", "Hennepin", "Ramsey"))%>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

MOR <- counties("NJ", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Essex", "Morris", "Somerset", "Union"))%>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

NAS <- counties("TN", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Cheatham", "Davidson", "Rutherford", "Williamson"))%>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

OA <- counties("HI", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Honolulu"))%>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

PHX <- counties("AZ", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Maricopa", "Pinal")) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

PTD <- counties("OR", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clackamas", "Multnomah"))
PTD2 <- counties("WA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Clark"))
PTD3 <- rbind(PTD, PTD2)
PTD <-vect(PTD) %>% aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)
rm(PTD2, PTD3)

SLC <- counties("UT", cb = TRUE, resolution = "20m") %>% 
  filter(NAME %in% c("Salt Lake", "Davis")) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

SD <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("San Diego")) %>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

SF <- counties("CA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Alameda","Contra Costa", "Marin", "San Francisco",
                     "San Mateo","Santa Clara")) %>% 
  vect() %>% 
  aggregate()%>% 
  project(elevation) %>% 
  buffer(20000)

SM <- counties("TX", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Hays"))%>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

SP <- counties("FL", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Pinellas"))%>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

SC <- counties("PA", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Centre"))%>% 
  vect()%>% 
  project(elevation) %>% 
  buffer(20000)

WOO <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Wayne"))%>% 
  vect() %>% 
  project(elevation) %>% 
  buffer(20000)

a <- c("AA", "ABQ", "ATH", "ATL", "BEL", "CED", "COL", "DEN", "DFW", "FLG", 
                  "GNV", "LAW", "LAX", "LCR", "HI", "MSP", "MOR", "NAS", "OA", "PHX", "PTD", "SC", 
                  "SD", "SF", "SLC", "SM", "SP", "WOO")

expandedArea <- c(AA, ABQ, ATH, ATL, BEL, CED, COL, DEN, DFW, FLG, 
                     GNV, LAW, LAX, LCR, HI, MSP, MOR, NAS, OA, PHX, PTD, SC, 
                     SD, SF, SLC,  SM, SP,  WOO)
names(expandedArea) <- a

e_min <- terra::extract(elevation, WOO, min, na.rm=TRUE)

for(i in a){
e_min <- terra::extract(elevation, i, min, na.rm=TRUE)
}

for(i in a){
  
}