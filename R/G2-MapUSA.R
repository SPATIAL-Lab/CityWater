#Prep & check data
data("us_states") #load data from spData
plot(us_states$geometry) #check geom
st_crs(us_states) #check CRS 
us_states <- st_transform(us_states, crs = 4326) #set WGS84


mapFull <- read.csv("data/mapFull.csv", na.strings = "NA")

mapFull.sf <- st_as_sf(mapFull, 
                       coords = c("Long", "Lat"), #xLong yLat
                       crs = 4326) #EPSG code for WGS84!


#Just CONUS spatial object
mapFull.sf_CONUS <- mapFull.sf %>%
  filter(Location != "Hawaii") %>%
  filter(Location != "Oahu")

#CONUS bubble map based on oxygen sd
CONUS_sd<-tm_shape(us_states) + 
  tm_borders(col = "black") +
  tm_fill(col = "grey", alpha = 0.2) +
  tm_shape(mapFull.sf_CONUS) + 
  tm_symbols(col = "Variability", size = "sd_d18O", 
             scale = 2.5, alpha = .8, 
             palette = c("red", "blue"), border.col = "black") +
  tm_layout(frame = FALSE) +
  tm_legend(outside = TRUE) 

tmap_save(CONUS_sd, "out/CONUS_sd.TIFF")


#Hawaii bubble map based on oxygen sd
mapFull.sf_Islands <- mapFull.sf %>%
  filter(Location %in% c("Hawaii", "Oahu"))

HW_sd<-tm_shape(hawaii) + 
  tm_borders(col = "black") +
  tm_fill(col = "grey", alpha = 0.2) +
  tm_shape(mapFull.sf_Islands) + 
  tm_symbols(col = "Variability", size = "sd_d18O", scale = 2.5, alpha = .8, 
             palette = c("red", "blue"), border.col = "black") +
  tm_layout(frame = FALSE) +
  tm_legend(outside = TRUE)  

tmap_save(HW_sd, "out/HW_sd.TIFF")
