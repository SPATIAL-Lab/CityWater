# Let's see what the rest of Ohio is up to. 
library(factoextra); library(readr); library(forcats); library(dplyr); 
library(tidyr); library(maptools); library(elevatr); library(stringr); 
library(maps); library(viridis)

WOO <- read_csv("data/cityWater.csv", 
                    col_types = cols(cluster_ID = col_character())) %>% 
  filter(cluster_location_time == "Wooster")

CLE <- read_csv("data/OhioNoWooster.csv")


# Cleveland data will need some extra info: counties and elevation

# Let's assign counties to each datapoint by coordinates
# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

xy <- data.frame(x = CLE$lon, y = CLE$lat)

CLE$county <- latlong2county(xy)
CLE$county <- gsub(".*,", "", CLE$county)
CLE$county <- str_to_title(CLE$county) 

# Elevation ---------------------------------------------------------------
elevation_USGS <- get_elev_point(xy, prj = 4326, src = "aws")
CLE$elevation <- elevation_USGS$elevation

# Final touches and binding -----------------------------------------------

CLE$cluster_location = "Cleveland"
CLE$state = 'Ohio'
CLE$sample_ID <- as.character(CLE$sample_ID)
CLE$d_ex = CLE$d2H - (8*CLE$d18O)

oh <- bind_rows(CLE, WOO)

# First looks -------------------------------------------------------------
library(terra); library(tidyterra); library(ggplot2); library(tigris)

datasummary <- oh %>% 
  group_by(cluster_location) %>% 
  summarize(
    n = n(),
    IDR_O = abs(diff(quantile(.data$d18O, c(0.1, 0.9), names = F))), 
    IDR_d_ex = abs(diff(quantile(.data$d_ex, c(0.1, 0.9), names = F))), 
    IDR_H = abs(diff(quantile(.data$d2H, c(0.1, 0.9), names = F))),
  )


counties <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Geauga", "Portage", "Summit", "Wayne", "Medina", "Cuyahoga", 
                     "Mahoning", "Trumbull", "Ashtabula", "Lake")) %>% 
  vect() #%>% 
  #terra::aggregate()
ohvect <- vect(oh, crs= crs(counties), keep = T)

ggplot() +  
  geom_sf(data = counties) + 
  #geom_sf(data = ohvect, aes(color = d18O), size = 3) + 
  geom_jitter(data = oh, aes(x = lon, y = lat, color = d18O), width = 0.01, size = 3) + 
  scale_color_viridis(discrete = F, option = 'mako') +
  theme_void()

ggplot() + 
  geom_boxplot(data = oh, aes(x = cluster_location, y = d18O)) + 
  theme_classic()

ggplot() + 
  geom_point(data = oh, aes(x = d18O, y = d2H, color = cluster_location), size = 3) + 
  scale_color_manual(values = c("#003f5c", "#d2042d")) + 
  theme_classic()

# Cluster re-analysis -----------------------------------------------------

km_OH <- oh %>%
  select(c(6, 5)) %>% 
  eclust("kmeans", nboot = 500)
oh$km_cluster <- factor(km_OH$cluster)
# okay so the number of clusters doesn't change

km_CLE <- subset(oh, cluster_location != "Wooster") %>%
  select(c(6, 5)) %>% 
  eclust("kmeans", nboot = 500)
oh$km_cluster <- factor(km_OH$cluster)

cities <- vect("maps/cb_2018_us_ua10_500k.shp")
citycheck <- as.data.frame(cities)

ggplot()+ 
  geom_sf(data = counties, fill = 'white') + 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Cleveland, OH"), fill = "#003f5c") + 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Wooster, OH"), fill =  "#d2042d") + 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Akron, OH"), fill = 'goldenrod') + 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Youngstown, OH--PA"), fill = 'hotpink') + 
  #geom_sf(data = ohvect, aes(color = d18O), size = 3) + 
  geom_jitter(data = oh, aes(x = lon, y = lat), width = 0.01, size = 3) + 
  scale_color_viridis(discrete = T, option = 'mako') +
  theme_void()
# you can see how we have a lot of samples well outside of Cleveland. There's an argument of cachement and whether these are suburbs


# Cutting Proposal --------------------------------------------------------
# I would think we keep Wooster on its own, and then cut the 'Cleveland' samples to those in counties that are at least 
# possibly serviced by Cleveland's water supply (accepting that urban cachement versus water supply are different things)

counties2 <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Geauga", "Portage", "Summit", "Medina", "Cuyahoga")) %>% 
  vect() #%>% 
#terra::aggregate()
ohvect2 <- mask(ohvect, counties2)

plot(ohvect2)
ggplot()+ 
  geom_sf(data = counties2, fill = 'white') + 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Cleveland, OH"), fill = "#003f5c") + 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Akron, OH"), fill = 'goldenrod') + 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Youngstown, OH--PA"), fill = 'darkred') + 
  geom_sf(data = ohvect2, aes(color = d18O), size = 3) + 
  scale_color_viridis(discrete = F, option = 'mako') +
  theme_void()

CLE2 <- as.data.frame(ohvect2)
CLE2$d_ex = CLE2$d2H - (8*CLE2$d18O)
oh2 <- bind_rows(CLE2, WOO)

ggplot() + 
  geom_point(data = oh2, aes(x = d18O, y = d2H, color = cluster_location), size = 3) + 
  scale_color_manual(values = c("#003f5c", "#d2042d")) + 
  theme_classic()

datasummary <- oh2 %>% 
  group_by(cluster_location) %>% 
  summarize(
    n = n(),
    IDR_O = abs(diff(quantile(.data$d18O, c(0.1, 0.9), names = F))), 
    IDR_d_ex = abs(diff(quantile(.data$d_ex, c(0.1, 0.9), names = F))), 
    IDR_H = abs(diff(quantile(.data$d2H, c(0.1, 0.9), names = F))),
  )

# Temporal Variability
# We have two sampling bouts, Jan-Feb 2018 and Jul-Aug 2018. So let's work through that. 

CLE_time1 <- CLE2[CLE2$cluster_location_time == "Cleveland_Jan-Feb-18", ]
CLE_time2 <- CLE2[CLE2$cluster_location_time == "Cleveland_Jul-Aug-18", ]

km_CLE_time1 <- CLE_time1 %>%
  select(c(6, 5)) %>% 
  eclust("kmeans", nboot = 500)
CLE_time1$km_cluster <- factor(km_CLE_time1$cluster)

km_CLE_time2 <- CLE_time2 %>%
  select(c(6, 5)) %>% 
  eclust("kmeans", nboot = 500)
CLE_time2$km_cluster <- factor(km_CLE_time2$cluster)

CLE3 <- rbind(CLE_time1, CLE_time2)
# hrmmm, sampling bout 1 has huge differences in values

table(CLE3$km_cluster, CLE3$type)

ggplot()+ 
  geom_sf(data = counties2, fill = 'grey50') + 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Cleveland, OH"), fill = "#003f5c") + 
  #geom_sf(data = terra::subset(cities, cities$NAME10 == "Akron, OH"), fill = 'goldenrod') + 
  #geom_sf(data = ohvect2, aes(color = cluster_location_time), size = 3) + 
  geom_jitter(data = CLE3, aes(x = lon, y = lat, color = km_cluster), 
              size = 3, width = 0.01) +
  scale_color_viridis(discrete = T, option = 'mako') +
  theme_void()



write.csv(CLE3, 'data/Cleveland.csv')


# Cleaning up Cleveland new data ------------------------------------------
library(dplyr); library(ggplot2); library(terra); library(tidyterra); library(tidyr)
CLE <- read.csv('data/OhioNoWooster.csv')

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

xy <- data.frame(x = CLE$lon, y = CLE$lat)

CLE$county <- latlong2county(xy)
CLE$county <- gsub(".*,", "", CLE$county)
CLE$county <- str_to_title(CLE$county) 

# Elevation ---------------------------------------------------------------
elevation_USGS <- get_elev_point(xy, prj = 4326, src = "aws")
CLE$elevation <- elevation_USGS$elevation

counties <- counties("OH", cb = T, resolution = "20m") %>% 
  filter(NAME %in% c("Geauga", "Portage", "Summit", "Wayne", "Medina", "Cuyahoga", 
                     "Mahoning", "Trumbull", "Ashtabula", "Lake")) %>% 
  vect() #%>% 
#terra::aggregate()
ohvect <- vect(oh, crs= crs(counties), keep = T)


# They're in the system, now what? ----------------------------------------

tapData <- read_csv("data/cityWater.csv", 
                    col_types = cols(cluster_ID = col_character()))

ggplot() + 
  geom_density(data = subset(tapData, cluster_location == "Cleveland-Akron" | cluster_location == "Youngstown"), 
               aes(x = d18O, fill = cluster_location), alpha = 0.5) + 
  scale_fill_manual(values = c("#003f5c","#84edff"))

YNG <- subset(tapData, cluster_location == "Youngstown")
CED <- subset(tapData, cluster_location == 'Cedar City')

ggplot()+ 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Youngstown, OH--PA"), fill = 'lightgrey') + 
  #geom_sf(data = ohvect, aes(color = d18O), size = 3) + 
  geom_jitter(data = YNG, aes(x = lon, y = lat, color = d18O), width = 0.01, size = 3) + 
  scale_color_viridis(discrete = F, option = 'mako') +
  labs(title = "City of Youngstown with d18O values of tap water") +
  theme_void()

ggplot()+ 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Cedar City, UT"), fill = 'lightgrey') + 
  #geom_sf(data = ohvect, aes(color = d18O), size = 3) + 
  geom_jitter(data = CED, aes(x = lon, y = lat, color = d18O), width = 0.01, size = 3) + 
  scale_color_viridis(discrete = F, option = 'mako') +
  labs(title = "City of Cedar City with d18O values of tap water") +
  theme_void()


# Why is Ohio like this ---------------------------------------------------


library(tidyr); library(dplyr); library(leaps); library(ggpubr); library(car)                      

datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(2:4, 6:13, 16, 17)]
multivariate <- read.csv("data/multivariate.csv") %>% 
  filter(cluster_location != "Oahu" & cluster_location != "Hawaii")
multilevel <- left_join(multivariate, datasummary, by = 'cluster_location') %>% 
  rename('sd' = 'd18O_sd', 
         'idr' = 'IDR_O')

ggplot() + 
  geom_col(data = multilevel, aes(x = fct_infreq(cluster_location), y = medincome))
