#Initial cleanup so there's a publication-ready .csv to work from. 
library(tidyverse);library(readxl);library(sp);library(maps);library(maptools)

###Data import & prep###
tapData <- read.csv("data/tapData.csv", na.strings = "NA")

#update some names
tapData$Cluster_Location = gsub("Ann_Arbor", "Ann Arbor", tapData$Cluster_Location)
tapData$Cluster_Location = gsub("MBS", "Morristown", tapData$Cluster_Location)
tapData$Cluster_Location = gsub("SC", "State College", tapData$Cluster_Location)
tapData$Cluster_Location = gsub("SLC_Area", "Salt Lake City", tapData$Cluster_Location)
tapData$Cluster_Location = gsub("Colorado_Springs", "Colorado Springs", tapData$Cluster_Location)
tapData$Cluster_Location = gsub("Cedar_City", "Cedar City", tapData$Cluster_Location)
tapData$Cluster_Location = gsub("San_Francisco", "San Francisco", tapData$Cluster_Location)
tapData$Cluster_Location = gsub("Los_Angeles", "Los Angeles", tapData$Cluster_Location)
tapData$Cluster_Location = gsub("DallasForthWard", "Dallas Fort Worth", tapData$Cluster_Location)
tapData$Cluster_Location = gsub("San_Diego", "San Diego", tapData$Cluster_Location)
tapData$Cluster_Location = gsub("San Petersburgo", "St Petersburg", tapData$Cluster_Location)

tapData$Cluster_Location_Time = gsub("Ann_Arbor", "Ann Arbor", tapData$Cluster_Location_Time)
tapData$Cluster_Location_Time = gsub("MBS", "Morristown", tapData$Cluster_Location_Time)
tapData$Cluster_Location_Time = gsub("SC", "State College", tapData$Cluster_Location_Time)
tapData$Cluster_Location_Time = gsub("SLC_Area", "Salt Lake City", tapData$Cluster_Location_Time)
tapData$Cluster_Location_Time = gsub("Colorado_Springs", "Colorado Springs", tapData$Cluster_Location_Time)
tapData$Cluster_Location_Time = gsub("Cedar_City", "Cedar City", tapData$Cluster_Location_Time)
tapData$Cluster_Location_Time = gsub("San_Francisco", "San Francisco", tapData$Cluster_Location_Time)
tapData$Cluster_Location_Time = gsub("Los_Angeles", "Los Angeles", tapData$Cluster_Location_Time)
tapData$Cluster_Location_Time = gsub("DallasForthWard", "Dallas Fort Worth", tapData$Cluster_Location_Time)
tapData$Cluster_Location_Time = gsub("San_Diego", "San Diego", tapData$Cluster_Location_Time)
tapData$Cluster_Location_Time = gsub("San Petersburgo", "St Petersburg", tapData$Cluster_Location_Time)

# Removing NC as a Cluster_State so there's no future confusion with North Carolina (not currently in the system)
tapData$Cluster_State = gsub("NC", NA, tapData$Cluster_State)
# Removing "Modality" since it was added to the data after the fact. Let's find a way to assign modality using the script
tapData <- select(tapData, -c("Modality"))
# Typo
tapData$Sample_Comments = gsub("temporarily", "temporally", tapData$Sample_Comments)

tapData$County <- ifelse(tapData$Cluster_Location == "Hawaii", 
                         "Hawaii",
                         tapData$County)
tapData$County <- ifelse(tapData$Cluster_Location == "Oahu", 
                         "Oahu",
                         tapData$County)

# Changing Cluster_ID 1.1.0 to 1.10
tapData$Cluster_ID = gsub("1.1.0", "1.10", tapData$Cluster_ID)

tapData <- subset(tapData, Cluster_ID != "NC")

write.csv(tapData, "data/cityWater.csv")

#### Let's prepare CoVariates for import to do some multilevel regression
covariates <- read_excel("data/desc_stats1.xlsx", 
    sheet = "CoVariates", col_types = c("skip", 
       "text", "skip", "skip", "text", "text", 
       "skip", "skip", "skip", "numeric", 
       "numeric", "skip", "numeric", "numeric", 
       "numeric", "text", "skip", "numeric", 
       "numeric", "numeric"))

covariates$GEOID <- str_pad(covariates$GEOID, 6, pad = "0")
covariates$COUNTYFP <- str_pad(covariates$COUNTYFP, 3, pad = "0")
################
# COUNTIES
################
#### Let's assign counties to each datapoint by coordinates
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

xy <- data.frame(x = tapData$Long, y = tapData$Lat)

tapData$County <- latlong2county(xy)
tapData$County <- gsub(".*,", "", tapData$County)
tapData$County <- str_to_title(tapData$County) 

################
# STATES
################
#Let's try to ID states from coordinates
latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

tapData$State <- latlong2state(xy)
tapData$State <- gsub(".*,", "", tapData$State)
tapData$State <- str_to_title(tapData$State) 

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
xy <- data.frame(x = tapData$Long, y = tapData$Lat)

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

