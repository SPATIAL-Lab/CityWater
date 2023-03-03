#Initial cleanup so there's a publication-ready .csv to work from. 
library(tidyverse);library(readxl);library(sp);library(maps);library(maptools); 
library(elevatr)

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
tapData$Cluster_Location = gsub("LaCrosse", "La Crosse", tapData$Cluster_Location)

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

# Typo
tapData$Sample_Comments = gsub("temporarily", "temporally", tapData$Sample_Comments)

# Changing Cluster_ID 1.1.0 to 1.10
tapData$Cluster_ID = gsub("1.1.0", "1.10", tapData$Cluster_ID)
tapData <- subset(tapData, Cluster_ID != "NC")

tapData <- subset(tapData, Cluster_ID != "NC")

# Let's create d-excess for tapData
tapData$d_ex <- (tapData$d2H - 8 * tapData$d18O)


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

##############
# ELEVATION
#################
#Because not all collected data has elevation information, we'll instead pull elevation
#using elevatr package
elevation_USGS <- get_elev_point(xy, prj = 4326, src = "epqs")
tapData$Elevation <- elevation_USGS$Elevation

plot(tapData$Elevation_mabsl, tapData$Elevation)
#okay, we see a few things: 
# a) lots of data where the elevation inputted was falsely zero. Mostly by Tipple
# b) a line where the two elevation columns don't follow- because elevation_mabsl 
# is in feet...
#c) some are one to one
tapData <- tapData[ -c(8, 24) ]
write.csv(tapData, "data/cityWater.csv")
