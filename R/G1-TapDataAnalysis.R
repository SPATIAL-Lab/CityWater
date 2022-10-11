#Run eveything!
library(ggplot2);library(raster); library(RColorBrewer);library(sf);
library(viridis);library(rgdal);library(mapview);library(leaflet);
library(tmap);library(spData);library(ggpmisc);library(tidyverse);
library(magrittr);library(rio);library(rstatix);library(diptest);
library(multimode);library(factoextra);library(patchwork);library("readxl")

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

#keep going
names(tapData)
tapData$Cluster_ID <- factor(tapData$Cluster_ID)
tapData$Cluster_Location <- factor(tapData$Cluster_Location)
tapData$Cluster_Location_Time <- factor(tapData$Cluster_Location_Time)
tapData$Cluster_State <- factor(tapData$Cluster_State)
tapData$Project_ID <- factor(tapData$Project_ID)
tapData$Modality <- factor(tapData$Modality)
levels(tapData$Cluster_ID)
levels(tapData$Cluster_Location)
levels(tapData$Cluster_Location_Time)
levels(tapData$Modality)
str(tapData)

#going spatial
tapData.sf <- st_as_sf(tapData, 
                        coords = c("Long", "Lat"),
                        crs = 4326) #EPSG code for WGS84!
str(tapData.sf)

#removing NC -non clustered- data
tapData.sf_1 <- tapData.sf[tapData.sf$Cluster_ID != "NC", ]
