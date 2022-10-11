#Run eveything!
library(ggplot2);library(raster); library(RColorBrewer);library(sf);
library(viridis);library(rgdal);library(mapview);library(leaflet);
library(tmap);library(spData);library(ggpmisc);library(tidyverse);
library(magrittr);library(rio);library(rstatix);library(diptest);
library(multimode);library(factoextra);library(patchwork);library("readxl")

###Data import & prep###
tapData <- read.csv("data/tapData.csv", na.strings = "NA")
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
