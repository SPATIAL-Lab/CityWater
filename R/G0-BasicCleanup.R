#Initial cleanup so there's a publication-ready .csv to work from. 
library(viridis);library(rgdal);library(mapview);library(leaflet);
library(tmap);library(spData);library(ggpmisc);library(tidyverse);
library("readxl")

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

#
tapData$Cluster_State = gsub("NC", NA, tapData$Cluster_State)
tapData <- select(tapData, -c("Modality"))
tapData$Sample_Comments = gsub("temporarily", "temporally", tapData$Sample_Comments)

#changing Cluster_ID 1.1.0 to 1.10
tapData$Cluster_ID = gsub("1.1.0", "1.10", tapData$Cluster_ID)

write.csv(tapData, "data/cityWater.csv")
