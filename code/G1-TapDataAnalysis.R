# This script sets up everything for the following scripts, run completely. 
library(raster); library(sf); library(viridis);library(rgdal);library(mapview);
library(leaflet); library(spData);library(ggpmisc);library(usmap);
library(tidyverse); library(magrittr);library(rio);library(rstatix);
library(diptest); library(multimode);library(factoextra);library(patchwork); 
library(ggridges); library(hrbrthemes); library(patchwork)

###Data import & prep###
tapData <- read.csv("data/cityWater.csv", na.strings = "NA")

#keep going
#names(tapData)
tapData$Cluster_ID <- factor(tapData$Cluster_ID)
tapData$Cluster_Location <- factor(tapData$Cluster_Location)
tapData$Cluster_Location_Time <- factor(tapData$Cluster_Location_Time)
tapData$Cluster_State <- factor(tapData$Cluster_State)
tapData$Project_ID <- factor(tapData$Project_ID)
#Chris: I want to create reproducible way or how modality is assigned
# tapData$Modality <- factor(tapData$Modality)
levels(tapData$Cluster_ID)
levels(tapData$Cluster_Location)
levels(tapData$Cluster_Location_Time)

#levels(tapData$Modality)
str(tapData)

#going spatial
tapData.sf <- st_as_sf(tapData, 
                        coords = c("Long", "Lat"),
                        crs = 4326) #EPSG code for WGS84!
str(tapData.sf)

###Subsetting Cities & Interactive Maps####
#SLC
tapData.sf_SLC_1.1 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ap-13", ]
intMap_SLC_1.1 <- mapview(tapData.sf_SLC_1.1, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)  

tapData.sf_SLC_1.2 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ag-Oct-13", ]
intMap_SLC_1.2 <- mapview(tapData.sf_SLC_1.2, 
                          col.regions = "blue", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)  

tapData.sf_SLC_1.3 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Feb-14", ]
intMap_SLC_1.3 <- mapview(tapData.sf_SLC_1.3, 
                          col.regions = "yellow", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)  

tapData.sf_SLC_1.4 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ap-May-14", ]
intMap_SLC_1.4 <- mapview(tapData.sf_SLC_1.4, 
                          col.regions = "black", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)  

tapData.sf_SLC_1.5 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ag-Sep-14", ]
intMap_SLC_1.5 <- mapview(tapData.sf_SLC_1.5, 
                          col.regions = "aquamarine4", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_SLC_1.6 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ap-May-15", ]
intMap_SLC_1.6 <- mapview(tapData.sf_SLC_1.6, 
                          col.regions = "antiquewhite3", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_SLC_1.7 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Sep-Oct-15", ]
intMap_SLC_1.7 <- mapview(tapData.sf_SLC_1.7, 
                          col.regions = "chartreuse3", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_SLC_1.8 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ap-16", ]
intMap_SLC_1.8 <- mapview(tapData.sf_SLC_1.8, 
                          col.regions = "chocolate1", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_SLC_1.9 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Sep-16", ]
intMap_SLC_1.9 <- mapview(tapData.sf_SLC_1.9, 
                          col.regions = "chocolate4", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_SLC_1.10 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Mar-May-17", ]
intMap_SLC_1.10 <- mapview(tapData.sf_SLC_1.10, 
                            col.regions = "coral2", # fill color
                            color = "gray",          # outline color
                            alpha.regions = 0.5,     # fill transparency
                            alpha = 0.5)

tapData.sf_SLC_1.11 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Oct-17", ]
intMap_SLC_1.11 <- mapview(tapData.sf_SLC_1.11, 
                           col.regions = "blueviolet", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

intMap_SLC_1.1 + intMap_SLC_1.2 + intMap_SLC_1.3 + intMap_SLC_1.4 + 
  intMap_SLC_1.5 + intMap_SLC_1.6 + intMap_SLC_1.7 + intMap_SLC_1.8 + 
  intMap_SLC_1.9 + intMap_SLC_1.10 + intMap_SLC_1.11

#SAN FRANCISCO
tapData.sf_SF_25.1 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Dic-13", ]
intMap_SF_25.1 <- mapview(tapData.sf_SF_25.1, 
                          col.regions = "blueviolet", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_SF_25.2 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Mar-Ap-14", ]
intMap_SF_25.2 <- mapview(tapData.sf_SF_25.2, 
                          col.regions = "black", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_SF_25.3 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Jun-14", ]
intMap_SF_25.3 <- mapview(tapData.sf_SF_25.3, 
                          col.regions = "darkred", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_SF_25.4 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Nov-14", ]
intMap_SF_25.4 <- mapview(tapData.sf_SF_25.4, 
                          col.regions = "darkblue", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_SF_25.5 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Dic-14", ]
intMap_SF_25.5 <- mapview(tapData.sf_SF_25.5, 
                          col.regions = "pink", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_SF_25.6 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Mar-15", ]
intMap_SF_25.6 <- mapview(tapData.sf_SF_25.6, 
                          col.regions = "darkgreen", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_SF_25.7 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Jul-15", ]
intMap_SF_25.7 <- mapview(tapData.sf_SF_25.7, 
                          col.regions = "gold", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

intMap_SF_25.1 + intMap_SF_25.2 + intMap_SF_25.3 + intMap_SF_25.4 +
  intMap_SF_25.5 + intMap_SF_25.6 + intMap_SF_25.7


#PHOENIX
tapData.sf_PHO_26.1 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Phoenix_Mar-Ap-14", ]
intMap_PHO_26.1 <- mapview(tapData.sf_PHO_26.1, 
                           col.regions = "red", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

tapData.sf_PHO_26.2 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Phoenix_Oct-14", ]
intMap_PHO_26.2 <- mapview(tapData.sf_PHO_26.2, 
                           col.regions = "blue", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

intMap_PHO_26.1 + intMap_PHO_26.2

#SAN DIEGO
tapData.sf_SD_27.1 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Diego_Dic-13", ]
intMap_SD_27.1 <- mapview(tapData.sf_SD_27.1, 
                          col.regions = "blue", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_SD_27.2 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Diego_Ap-14", ]
intMap_SD_27.2 <- mapview(tapData.sf_SD_27.2, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

intMap_SD_27.1 + intMap_SD_27.2

#LOS ANGELES
tapData.sf_LA_28.1 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Los Angeles_Dic-13", ]
intMap_LA_28.1 <- mapview(tapData.sf_LA_28.1, 
                          col.regions = "blue", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_LA_28.2 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Los Angeles_Mar-Ap-14", ]
intMap_LA_28.2 <- mapview(tapData.sf_LA_28.2, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_LA_28.3 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Los Angeles_Nov-14", ]
intMap_LA_28.3 <- mapview(tapData.sf_LA_28.3, 
                          col.regions = "gold", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

intMap_LA_28.1 + intMap_LA_28.2 + intMap_LA_28.3


#OAHU
tapData.sf_Oahu_2 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Oahu", ]
intMap_Oahu_2 <- mapview(tapData.sf_Oahu_2, 
                         col.regions = "red", # fill color
                         color = "gray",          # outline color
                         alpha.regions = 0.5,     # fill transparency
                         alpha = 0.5)

#St Petersburg
tapData.sf_SanPete_3 <- tapData.sf[tapData.sf$Cluster_Location_Time == "St Petersburg", ]
intMap_SanPete_3 <- mapview(tapData.sf_SanPete_3, 
                            col.regions = "red", # fill color
                            color = "gray",          # outline color
                            alpha.regions = 0.5,     # fill transparency
                            alpha = 0.5)

#Gainesville
tapData.sf_Gaines_4 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Gainesville", ]
intMap_Gaines_4 <- mapview(tapData.sf_Gaines_4, 
                           col.regions = "red", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

#San Marcos
tapData.sf_SM_5 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Marcos", ]
intMap_SM_5 <- mapview(tapData.sf_SM_5, 
                       col.regions = "red", # fill color
                       color = "gray",          # outline color
                       alpha.regions = 0.5,     # fill transparency
                       alpha = 0.5)

#Dallas Fort Worth
tapData.sf_DF_6 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Dallas Fort Worth", ]
intMap_DF_6 <- mapview(tapData.sf_DF_6, 
                       col.regions = "red", # fill color
                       color = "gray",          # outline color
                       alpha.regions = 0.5,     # fill transparency
                       alpha = 0.5)

#Atlanta
tapData.sf_Atl_7 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Atlanta", ]
intMap_Atl_7 <- mapview(tapData.sf_Atl_7, 
                        col.regions = "red", # fill color
                        color = "gray",          # outline color
                        alpha.regions = 0.5,     # fill transparency
                        alpha = 0.5)

#Athens
tapData.sf_Ath_8 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Athens", ]
intMap_Ath_8 <- mapview(tapData.sf_Ath_8, 
                        col.regions = "red", # fill color
                        color = "gray",          # outline color
                        alpha.regions = 0.5,     # fill transparency
                        alpha = 0.5)

#Albuquerque
tapData.sf_ABQ_9 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Albuquerque", ]
intMap_ABQ_9 <- mapview(tapData.sf_ABQ_9, 
                        col.regions = "red", # fill color
                        color = "gray",          # outline color
                        alpha.regions = 0.5,     # fill transparency
                        alpha = 0.5)

#Flagstaff
tapData.sf_Flag_10 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Flagstaff", ]
intMap_Flag_10 <- mapview(tapData.sf_Flag_10, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

#Nashville
tapData.sf_Nashv_11 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Nashville", ]
intMap_Nashv_11 <- mapview(tapData.sf_Nashv_11, 
                           col.regions = "red", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

#Cedar City
tapData.sf_Cedar_12 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Cedar City", ]
intMap_Cedar_12 <- mapview(tapData.sf_Cedar_12, 
                           col.regions = "red", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

#Colorado Springs
tapData.sf_ColoSp_13 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Colorado Springs", ]
intMap_ColoSp_13 <- mapview(tapData.sf_ColoSp_13, 
                            col.regions = "red", # fill color
                            color = "gray",          # outline color
                            alpha.regions = 0.5,     # fill transparency
                            alpha = 0.5)

#Lawrence
tapData.sf_Law_14 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Lawrence", ]
intMap_Law_14 <- mapview(tapData.sf_Law_14, 
                         col.regions = "red", # fill color
                         color = "gray",          # outline color
                         alpha.regions = 0.5,     # fill transparency
                         alpha = 0.5)

#Denver
tapData.sf_Denv_15 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Denver", ]
intMap_Denv_15 <- mapview(tapData.sf_Denv_15, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

#Morristown
tapData.sf_Morristown_16 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Morristown", ]
intMap_Morristown_16 <- mapview(tapData.sf_Morristown_16, 
                         col.regions = "red", # fill color
                         color = "gray",          # outline color
                         alpha.regions = 0.5,     # fill transparency
                         alpha = 0.5)

#State College
tapData.sf_SC_17 <- tapData.sf[tapData.sf$Cluster_Location_Time == "State College", ]
intMap_SC_17 <- mapview(tapData.sf_SC_17, 
                        col.regions = "red", # fill color
                        color = "gray",          # outline color
                        alpha.regions = 0.5,     # fill transparency
                        alpha = 0.5)

#Wooster
tapData.sf_Woo_18 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Wooster", ]
intMap_Woo_18 <- mapview(tapData.sf_Woo_18, 
                         col.regions = "red", # fill color
                         color = "gray",          # outline color
                         alpha.regions = 0.5,     # fill transparency
                         alpha = 0.5)

#Ann Arbor
tapData.sf_Ann_19 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Ann Arbor", ]
intMap_Ann_19 <- mapview(tapData.sf_Ann_19, 
                         col.regions = "red", # fill color
                         color = "gray",          # outline color
                         alpha.regions = 0.5,     # fill transparency
                         alpha = 0.5)

#LaCrosse
tapData.sf_LaCro_20 <- tapData.sf[tapData.sf$Cluster_Location_Time == "LaCrosse", ]
intMap_LaCro_20 <- mapview(tapData.sf_LaCro_20, 
                           col.regions = "red", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

#Minneapolis
tapData.sf_MPLS_21 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Minneapolis", ]
intMap_MPLS_21 <- mapview(tapData.sf_MPLS_21, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

#Bellingham
tapData.sf_Bell_22 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Bellingham", ]
intMap_Bell_22 <- mapview(tapData.sf_Bell_22, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

#Portland
tapData.sf_Port_23 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Portland", ]
intMap_Port_23 <- mapview(tapData.sf_Port_23, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)
#Hawaii
tapData.sf_Haw_24 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Hawaii", ]
intMap_Haw_24 <- mapview(tapData.sf_Haw_24, 
                         col.regions = "red", # fill color
                         color = "gray",          # outline color
                         alpha.regions = 0.5,     # fill transparency
                         alpha = 0.5)

