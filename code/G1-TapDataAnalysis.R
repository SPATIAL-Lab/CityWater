# This script sets up everything for the following scripts, run completely. 
library(raster); library(sf); library(viridis);library(rgdal);library(mapview);
library(leaflet); library(spData);library(ggpmisc);library(usmap);
library(tidyverse); library(magrittr);library(rio);library(rstatix);
library(diptest); library(multimode);library(factoextra);library(patchwork); 
library(ggridges); library(hrbrthemes); library(patchwork); library(tmap); 
library(ggrepel)

###Data import & prep###
tapData <- read_csv("data/cityWater.csv", 
                    col_types = cols(Cluster_ID = col_character()))

#names(tapData)
tapData$Cluster_ID <- factor(tapData$Cluster_ID)
tapData$Cluster_Location <- factor(tapData$Cluster_Location)
tapData$Cluster_Location_Time <- factor(tapData$Cluster_Location_Time)
tapData$Cluster_State <- factor(tapData$Cluster_State)
tapData$Project_ID <- factor(tapData$Project_ID)

#going spatial
tapData.sf <- st_as_sf(tapData, 
                        coords = c("Long", "Lat"),
                        crs = 4326) #EPSG code for WGS84

# Subsetting Urban Areas and Slices ---------------------------------------

#SLC
tapData.sf_SLC_1.01 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ap-13", ]
tapData.sf_SLC_1.02 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ag-Oct-13", ]
tapData.sf_SLC_1.03 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Feb-14", ]
tapData.sf_SLC_1.04 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ap-May-14", ]
tapData.sf_SLC_1.05 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ag-Sep-14", ]
tapData.sf_SLC_1.06 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ap-May-15", ]
tapData.sf_SLC_1.07 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Sep-Oct-15", ]
tapData.sf_SLC_1.08 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ap-16", ]
tapData.sf_SLC_1.09 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Sep-16", ]
tapData.sf_SLC_1.10 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Mar-May-17", ]
tapData.sf_SLC_1.11 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Oct-17", ]

#SAN FRANCISCO
tapData.sf_SF_25.1 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Dic-13", ]
tapData.sf_SF_25.2 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Mar-Ap-14", ]
tapData.sf_SF_25.3 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Jun-14", ]
tapData.sf_SF_25.4 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Nov-14", ]
tapData.sf_SF_25.5 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Dic-14", ]
tapData.sf_SF_25.6 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Mar-15", ]
tapData.sf_SF_25.7 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Jul-15", ]

#PHOENIX
tapData.sf_PHO_26.1 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Phoenix_Mar-Ap-14", ]
tapData.sf_PHO_26.2 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Phoenix_Oct-14", ]

#SAN DIEGO
tapData.sf_SD_27.1 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Diego_Dic-13", ]
tapData.sf_SD_27.2 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Diego_Ap-14", ]

#LOS ANGELES
tapData.sf_LA_28.1 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Los Angeles_Dic-13", ]
tapData.sf_LA_28.2 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Los Angeles_Mar-Ap-14", ]
tapData.sf_LA_28.3 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Los Angeles_Nov-14", ]

#OAHU
tapData.sf_Oahu_2 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Oahu", ]

#St Petersburg
tapData.sf_SanPete_3 <- tapData.sf[tapData.sf$Cluster_Location_Time == "St Petersburg", ]

#Gainesville
tapData.sf_Gaines_4 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Gainesville", ]

#San Marcos
tapData.sf_SM_5 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Marcos", ]

#Dallas Fort Worth
tapData.sf_DF_6 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Dallas Fort Worth", ]

#Atlanta
tapData.sf_Atl_7 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Atlanta", ]

#Athens
tapData.sf_Ath_8 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Athens", ]

#Albuquerque
tapData.sf_ABQ_9 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Albuquerque", ]

#Flagstaff
tapData.sf_Flag_10 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Flagstaff", ]

#Nashville
tapData.sf_Nashv_11 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Nashville", ]

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