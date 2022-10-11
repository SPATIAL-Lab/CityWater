####Maps for the insets and their interactive versions#### 
#ATLANTA_GA
tapData.sf_1_Atl_7$km_cluster <- tapData_1_Atl_7$km_cluster

tmap_mode("view") 

Map_Atl_7 <- tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) + #OpenStreetMap.Mapnik , Stamen.TonerLite , Stamen.Terrain ,CartoDB.Positron
  tm_shape(tapData.sf_1_Atl_7) +
  tm_symbols(col = "km_cluster", size = 1.25, alpha = .5, #aca vas cambiando la variable que queres plotear...en "col="
             palette = c("pink", "lightblue"), border.col = "black")

#Interactive map in html
tmap_save(Map_Atl_7, "Map_Atl_7v2.html")

#MULTIMODAL + RANGO LARGO = Lawrence_KS
tapData.sf_1_Law_14$km_cluster <- tapData_1_Law_14$km_cluster
tmap_mode("view") 

Map_Law_14 <- tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) + 
  tm_shape(tapData.sf_1_Law_14) +
  tm_symbols(col = "km_cluster", size = 1.25, alpha = .85, 
             palette = c("pink", "lightblue"), style = "quantile")


#UNIMODAL + RANGO CHICO =  Minneapolis_MN
tapData.sf_1_MPLS_21$km_cluster <- tapData_1_MPLS_21$km_cluster
tmap_mode("view")

Map_MPLS_21 <- tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) + 
  tm_shape(tapData.sf_1_MPLS_21) +
  tm_symbols(col = "km_cluster", size = 1.25, alpha = .85,
             palette = c("pink"), style = "quantile")


#UNIMODAL + RANGO LARGO = Denver_CO
tapData.sf_1_Denv_15$km_cluster <- tapData_1_Denv_15$km_cluster
tmap_mode("view")

Map_Denv_15 <- tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) + 
  tm_shape(tapData.sf_1_Denv_15) +
  tm_symbols(col = "km_cluster", size = 1.25, alpha = .85,
             palette = c("pink"), style = "quantile")

##############INTERACTIVE MAPs for all the cities##############################
#SLC
tapData.sf_1_SLC_1.1 <- tapData.sf_1_SLC[tapData.sf_1_SLC$Cluster_Location_Time == "SLC_Area_Ap-13", ]
intMap_SLC_1.1 <- mapview(tapData.sf_1_SLC_1.1, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)  

tapData.sf_1_SLC_1.2 <- tapData.sf_1_SLC[tapData.sf_1_SLC$Cluster_Location_Time == "SLC_Area_Ag-Oct-13", ]
intMap_SLC_1.2 <- mapview(tapData.sf_1_SLC_1.2, 
                          col.regions = "blue", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)  

tapData.sf_1_SLC_1.3 <- tapData.sf_1_SLC[tapData.sf_1_SLC$Cluster_Location_Time == "SLC_Area_Feb-14", ]
intMap_SLC_1.3 <- mapview(tapData.sf_1_SLC_1.3, 
                          col.regions = "yellow", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)  

tapData.sf_1_SLC_1.4 <- tapData.sf_1_SLC[tapData.sf_1_SLC$Cluster_Location_Time == "SLC_Area_Ap-May-14", ]
intMap_SLC_1.4 <- mapview(tapData.sf_1_SLC_1.4, 
                          col.regions = "black", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)  

tapData.sf_1_SLC_1.5 <- tapData.sf_1_SLC[tapData.sf_1_SLC$Cluster_Location_Time == "SLC_Area_Ag-Sep-14", ]
intMap_SLC_1.5 <- mapview(tapData.sf_1_SLC_1.5, 
                          col.regions = "aquamarine4", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_SLC_1.6 <- tapData.sf_1_SLC[tapData.sf_1_SLC$Cluster_Location_Time == "SLC_Area_Ap-May-15", ]
intMap_SLC_1.6 <- mapview(tapData.sf_1_SLC_1.6, 
                          col.regions = "antiquewhite3", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_SLC_1.7 <- tapData.sf_1_SLC[tapData.sf_1_SLC$Cluster_Location_Time == "SLC_Area_Sep-Oct-15", ]
intMap_SLC_1.7 <- mapview(tapData.sf_1_SLC_1.7, 
                          col.regions = "chartreuse3", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_SLC_1.8 <- tapData.sf_1_SLC[tapData.sf_1_SLC$Cluster_Location_Time == "SLC_Area_Ap-16", ]
intMap_SLC_1.8 <- mapview(tapData.sf_1_SLC_1.8, 
                          col.regions = "chocolate1", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_SLC_1.9 <- tapData.sf_1_SLC[tapData.sf_1_SLC$Cluster_Location_Time == "SLC_Area_Sep-16", ]
intMap_SLC_1.9 <- mapview(tapData.sf_1_SLC_1.9, 
                          col.regions = "chocolate4", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_SLC_1.1.0 <- tapData.sf_1_SLC[tapData.sf_1_SLC$Cluster_Location_Time == "SLC_Area_Mar-May-17", ]
intMap_SLC_1.1.0 <- mapview(tapData.sf_1_SLC_1.1.0, 
                            col.regions = "coral2", # fill color
                            color = "gray",          # outline color
                            alpha.regions = 0.5,     # fill transparency
                            alpha = 0.5)

tapData.sf_1_SLC_1.11 <- tapData.sf_1_SLC[tapData.sf_1_SLC$Cluster_Location_Time == "SLC_Area_Oct-17", ]
intMap_SLC_1.11 <- mapview(tapData.sf_1_SLC_1.11, 
                           col.regions = "blueviolet", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

intMap_SLC_1.1 + intMap_SLC_1.2 + intMap_SLC_1.3 + intMap_SLC_1.4 + 
  intMap_SLC_1.5 + intMap_SLC_1.6 + intMap_SLC_1.7 + intMap_SLC_1.8 + 
  intMap_SLC_1.9 + intMap_SLC_1.1.0 + intMap_SLC_1.11

#SAN FRANCISCO
tapData.sf_1_SF_25.1 <- tapData.sf_1_San_Francisco[tapData.sf_1_San_Francisco$Cluster_Location_Time == "San_Francisco_Dic-13", ]
intMap_SF_25.1 <- mapview(tapData.sf_1_SF_25.1, 
                          col.regions = "blueviolet", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_SF_25.2 <- tapData.sf_1_San_Francisco[tapData.sf_1_San_Francisco$Cluster_Location_Time == "San_Francisco_Mar-Ap-14", ]
intMap_SF_25.2 <- mapview(tapData.sf_1_SF_25.2, 
                          col.regions = "black", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_SF_25.3 <- tapData.sf_1_San_Francisco[tapData.sf_1_San_Francisco$Cluster_Location_Time == "San_Francisco_Jun-14", ]
intMap_SF_25.3 <- mapview(tapData.sf_1_SF_25.3, 
                          col.regions = "darkred", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_SF_25.4 <- tapData.sf_1_San_Francisco[tapData.sf_1_San_Francisco$Cluster_Location_Time == "San_Francisco_Nov-14", ]
intMap_SF_25.4 <- mapview(tapData.sf_1_SF_25.4, 
                          col.regions = "darkblue", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_SF_25.5 <- tapData.sf_1_San_Francisco[tapData.sf_1_San_Francisco$Cluster_Location_Time == "San_Francisco_Dic-14", ]
intMap_SF_25.5 <- mapview(tapData.sf_1_SF_25.5, 
                          col.regions = "pink", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_SF_25.6 <- tapData.sf_1_San_Francisco[tapData.sf_1_San_Francisco$Cluster_Location_Time == "San_Francisco_Mar-15", ]
intMap_SF_25.6 <- mapview(tapData.sf_1_SF_25.6, 
                          col.regions = "darkgreen", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_SF_25.7 <- tapData.sf_1_San_Francisco[tapData.sf_1_San_Francisco$Cluster_Location_Time == "San_Francisco_Jul-15", ]
intMap_SF_25.7 <- mapview(tapData.sf_1_SF_25.7, 
                          col.regions = "gold", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

intMap_SF_25.1 + intMap_SF_25.2 + intMap_SF_25.3 + intMap_SF_25.4 +
  intMap_SF_25.5 + intMap_SF_25.6 + intMap_SF_25.7


#PHOENIX
tapData.sf_1_PHO_26.1 <- tapData.sf_1_Phoenix[tapData.sf_1_Phoenix$Cluster_Location_Time == "Phoenix_Mar-Ap-14", ]
intMap_PHO_26.1 <- mapview(tapData.sf_1_PHO_26.1, 
                           col.regions = "red", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

tapData.sf_1_PHO_26.2 <- tapData.sf_1_Phoenix[tapData.sf_1_Phoenix$Cluster_Location_Time == "Phoenix_Oct-14", ]
intMap_PHO_26.2 <- mapview(tapData.sf_1_PHO_26.2, 
                           col.regions = "blue", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

intMap_PHO_26.1 + intMap_PHO_26.2

#SAN DIEGO
tapData.sf_1_SD_27.1 <- tapData.sf_1_San_Diego[tapData.sf_1_San_Diego$Cluster_Location_Time == "San_Diego_Dic-13", ]
intMap_SD_27.1 <- mapview(tapData.sf_1_SD_27.1, 
                          col.regions = "blue", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_SD_27.2 <- tapData.sf_1_San_Diego[tapData.sf_1_San_Diego$Cluster_Location_Time == "San_Diego_Ap-14", ]
intMap_SD_27.2 <- mapview(tapData.sf_1_SD_27.2, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

intMap_SD_27.1 + intMap_SD_27.2

#LOS ANGELES
tapData.sf_1_LA_28.1 <- tapData.sf_1_Los_Angeles[tapData.sf_1_Los_Angeles$Cluster_Location_Time == "Los_Angeles_Dic-13", ]
intMap_LA_28.1 <- mapview(tapData.sf_1_LA_28.1, 
                          col.regions = "blue", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_LA_28.2 <- tapData.sf_1_Los_Angeles[tapData.sf_1_Los_Angeles$Cluster_Location_Time == "Los_Angeles_Mar-Ap-14", ]
intMap_LA_28.2 <- mapview(tapData.sf_1_LA_28.2, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

tapData.sf_1_LA_28.3 <- tapData.sf_1_Los_Angeles[tapData.sf_1_Los_Angeles$Cluster_Location_Time == "Los_Angeles_Nov-14", ]
intMap_LA_28.3 <- mapview(tapData.sf_1_LA_28.3, 
                          col.regions = "gold", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

intMap_LA_28.1 + intMap_LA_28.2 + intMap_LA_28.3


#Oahu
tapData.sf_1_Oahu_2 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Oahu", ]
intMap_Oahu_2 <- mapview(tapData.sf_1_Oahu_2, 
                         col.regions = "red", # fill color
                         color = "gray",          # outline color
                         alpha.regions = 0.5,     # fill transparency
                         alpha = 0.5)

#San Petersburgo
tapData.sf_1_SanPete_3 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "San Petersburgo", ]
intMap_SanPete_3 <- mapview(tapData.sf_1_SanPete_3, 
                            col.regions = "red", # fill color
                            color = "gray",          # outline color
                            alpha.regions = 0.5,     # fill transparency
                            alpha = 0.5)

#Gainesville
tapData.sf_1_Gaines_4 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Gainesville", ]
intMap_Gaines_4 <- mapview(tapData.sf_1_Gaines_4, 
                           col.regions = "red", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

#San Marcos
tapData.sf_1_SM_5 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "San Marcos", ]
intMap_SM_5 <- mapview(tapData.sf_1_SM_5, 
                       col.regions = "red", # fill color
                       color = "gray",          # outline color
                       alpha.regions = 0.5,     # fill transparency
                       alpha = 0.5)

#DallasForthWard
tapData.sf_1_DF_6 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "DallasForthWard", ]
intMap_DF_6 <- mapview(tapData.sf_1_DF_6, 
                       col.regions = "red", # fill color
                       color = "gray",          # outline color
                       alpha.regions = 0.5,     # fill transparency
                       alpha = 0.5)

#Atlanta
tapData.sf_1_Atl_7 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Atlanta", ]
intMap_Atl_7 <- mapview(tapData.sf_1_Atl_7, 
                        col.regions = "red", # fill color
                        color = "gray",          # outline color
                        alpha.regions = 0.5,     # fill transparency
                        alpha = 0.5)

#Athens
tapData.sf_1_Ath_8 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Athens", ]
intMap_Ath_8 <- mapview(tapData.sf_1_Ath_8, 
                        col.regions = "red", # fill color
                        color = "gray",          # outline color
                        alpha.regions = 0.5,     # fill transparency
                        alpha = 0.5)

#Albuquerque
tapData.sf_1_ABQ_9 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Albuquerque", ]
intMap_ABQ_9 <- mapview(tapData.sf_1_ABQ_9, 
                        col.regions = "red", # fill color
                        color = "gray",          # outline color
                        alpha.regions = 0.5,     # fill transparency
                        alpha = 0.5)

#Flagstaff
tapData.sf_1_Flag_10 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Flagstaff", ]
intMap_Flag_10 <- mapview(tapData.sf_1_Flag_10, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

#Nashville
tapData.sf_1_Nashv_11 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Nashville", ]
intMap_Nashv_11 <- mapview(tapData.sf_1_Nashv_11, 
                           col.regions = "red", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

#Cedar City
tapData.sf_1_Cedar_12 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Cedar_City", ]
intMap_Cedar_12 <- mapview(tapData.sf_1_Cedar_12, 
                           col.regions = "red", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

#Colorado_Springs
tapData.sf_1_ColoSp_13 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Colorado_Springs", ]
intMap_ColoSp_13 <- mapview(tapData.sf_1_ColoSp_13, 
                            col.regions = "red", # fill color
                            color = "gray",          # outline color
                            alpha.regions = 0.5,     # fill transparency
                            alpha = 0.5)

#Lawrence
tapData.sf_1_Law_14 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Lawrence", ]
intMap_Law_14 <- mapview(tapData.sf_1_Law_14, 
                         col.regions = "red", # fill color
                         color = "gray",          # outline color
                         alpha.regions = 0.5,     # fill transparency
                         alpha = 0.5)

#Denver
tapData.sf_1_Denv_15 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Denver", ]
intMap_Denv_15 <- mapview(tapData.sf_1_Denv_15, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

#MBS
tapData.sf_1_MBS_16 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "MBS", ]
intMap_MBS_16 <- mapview(tapData.sf_1_MBS_16, 
                         col.regions = "red", # fill color
                         color = "gray",          # outline color
                         alpha.regions = 0.5,     # fill transparency
                         alpha = 0.5)

#SC
tapData.sf_1_SC_17 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "SC", ]
intMap_SC_17 <- mapview(tapData.sf_1_SC_17, 
                        col.regions = "red", # fill color
                        color = "gray",          # outline color
                        alpha.regions = 0.5,     # fill transparency
                        alpha = 0.5)

#Wooster
tapData.sf_1_Woo_18 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Wooster", ]
intMap_Woo_18 <- mapview(tapData.sf_1_Woo_18, 
                         col.regions = "red", # fill color
                         color = "gray",          # outline color
                         alpha.regions = 0.5,     # fill transparency
                         alpha = 0.5)

#Ann_Arbor
tapData.sf_1_Ann_19 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Ann_Arbor", ]
intMap_Ann_19 <- mapview(tapData.sf_1_Ann_19, 
                         col.regions = "red", # fill color
                         color = "gray",          # outline color
                         alpha.regions = 0.5,     # fill transparency
                         alpha = 0.5)

#LaCrosse
tapData.sf_1_LaCro_20 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "LaCrosse", ]
intMap_LaCro_20 <- mapview(tapData.sf_1_LaCro_20, 
                           col.regions = "red", # fill color
                           color = "gray",          # outline color
                           alpha.regions = 0.5,     # fill transparency
                           alpha = 0.5)

#Minneapolis
tapData.sf_1_MPLS_21 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Minneapolis", ]
intMap_MPLS_21 <- mapview(tapData.sf_1_MPLS_21, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

#Bellingham
tapData.sf_1_Bell_22 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Bellingham", ]
intMap_Bell_22 <- mapview(tapData.sf_1_Bell_22, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)

#Portland
tapData.sf_1_Port_23 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Portland", ]
intMap_Port_23 <- mapview(tapData.sf_1_Port_23, 
                          col.regions = "red", # fill color
                          color = "gray",          # outline color
                          alpha.regions = 0.5,     # fill transparency
                          alpha = 0.5)
#Hawaii
tapData.sf_1_Haw_24 <- tapData.sf_1[tapData.sf_1$Cluster_Location_Time == "Hawaii", ]
intMap_Haw_24 <- mapview(tapData.sf_1_Haw_24, 
                         col.regions = "red", # fill color
                         color = "gray",          # outline color
                         alpha.regions = 0.5,     # fill transparency
                         alpha = 0.5)
