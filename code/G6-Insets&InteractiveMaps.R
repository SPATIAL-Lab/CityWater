####Maps for the insets and their interactive versions#### 
#ATLANTA_GA
tapData.sf_Atl_7$km_cluster <- tapData_Atl_7$km_cluster

tmap_mode("view") 

Map_Atl_7 <- tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) + #OpenStreetMap.Mapnik , Stamen.TonerLite , Stamen.Terrain ,CartoDB.Positron
  tm_shape(tapData.sf_Atl_7) +
  tm_symbols(col = "km_cluster", size = 1.25, alpha = .5, #aca vas cambiando la variable que queres plotear...en "col="
             palette = c("pink", "lightblue"), border.col = "black")

# This technically isn't 'interactive', but I'm not sure we need it to be? 
mapview(tapData.sf_Atl_7, 
        zcol= 'km_cluster',
        color = "gray",          # outline color
        alpha.regions = 0.8,     # fill transparency
        alpha = 0.5, 
        col.regions = c("#003f5c", "#ffa600"))

#Interactive map in html
tmap_save(Map_Atl_7, "figures/Map_Atl_7v2.html")

#MULTIMODAL + RANGO LARGO = Lawrence_KS
tapData.sf_Law_14$km_cluster <- tapData_Law_14$km_cluster
tmap_mode("view") 

Map_Law_14 <- tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) + 
  tm_shape(tapData.sf_Law_14) +
  tm_symbols(col = "km_cluster", size = 1.25, alpha = .85, 
             palette = c("pink", "lightblue"), style = "quantile")


#UNIMODAL + RANGO CHICO =  Minneapolis_MN
tapData.sf_MPLS_21$km_cluster <- tapData_MPLS_21$km_cluster
tmap_mode("view")

Map_MPLS_21 <- tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) + 
  tm_shape(tapData.sf_MPLS_21) +
  tm_symbols(col = "km_cluster", size = 1.25, alpha = .85,
             palette = c("pink"), style = "quantile")


#UNIMODAL + RANGO LARGO = Denver_CO
tapData.sf_Denv_15$km_cluster <- tapData_Denv_15$km_cluster
tmap_mode("view")

Map_Denv_15 <- tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) + 
  tm_shape(tapData.sf_Denv_15) +
  tm_symbols(col = "km_cluster", size = 1.25, alpha = .85,
             palette = c("pink"), style = "quantile")

