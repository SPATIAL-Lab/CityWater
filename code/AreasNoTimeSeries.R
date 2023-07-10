tapData.sf_SLC <- tapData.sf[tapData.sf$Cluster_Location == "Salt Lake City", ]
# cutting some outliers (NOTE: doesn't make a difference, only one cluster either way)
# tapData.sf_SLC <- subset(tapData.sf_SLC, d18O < -10)
tapData_SLC <- st_set_geometry(tapData.sf_SLC, NULL)

km_SLC <- tapData_SLC %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC$km_cluster <- km_SLC$cluster
tapData_SLC$km_cluster <- factor(tapData_SLC$km_cluster)
tapData.sf_SLC$km_cluster <- tapData_SLC$km_cluster

mapview(tapData.sf_SLC, 
        zcol= 'd2H',
        at = seq(-132, -90, 10), 
        legend = T)

# Let's also run for clusters San Fran, Phoenix, San Diego, and Los Angeles

# Los Angeles

tapData.sf_LA <- tapData.sf[tapData.sf$Cluster_Location == "Los Angeles", ]

tapData_LA <- st_set_geometry(tapData.sf_LA, NULL)

km_LA <- tapData_LA %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_LA$km_cluster <- km_LA$cluster
tapData_LA$km_cluster <- factor(tapData_LA$km_cluster)
tapData.sf_LA$km_cluster <- tapData_LA$km_cluster

# Phoenix

tapData.sf_PHX <- tapData.sf[tapData.sf$Cluster_Location == "Phoenix", ]

tapData_PHX <- st_set_geometry(tapData.sf_PHX, NULL)

km_PHX <- tapData_PHX %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_PHX$km_cluster <- km_PHX$cluster
tapData_PHX$km_cluster <- factor(tapData_PHX$km_cluster)
tapData.sf_PHX$km_cluster <- tapData_PHX$km_cluster


# San Diego
tapData.sf_SAN <- tapData.sf[tapData.sf$Cluster_Location == "San Diego", ]

tapData_SAN <- st_set_geometry(tapData.sf_SAN, NULL)

km_SAN <- tapData_SAN %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SAN$km_cluster <- km_SAN$cluster
tapData_SAN$km_cluster <- factor(tapData_SAN$km_cluster)
tapData.sf_SAN$km_cluster <- tapData_SAN$km_cluster

# San Francisco
tapData.sf_SFO <- tapData.sf[tapData.sf$Cluster_Location == "San Francisco", ]

tapData_SFO <- st_set_geometry(tapData.sf_SFO, NULL)

km_SFO <- tapData_SFO %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SFO$km_cluster <- km_SFO$cluster
tapData_SFO$km_cluster <- factor(tapData_SFO$km_cluster)
tapData.sf_SFO$km_cluster <- tapData_SFO$km_cluster

