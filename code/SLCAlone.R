tapData.sf_SLC <- tapData.sf[tapData.sf$Cluster_Location == "Salt Lake City", ]

tapData_SLC <- st_set_geometry(tapData.sf_SLC, NULL)

km_SLC <- tapData_SLC %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC$km_cluster <- km_SLC$cluster
tapData_SLC$km_cluster <- factor(tapData_SLC$km_cluster)


tapData.sf_SLC$km_cluster <- tapData_SLC$km_cluster

mapview(tapData.sf_SLC, 
          zcol= 'd2H',
          color = "gray",          # outline color
          alpha.regions = 0.8,     # fill transparency
          alpha = 0.8, 
          col.regions = c("#003f5c", "#d2042d"))


ggplot(data = tapData_SLC) + 
  geom_point(aes(x = d18O, y = d2H, color = Project_ID)) + 
  theme_classic()
# So the really really high numbers are p65, who also provided some really low numbers. Don't know what to say to that

ggplot(data = tapData_SLC) + 
  geom_point(aes(x = d18O, y = d2H, color = Month)) + 
  theme_classic()
