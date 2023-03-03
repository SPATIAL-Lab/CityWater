###RIDGE PLOT

#Oxygen Latitude North-South
tapData_1_ridge <- tapData.sf %>%
  mutate(Cluster_Location = fct_relevel(Cluster_Location, 
                                        levels = "Hawaii","Oahu","St Petersburg",
                                        "Gainesville","San Marcos","San Diego",
                                        "Dallas Fort Worth","Phoenix","Atlanta","Athens",
                                        "Los Angeles","Albuquerque",
                                        "Flagstaff","Nashville","San Francisco","Cedar City",
                                        "Colorado Springs","Lawrence",
                                        "Denver","Salt Lake City","State College","Morristown",
                                        "Wooster","Ann Arbor",
                                        "La Crosse","Minneapolis","Portland","Bellingham"))

DensPlot_d18O <- ggplot(tapData_1_ridge, 
                        aes(x = `d18O`, y = `Cluster_Location`, fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradient(low = "#003f5c", high = "#84edff", na.value = NA) +
  theme_bw(base_size = 16) +
  theme(
    legend.position="none",
    axis.title.y=element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  labs(
    x = expression(paste(delta^18, "O", " (\u2030, VSMOW)"))
  ) 

DensPlot_d_ex <- ggplot(tapData_1_ridge, aes(x = d_ex, y = Cluster_Location, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradient(low = "#003f5c", high = "#84edff", na.value = NA) +
  #labs(title = 'Cities') +
  theme_bw(base_size = 16) +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + 
  labs(
    x = "Deuterium Excess"
  )

DensPlot_d18O+DensPlot_d_ex  
  
