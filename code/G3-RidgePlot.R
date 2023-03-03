###RIDGE PLOT
library(ggridges);library(ggplot2);library(viridis);
library(hrbrthemes);library(patchwork)

#Oxygen Latitude North-South
tapData_1_ridge <- tapData.sf_1 %>%
  mutate(Cluster_Location = fct_relevel(Cluster_Location, 
                                        levels = "Hawaii","Oahu","St Petersburg",
                                        "Gainesville","San Marcos","San Diego",
                                        "Dallas Fort Worth","Phoenix","Atlanta","Athens",
                                        "Los Angeles","Albuquerque",
                                        "Flagstaff","Nashville","San Francisco","Cedar City",
                                        "Colorado Springs","Lawrence",
                                        "Denver","Salt Lake City","State College","Morristown",
                                        "Wooster","Ann Arbor",
                                        "LaCrosse","Minneapolis","Portland","Bellingham"))

DensPlot_d18O<-ggplot(tapData_1_ridge, aes(x = `d18O`, y = `Cluster_Location`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "d18O", option = "C") +
  #labs(title = 'Cities') +
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


#D-ex calculado y adherido a la tabla
tapData.sf_1$d_ex <- (tapData.sf_1$d2H - 8 * tapData.sf_1$d18O)

#D-ex Latitude North-South
tapData.sf_1_ridge <- tapData.sf_1 %>%
  mutate(Cluster_Location = fct_relevel(Cluster_Location, 
                                        levels = "Hawaii","Oahu","St Petersburg",
                                        "Gainesville","San Marcos","San Diego",
                                        "Dallas Fort Worth","Phoenix","Atlanta","Athens",
                                        "Los Angeles","Albuquerque",
                                        "Flagstaff","Nashville","San Francisco","Cedar City",
                                        "Colorado Springs","Lawrence",
                                        "Denver","Salt Lake City","State College","Morristown",
                                        "Wooster","Ann Arbor",
                                        "LaCrosse","Minneapolis","Portland","Bellingham"))

DensPlot_d_ex <- ggplot(tapData.sf_1_ridge, aes(x = `d_ex`, y = `Cluster_Location`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "d_ex", option = "C") +
  #labs(title = 'Cities') +
  theme_bw(base_size = 16) +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") #+
#geom_vline(xintercept = 10, linetype="dashed", 
#color = "black")

DensPlot_d18O+DensPlot_d_ex  
  
