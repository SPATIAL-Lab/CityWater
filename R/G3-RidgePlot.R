###RIDGE PLOT
library(ggridges);library(ggplot2);library(viridis);
library(hrbrthemes);library(patchwork)

#Oxigeno Latitud Norte-Sur
tapData_1_ridge <- tapData_1 %>%
  mutate(Cluster_Location = fct_relevel(Cluster_Location, 
                                        levels = "Hawaii","Oahu","San Petersburgo",
                                        "Gainesville","San Marcos","San_Diego",
                                        "DallasForthWard","Phoenix","Atlanta","Athens",
                                        "Los_Angeles","Albuquerque",
                                        "Flagstaff","Nashville","San_Francisco","Cedar_City",
                                        "Colorado_Springs","Lawrence",
                                        "Denver","SLC_Area","SC","MBS","Wooster","Ann_Arbor",
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
  )


#D-ex calculado y adherido a la tabla
d_ex = (tapData.sf_1$d2H - 8 * tapData.sf_1$d18O)
d_ex
tapData.sf_1$d_ex <- d_ex

#D-ex Latitud Norte-Sur
tapData.sf_1_ridge <- tapData.sf_1 %>%
  mutate(Cluster_Location = fct_relevel(Cluster_Location, 
                                        levels = "Hawaii","Oahu","San Petersburgo",
                                        "Gainesville","San Marcos","San_Diego",
                                        "DallasForthWard","Phoenix","Atlanta","Athens",
                                        "Los_Angeles","Albuquerque",
                                        "Flagstaff","Nashville","San_Francisco","Cedar_City",
                                        "Colorado_Springs","Lawrence",
                                        "Denver","SLC_Area","SC","MBS","Wooster","Ann_Arbor",
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