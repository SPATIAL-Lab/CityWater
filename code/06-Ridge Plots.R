# Ridge Plots for Figure 3 ------------------------------------------------

library(dplyr); library(ggplot2); library(ggridges); library(patchwork)

tapData <- read_csv("data/cityWater.csv", 
                    col_types = cols(cluster_ID = col_character()))

#Oxygen Latitude North-South
density <- tapData %>%
  mutate(cluster_location = fct_relevel(cluster_location, 
                                        "Hawaii","Oahu","St Petersburg",
                                        "Gainesville","San Marcos","Dallas Fort Worth",
                                        "San Diego", "Phoenix","Atlanta","Athens",
                                        "Los Angeles","Albuquerque", "Flagstaff",
                                        "Nashville","Cedar City", "San Francisco",
                                        "Colorado Springs","Lawrence","Denver",
                                        "Salt Lake City","Morristown","State College",
                                        "Wooster", "Youngstown", "Cleveland-Akron",
                                        "Ann Arbor","La Crosse","Minneapolis",
                                        "Portland","Bellingham")) 


DensPlot_d18O <- ggplot(density, 
                        aes(x = `d18O`, y = `cluster_location`, fill = ..x..)) + 
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

DensPlot_d_ex <- ggplot(density, aes(x = d_ex, y = cluster_location, fill = ..x..)) +
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

DensPlot_d18O + DensPlot_d_ex  
ggsave("figures/densityRidges.pdf", width=12, height=5, units="in", dpi=300)



