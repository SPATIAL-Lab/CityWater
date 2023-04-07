#Run G1 while trying to find a better density plotting solution
library(ggplot2); library(dplyr); library(broom); library(ggridges)


# Ribbons -------------------------------------------------------
# Nope this doesn't create multimodal plots

df <- tapData %>% 
  select(c(Cluster_Location, d2H, d18O, d_ex)) %>% 
  mutate(GroupNum = as.numeric(factor(Cluster_Location))) %>% #rev() means the ordering will be from top to bottom
  group_by(GroupNum, Cluster_Location) %>% 
  do(tidy(density(.$d2H, bw = 
                  #abs(diff(range(.$d2H))/20)
                  10
                  ))) %>% 
  mutate(ymin = GroupNum * (max(y) / 1.5), #This constant controls how much overlap between groups there is
         ymax = y + ymin,
         ylabel = ymin + min(ymin)/2,
         xlabel = min(x) - mean(range(x))/2) #This constant controls how far to the left the labels are

labels <- tapData %>%
  select(c(Cluster_Location, d2H, d18O, d_ex)) %>% 
  mutate(Group_Num = as.numeric(factor(Cluster_Location))) %>%
  group_by(Cluster_Location, Group_Num) %>% 
  mutate(q1 = quantile(d2H)[2],
         median = quantile(d2H)[3],
         q3 = quantile(d2H)[4]) %>%
  filter(row_number() == 1) %>% 
  select(-d2H) %>% 
  left_join(df) %>% 
  mutate(xmed = x[which.min(abs(x - median))],
         yminmed = ymin[which.min(abs(x - median))],
         ymaxmed = ymax[which.min(abs(x - median))]) %>% 
  filter(row_number() == 1)

p <- ggplot(df, aes(x, ymin = ymin, ymax = ymax)) + geom_text(data = labels, aes(xlabel, ylabel, label = Cluster_Location)) +
  
  
  geom_vline(xintercept = 0, linewidth = 1.5, alpha = 0.5, colour = "#626262") + 
  geom_vline(xintercept = c(-2.5, -1.25, 1.25, 2.5), linewidth = 0.75, alpha = 0.25, colour = "#626262") + 
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#F0F0F0"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
for (i in unique(df$GroupNum)) {
  p <- p + geom_ribbon(data = df[df$GroupNum == i,], aes(group = GroupNum), colour = "#F0F0F0", fill = "black") +
    geom_segment(data = labels[labels$GroupNum == i,], aes(x = xmed, xend = xmed, y = yminmed, yend = ymaxmed), colour = "#F0F0F0", linetype = "dashed") +
    geom_segment(data = labels[labels$GroupNum == i,], x = min(df$x), xend = max(df$x), aes(y = ymin, yend = ymin), linewidth = 1.5, lineend = "round") 
}

# Density Ridges ----------------------------------------------------------
tapData <- read_csv("data/cityWater.csv", 
                    col_types = cols(Cluster_ID = col_character()))

density <- tapData %>%
  mutate(Cluster_Location = fct_relevel(Cluster_Location, 
                                        levels = "Hawaii","Oahu","St Petersburg",
                                        "Gainesville","San Marcos","San Diego",
                                        "Dallas Fort Worth","Phoenix","Atlanta","Athens",
                                        "Los Angeles","Albuquerque",
                                        "Flagstaff","Nashville","San Francisco","Cedar City",
                                        "Colorado Springs","Lawrence",
                                        "Denver","Salt Lake City","State College","Morristown",
                                        "Wooster","Ann Arbor",
                                        "La Crosse","Minneapolis","Portland","Bellingham")) %>% 
  select(c(Cluster_Location, d2H, d18O, d_ex))



# Basic Density Factor Wrap -----------------------------------------------

ggplot(density, aes(d2H, group = Cluster_Location, fill = Cluster_Location)) + 
  geom_density(bw = "nrd0") + 
  facet_wrap(~Cluster_Location, scales = "free") +
  scale_fill_viridis(discrete = T) + 
  theme_classic() + 
  theme(legend.position = "none")

# ggridges package ----------------------------------------------------------

# current ggridges plot
ggplot() + 
  geom_density_ridges_gradient(data = density, 
                               aes(x = `d18O`, y = `Cluster_Location`, fill = ..x..),
                               scale = 3, rel_min_height = 0.01) +
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

# If we try to call each one, they're put in alphabetical order even though I don't place them as such
tapData_Oahu_2 <- tapData[tapData$Cluster_Location_Time == "Oahu", ]
tapData_SanPete_3 <- tapData[tapData$Cluster_Location_Time == "St Petersburg", ]
tapData_Gaines_4 <- tapData[tapData$Cluster_Location_Time == "Gainesville", ]
tapData_SM_5 <- tapData[tapData$Cluster_Location_Time == "San Marcos", ]
tapData_Atl_7 <- tapData[tapData$Cluster_Location_Time == "Atlanta", ]
tapData_Ath_8 <- tapData[tapData$Cluster_Location_Time == "Athens", ]

ggplot() + 
  geom_density_ridges_gradient(data = tapData_Gaines_4, 
                               aes(x = `d18O`, y = `Cluster_Location`, fill = ..x..),
                               scale = 3, rel_min_height = 0.01) +
  geom_density_ridges_gradient(data = tapData_Ath_8, 
                               aes(x = `d18O`, y = `Cluster_Location`, fill = ..x..),
                               scale = 3, rel_min_height = 0.01) +
  geom_density_ridges_gradient(data = tapData_Atl_7, 
                               aes(x = `d18O`, y = `Cluster_Location`, fill = ..x..),
                               scale = 3, rel_min_height = 0.01) +
  geom_density_ridges_gradient(data = tapData_Oahu_2, 
                               aes(x = `d18O`, y = `Cluster_Location`, fill = ..x..),
                               scale = 3, rel_min_height = 0.01) +
  geom_density_ridges_gradient(data = tapData_SanPete_3, 
                               aes(x = `d18O`, y = `Cluster_Location`, fill = ..x..),
                               scale = 3, rel_min_height = 0.01) +
  geom_density_ridges_gradient(data = tapData_SM_5, 
                               aes(x = `d18O`, y = `Cluster_Location`, fill = ..x..),
                               scale = 3, rel_min_height = 0.01) +
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


# HELP: If we do a for loop we are able to do every Cluster_Location and each one gets its own 
# bandwidth. But we can't control the y-axis order it seems? 

p <- ggplot() +
  scale_fill_gradient(low = "#003f5c", high = "#84edff", na.value = NA) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  labs(
    x = expression(paste(delta^18, "O", " (\u2030, VSMOW)"))
  )

# reorder does nothing here
for (i in unique(density$Cluster_Location)) {
  p <- p +
    geom_density_ridges_gradient(data = density[density$Cluster_Location == i,], 
                                 aes(x = d18O, y = Cluster_Location, fill = ..x..),
                                                   scale = 0.9)
    }


# This only takes the first bandwidth in the list, so it doesn't really work. 
bw <- c(0.114, 0.828, 0.354, 0.199, 0.462, 0.368, 0.767, 0.141, 0.035, 0.251, 0.0275, 
        0.0357, 0.387, 0.0392, 0.0494, 0.136, 0.0468, 0.065, 0.174, 0.379, 0.0522, 
        0.505, 0.144, 0.042, 0.0824, 0.152, 0.0955, 0.304)
ggplot() + 
  geom_density_ridges_gradient(data = density, aes(x = d18O, 
                                                   y = Cluster_Location, 
                                                   fill = ..x..), 
                               bandwidth = rev(bw))

