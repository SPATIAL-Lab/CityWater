# This script is for the density plot of Figure 1. Some post-processing was done in Adobe Illustrator
library(factoextra); library(ggridges); library(forcats); library(readr); library(dplyr)

tapData <- read_csv("data/cityWater.csv", 
                    col_types = cols(cluster_ID = col_character()))
# Modality ----------------------------------------------------------------
#SLC
temporal_1.01 <- tapData[tapData$cluster_location_time == "Salt Lake City_Ap-13", ]
temporal_1.02 <- tapData[tapData$cluster_location_time == "Salt Lake City_Ag-Oct-13", ]
temporal_1.03 <- tapData[tapData$cluster_location_time == "Salt Lake City_Feb-14", ]
temporal_1.04 <- tapData[tapData$cluster_location_time == "Salt Lake City_Ap-May-14", ]
temporal_1.05 <- tapData[tapData$cluster_location_time == "Salt Lake City_Ag-Sep-14", ]
temporal_1.06 <- tapData[tapData$cluster_location_time == "Salt Lake City_Ap-May-15", ]
temporal_1.07 <- tapData[tapData$cluster_location_time == "Salt Lake City_Sep-Oct-15", ]
temporal_1.08 <- tapData[tapData$cluster_location_time == "Salt Lake City_Ap-16", ]
temporal_1.09 <- tapData[tapData$cluster_location_time == "Salt Lake City_Sep-16", ]
temporal_1.10 <- tapData[tapData$cluster_location_time == "Salt Lake City_Mar-May-17", ]
temporal_1.11 <- tapData[tapData$cluster_location_time == "Salt Lake City_Oct-17", ]

#San Francisco
temporal_25.1 <- tapData[tapData$cluster_location_time == "San Francisco_Dic-13", ]
temporal_25.2 <- tapData[tapData$cluster_location_time == "San Francisco_Mar-Ap-14", ]
temporal_25.3 <- tapData[tapData$cluster_location_time == "San Francisco_Jun-14", ]
temporal_25.4 <- tapData[tapData$cluster_location_time == "San Francisco_Nov-14", ]
temporal_25.5 <- tapData[tapData$cluster_location_time == "San Francisco_Dic-14", ]
temporal_25.6 <- tapData[tapData$cluster_location_time == "San Francisco_Mar-15", ]
temporal_25.7 <- tapData[tapData$cluster_location_time == "San Francisco_Jul-15", ]

# Los Angeles
temporal_28.1 <- tapData[tapData$cluster_location_time == "Los Angeles_Dic-13", ]
temporal_28.2 <- tapData[tapData$cluster_location_time == "Los Angeles_Mar-Ap-14", ]
temporal_28.3 <- tapData[tapData$cluster_location_time == "Los Angeles_Nov-14", ]

# Phoenix
temporal_26.1 <- tapData[tapData$cluster_location_time == "Phoenix_Mar-Ap-14", ]
temporal_26.2 <- tapData[tapData$cluster_location_time == "Phoenix_Oct-14", ]

# k-means clustering ------------------------------------------------------

km_SF_25.1 <- temporal_25.1 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_25.1$km_cluster <- factor(km_SF_25.1$cluster)

#temporal_25.2
km_SF_25.2 <- temporal_25.2 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_25.2$km_cluster <- factor(km_SF_25.2$cluster)

#temporal_25.3
km_SF_25.3 <- temporal_25.3 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_25.3$km_cluster <- factor(km_SF_25.3$cluster)

#temporal_25.4
km_SF_25.4 <- temporal_25.4 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_25.4$km_cluster <- factor(km_SF_25.4$cluster)

#temporal_25.5
km_SF_25.5 <- temporal_25.5 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_25.5$km_cluster <- factor(km_SF_25.5$cluster)

#temporal_25.6
km_SF_25.6 <- temporal_25.6 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_25.6$km_cluster <- factor(km_SF_25.6$cluster)

#temporal_25.7
km_SF_25.7 <- temporal_25.7 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_25.7$km_cluster <- factor(km_SF_25.7$cluster)

#temporal_1.1
km_SLC_1.01 <- temporal_1.01 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_1.01$km_cluster <- factor(km_SLC_1.01$cluster)

#temporal_1.02
km_SLC_1.02 <- temporal_1.02 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_1.02$km_cluster <- factor(km_SLC_1.02$cluster)

#temporal_1.03
km_SLC_1.03 <- temporal_1.03 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_1.03$km_cluster <- factor(km_SLC_1.03$cluster)

#temporal_1.04
km_SLC_1.04 <- temporal_1.04 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_1.04$km_cluster <- factor(km_SLC_1.04$cluster)

#temporal_1.05
km_SLC_1.05 <- temporal_1.05 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_1.05$km_cluster <- factor(km_SLC_1.05$cluster)

#temporal_1.06
km_SLC_1.06 <- temporal_1.06 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_1.06$km_cluster <- factor(km_SLC_1.06$cluster)

#temporal_1.07
km_SLC_1.07 <- temporal_1.07 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_1.07$km_cluster <- factor(km_SLC_1.07$cluster)

#temporal_1.08
km_SLC_1.08 <- temporal_1.08 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_1.08$km_cluster <- factor(km_SLC_1.08$cluster)

#temporal_1.09
km_SLC_1.09 <- temporal_1.09 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_1.09$km_cluster <- factor(km_SLC_1.09$cluster)

#temporal_1.10
km_SLC_1.10 <- temporal_1.10 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_1.10$km_cluster <- factor(km_SLC_1.10$cluster)

#temporal_1.11
km_SLC_1.11 <- temporal_1.11 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_1.11$km_cluster <- factor(km_SLC_1.11$cluster)

#temporal_26.1
km_PHO_26.1 <- temporal_26.1 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_26.1$km_cluster <- factor(km_PHO_26.1$cluster)

#temporal_26.2
km_PHO_26.2 <- temporal_26.2 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_26.2$km_cluster <- factor(km_PHO_26.2$cluster)

#temporal_28.1
km_LAX_28.1 <- temporal_28.1 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_28.1$km_cluster <- factor(km_LAX_28.1$cluster)

#temporal_28.2
km_LAX_28.2 <- temporal_28.2 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_28.2$km_cluster <- factor(km_LAX_28.2$cluster)

#temporal_28.3
km_LAX_28.3 <- temporal_28.3 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
temporal_28.3$km_cluster <- factor(km_LAX_28.3$cluster)

#Let's count the number of clusters and report modality in a reproducible table. 
kmeans <- list(temporal_1.01, temporal_1.02, temporal_1.03,
               temporal_1.04, temporal_1.05, temporal_1.06, 
               temporal_1.07, temporal_1.08, temporal_1.09, 
               temporal_1.10, temporal_1.11, temporal_25.1, 
               temporal_25.2, temporal_25.3, temporal_25.4, 
               temporal_25.5, temporal_25.6, temporal_25.7, 
               temporal_26.1, temporal_26.2, temporal_28.1, 
               temporal_28.2, temporal_28.3
)

clustering <- data.frame(matrix(ncol = 0, nrow = 18))
for (i in kmeans) {
  clusters = n_distinct(i$km_cluster)
  cluster_ID = unique(c(as.character(i$cluster_ID)))
  city = unique(c(as.character(i$cluster_location)))
  clustering = rbind(clustering, data.frame(clusters, cluster_ID, city))
}

clustering$modality <- ifelse(clustering$clusters == 1, "Uni", "Multi")

calculate_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

clustering <- clustering %>% 
  group_by(city) %>% 
  mutate(majority_modality = calculate_mode(modality))

write.csv(clustering, 'data/timeseriesModality.csv')

tapData$cluster_ID <- as.character(tapData$cluster_ID)
tapData$cluster_ID <- replace(tapData$cluster_ID, tapData$cluster_ID == "1.1", "1.10")
clustering$cluster_ID <- replace(clustering$cluster_ID, clustering$cluster_ID == "1.1", "1.10")
df <- tapData %>%
  filter(cluster_location == "Salt Lake City" | cluster_location == "San Francisco" | 
cluster_location == "Los Angeles" | cluster_location == "Phoenix") %>%
  left_join(clustering, by = 'cluster_ID')

# Density Plots ----------------------------------------------------------

df$Cluster_ID <- fct_relevel(df$cluster_ID, #order these temporally for plotting
                             "25.7", "25.6", "25.5", "25.4", "25.3", "25.2", 
                             "25.1", "1.11", "1.10", "1.09", "1.08", "1.07",
                             "1.06", "1.05", "1.04", "1.03", "1.02", "1.01")

ggplot(data = df, aes(y = cluster_location_time, x = d18O, fill = modality)) + 
  geom_density_ridges(scale = 3, rel_min_height = 0.01, stat = "density_ridges") +
  scale_fill_manual(values = c("#003f5c", "#d2042d")) + 
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
ggsave("figures/density_time_slice.pdf", width=6, height=5, units="in", dpi=600)

# Comparing IDR -----------------------------------------------------------

idr <- df %>% 
  group_by(Cluster_ID) %>% 
  summarize(
    IDR_O = round(abs(diff(quantile(.data$d18O, c(0.1, 0.9), names = F))), 2), 
    IDR_d_ex = round(abs(diff(quantile(.data$d_ex, c(0.1, 0.9), names = F))), 2), 
    IDR_H = round(abs(diff(quantile(.data$d2H, c(0.1, 0.9), names = F))), 2),
  ) %>% 
  select(Cluster_ID, IDR_O, IDR_d_ex, IDR_H) %>% 
  mutate(city = Cluster_ID, 
         city = if_else(grepl('25.*', city), "SF", "SLC"))

write.csv(idr, 'data/timeseriesSummary.csv')
mean(subset(idr, city == 'SLC')$IDR_O)
mean(subset(idr, city == 'SF')$IDR_O)
#these numbers manually entered into datasummary for now. 
