# This script is for the density plot of Figure 1. Some post-processing was done in Adobe Illustrator
library(raster); library(sf); library(tidyr); library(dplyr); library(factoextra)
tapData <- read_csv("data/cityWater.csv", 
                    col_types = cols(Cluster_ID = col_character()))

tapData$Cluster_ID <- factor(tapData$Cluster_ID)
tapData$Cluster_Location <- factor(tapData$Cluster_Location)
tapData$Cluster_Location_Time <- factor(tapData$Cluster_Location_Time)
tapData$Cluster_State <- factor(tapData$Cluster_State)
tapData$Project_ID <- factor(tapData$Project_ID)

#going spatial
tapData.sf <- st_as_sf(tapData, 
                       coords = c("Long", "Lat"),
                       crs = 4326) #EPSG code for WGS84

# Modality ----------------------------------------------------------------
#SLC
tapData.sf_SLC_1.01 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ap-13", ]
tapData.sf_SLC_1.02 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ag-Oct-13", ]
tapData.sf_SLC_1.03 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Feb-14", ]
tapData.sf_SLC_1.04 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ap-May-14", ]
tapData.sf_SLC_1.05 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ag-Sep-14", ]
tapData.sf_SLC_1.06 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ap-May-15", ]
tapData.sf_SLC_1.07 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Sep-Oct-15", ]
tapData.sf_SLC_1.08 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Ap-16", ]
tapData.sf_SLC_1.09 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Sep-16", ]
tapData.sf_SLC_1.10 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Mar-May-17", ]
tapData.sf_SLC_1.11 <- tapData.sf[tapData.sf$Cluster_Location_Time == "Salt Lake City_Oct-17", ]

#San Francisco
tapData.sf_SF_25.1 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Dic-13", ]
tapData.sf_SF_25.2 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Mar-Ap-14", ]
tapData.sf_SF_25.3 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Jun-14", ]
tapData.sf_SF_25.4 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Nov-14", ]
tapData.sf_SF_25.5 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Dic-14", ]
tapData.sf_SF_25.6 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Mar-15", ]
tapData.sf_SF_25.7 <- tapData.sf[tapData.sf$Cluster_Location_Time == "San Francisco_Jul-15", ]

tapData_SF_25.1 <- st_set_geometry(tapData.sf_SF_25.1, NULL)
tapData_SF_25.2 <- st_set_geometry(tapData.sf_SF_25.2, NULL)
tapData_SF_25.3 <- st_set_geometry(tapData.sf_SF_25.3, NULL)
tapData_SF_25.4 <- st_set_geometry(tapData.sf_SF_25.4, NULL)
tapData_SF_25.5 <- st_set_geometry(tapData.sf_SF_25.5, NULL)
tapData_SF_25.6 <- st_set_geometry(tapData.sf_SF_25.6, NULL)
tapData_SF_25.7 <- st_set_geometry(tapData.sf_SF_25.7, NULL)
tapData_SLC_1.01 <- st_set_geometry(tapData.sf_SLC_1.01, NULL)
tapData_SLC_1.02 <- st_set_geometry(tapData.sf_SLC_1.02, NULL)
tapData_SLC_1.03 <- st_set_geometry(tapData.sf_SLC_1.03, NULL)
tapData_SLC_1.04 <- st_set_geometry(tapData.sf_SLC_1.04, NULL)
tapData_SLC_1.05 <- st_set_geometry(tapData.sf_SLC_1.05, NULL)
tapData_SLC_1.06 <- st_set_geometry(tapData.sf_SLC_1.06, NULL)
tapData_SLC_1.07 <- st_set_geometry(tapData.sf_SLC_1.07, NULL)
tapData_SLC_1.08 <- st_set_geometry(tapData.sf_SLC_1.08, NULL)
tapData_SLC_1.09 <- st_set_geometry(tapData.sf_SLC_1.09, NULL)
tapData_SLC_1.10 <- st_set_geometry(tapData.sf_SLC_1.10, NULL)
tapData_SLC_1.11 <- st_set_geometry(tapData.sf_SLC_1.11, NULL)
#tapData_SF_25.1
km_SF_25.1 <- tapData_SF_25.1 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SF_25.1$km_cluster <- km_SF_25.1$cluster
tapData_SF_25.1$km_cluster <- factor(tapData_SF_25.1$km_cluster)

#tapData_SF_25.2
km_SF_25.2 <- tapData_SF_25.2 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SF_25.2$km_cluster <- km_SF_25.2$cluster
tapData_SF_25.2$km_cluster <- factor(tapData_SF_25.2$km_cluster)

#tapData_SF_25.3
km_SF_25.3 <- tapData_SF_25.3 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SF_25.3$km_cluster <- km_SF_25.3$cluster
tapData_SF_25.3$km_cluster <- factor(tapData_SF_25.3$km_cluster)

#tapData_SF_25.4
km_SF_25.4 <- tapData_SF_25.4 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SF_25.4$km_cluster <- km_SF_25.4$cluster
tapData_SF_25.4$km_cluster <- factor(tapData_SF_25.4$km_cluster)

#tapData_SF_25.5
km_SF_25.5 <- tapData_SF_25.5 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SF_25.5$km_cluster <- km_SF_25.5$cluster
tapData_SF_25.5$km_cluster <- factor(tapData_SF_25.5$km_cluster)

#tapData_SF_25.6
km_SF_25.6 <- tapData_SF_25.6 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SF_25.6$km_cluster <- km_SF_25.6$cluster
tapData_SF_25.6$km_cluster <- factor(tapData_SF_25.6$km_cluster)

#tapData_SF_25.7
km_SF_25.7 <- tapData_SF_25.7 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SF_25.7$km_cluster <- km_SF_25.7$cluster
tapData_SF_25.7$km_cluster <- factor(tapData_SF_25.7$km_cluster)

#tapData_SLC_1.1
km_SLC_1.01 <- tapData_SLC_1.01 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC_1.01$km_cluster <- km_SLC_1.01$cluster
tapData_SLC_1.01$km_cluster <- factor(tapData_SLC_1.01$km_cluster)

#tapData_SLC_1.02
km_SLC_1.02 <- tapData_SLC_1.02 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC_1.02$km_cluster <- km_SLC_1.02$cluster
tapData_SLC_1.02$km_cluster <- factor(tapData_SLC_1.02$km_cluster)

#tapData_SLC_1.03
km_SLC_1.03 <- tapData_SLC_1.03 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC_1.03$km_cluster <- km_SLC_1.03$cluster
tapData_SLC_1.03$km_cluster <- factor(tapData_SLC_1.03$km_cluster)

#tapData_SLC_1.04
km_SLC_1.04 <- tapData_SLC_1.04 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC_1.04$km_cluster <- km_SLC_1.04$cluster
tapData_SLC_1.04$km_cluster <- factor(tapData_SLC_1.04$km_cluster)

#tapData_SLC_1.05
km_SLC_1.05 <- tapData_SLC_1.05 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC_1.05$km_cluster <- km_SLC_1.05$cluster
tapData_SLC_1.05$km_cluster <- factor(tapData_SLC_1.05$km_cluster)

#tapData_SLC_1.06
km_SLC_1.06 <- tapData_SLC_1.06 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC_1.06$km_cluster <- km_SLC_1.06$cluster
tapData_SLC_1.06$km_cluster <- factor(tapData_SLC_1.06$km_cluster)

#tapData_SLC_1.07
km_SLC_1.07 <- tapData_SLC_1.07 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC_1.07$km_cluster <- km_SLC_1.07$cluster
tapData_SLC_1.07$km_cluster <- factor(tapData_SLC_1.07$km_cluster)

#tapData_SLC_1.08
km_SLC_1.08 <- tapData_SLC_1.08 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC_1.08$km_cluster <- km_SLC_1.08$cluster
tapData_SLC_1.08$km_cluster <- factor(tapData_SLC_1.08$km_cluster)

#tapData_SLC_1.09
km_SLC_1.09 <- tapData_SLC_1.09 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC_1.09$km_cluster <- km_SLC_1.09$cluster
tapData_SLC_1.09$km_cluster <- factor(tapData_SLC_1.09$km_cluster)

#tapData_SLC_1.10
km_SLC_1.10 <- tapData_SLC_1.10 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC_1.10$km_cluster <- km_SLC_1.10$cluster
tapData_SLC_1.10$km_cluster <- factor(tapData_SLC_1.10$km_cluster)

#tapData_SLC_1.11
km_SLC_1.11 <- tapData_SLC_1.11 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SLC_1.11$km_cluster <- km_SLC_1.11$cluster
tapData_SLC_1.11$km_cluster <- factor(tapData_SLC_1.11$km_cluster)

#Let's count the number of clusters and report modality in a reproducible table. 
kmeans <- list(tapData_SLC_1.01, tapData_SLC_1.02, tapData_SLC_1.03,
               tapData_SLC_1.04, tapData_SLC_1.05, tapData_SLC_1.06, 
               tapData_SLC_1.07, tapData_SLC_1.08, tapData_SLC_1.09, 
               tapData_SLC_1.10, tapData_SLC_1.11, tapData_SF_25.1, 
               tapData_SF_25.2, tapData_SF_25.3, tapData_SF_25.4, 
               tapData_SF_25.5, tapData_SF_25.6, tapData_SF_25.7
)

modality <- data.frame(matrix(ncol = 0, nrow = 18))
for (i in kmeans) {
  Clusters = n_distinct(i$km_cluster)
  Cluster_ID = unique(c(as.character(i$Cluster_ID)))
  City = unique(c(as.character(i$Cluster_Location)))
  modality = rbind(modality, data.frame(Clusters, Cluster_ID, City))
}
write.csv(modality, 'data/timeseriesModality.csv')

df <- tapData.sf %>%
  filter(Cluster_ID %in% c("1.01","1.02","1.03","1.04","1.05","1.06","1.07","1.08",
                           "1.09","1.10","1.11","25.1","25.2","25.3",
                           "25.3","25.4","25.5","25.6","25.7")) %>%
 left_join(modality, by = 'Cluster_ID') %>% 
  group_by(Cluster_ID) %>%
  mutate(Mode = ifelse(mean(Clusters) <= 1, "Uni", "Multi"))

# Density Plots ----------------------------------------------------------
library(ggridges)

df$Cluster_ID <- fct_relevel(df$Cluster_ID, #order these temporally for plotting
                             "25.7", "25.6", "25.5", "25.4", "25.3", "25.2", 
                             "25.1", "1.11", "1.10", "1.09", "1.08", "1.07",
                             "1.06", "1.05", "1.04", "1.03", "1.02", "1.01")

timeDensity <- ggplot(data = df, aes(y = Cluster_ID, x = d18O, fill = Mode)) + 
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
timeDensity
ggsave("figures/density_time_slice.pdf", width=6, height=5, units="in", dpi=300)

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