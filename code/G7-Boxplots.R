###BOXPLOTS time slices for SLC and SF####

names(tapData.sf)
unique(tapData.sf$Cluster_ID)
unique(tapData.sf$Cluster_Location)

#Boxplot SLC and SF time slices...
Aboxtest_SLC.SF <- tapData.sf %>%
  filter(Cluster_ID %in% c("1.1","1.2","1.3","1.4","1.5","1.6","1.7","1.8",
                           "1.9","1.10","1.11","25.1","25.2","25.3",
                           "25.3","25.4","25.5","25.6","25.7")) %>%
  mutate(Cluster_ID = fct_relevel(Cluster_ID,
                       "1.1","1.2","1.3","1.4","1.5","1.6","1.7","1.8",
                           "1.9","1.10","1.11","25.1","25.2","25.3",
                           "25.4","25.5","25.6","25.7")) %>%
  ggplot(aes(x=Cluster_ID, y=d18O)) + 
  geom_boxplot() +
  stat_summary(fun="mean", fill="blue", shape=23) +
  theme_bw(base_size = 16) + 
  labs(
    y = expression(paste(delta^18, "O", " (\u2030, VSMOW)")), 
    x = "Cluster ID"
  ) +
  theme(axis.text.x = element_text(angle = 90))

Aboxtest_SLC.SF
ggsave("figures/boxplot_time_slice.tiff", width=6, height=4, units="in", dpi=300)