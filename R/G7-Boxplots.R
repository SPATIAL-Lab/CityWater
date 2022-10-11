###BOXPLOTS time slices for SLC and SF####

names(tapData_1)
unique(tapData_1$Cluster_ID)
unique(tapData_1$Cluster_Location)

#Boxplot SLC and SF time slices...
Aboxtest_SLC.SF <- tapData_1 %>%
  filter(Cluster_ID %in% c("1.1","1.2","1.3","1.4","1.5","1.6","1.7","1.8",
                           "1.9","1.1.0","1.11","25.1","25.2","25.3",
                           "25.3","25.4","25.5","25.6","25.7")) %>%
  ggplot(aes(x=Cluster_ID, y=d18O)) + 
  geom_boxplot() +
  stat_summary(fun="mean", fill="blue", shape=23) +
  theme_bw(base_size = 16)

Aboxtest_SLC.SF <- Aboxtest_SLC.SF + theme(axis.text.x = element_text(angle=90))

ggsave("boxplot_time_slice.tiff", width=6, height=4, units="in", dpi=300)
