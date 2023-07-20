# Run CW1 before this. 

# K-means Clustering Setup ------------------------------------------------

# Conversion from spatial objects (sf) to non-spatial objects -Kmeans doesnt like sf-#######
tapData_ABQ_9 <- st_set_geometry(tapData.sf_ABQ_9, NULL)
tapData_Ann_19 <- st_set_geometry(tapData.sf_Ann_19, NULL)
tapData_Ath_8 <- st_set_geometry(tapData.sf_Ath_8, NULL)
tapData_Atl_7 <- st_set_geometry(tapData.sf_Atl_7, NULL)
tapData_Bell_22 <- st_set_geometry(tapData.sf_Bell_22, NULL)
tapData_Cedar_12 <- st_set_geometry(tapData.sf_Cedar_12, NULL)
tapData_ColoSp_13 <- st_set_geometry(tapData.sf_ColoSp_13, NULL)
tapData_Denv_15 <- st_set_geometry(tapData.sf_Denv_15, NULL)
tapData_DF_6 <- st_set_geometry(tapData.sf_DF_6, NULL)
tapData_Flag_10 <- st_set_geometry(tapData.sf_Flag_10, NULL)
tapData_Gaines_4 <- st_set_geometry(tapData.sf_Gaines_4, NULL)
tapData_Haw_24 <- st_set_geometry(tapData.sf_Haw_24, NULL)
tapData_LA_28 <- st_set_geometry(tapData.sf_LA_28, NULL)
tapData_LaCro_20 <- st_set_geometry(tapData.sf_LaCro_20, NULL)
tapData_Law_14 <- st_set_geometry(tapData.sf_Law_14, NULL)
tapData_MBS_16 <- st_set_geometry(tapData.sf_Morristown_16, NULL)
tapData_MPLS_21 <- st_set_geometry(tapData.sf_MPLS_21, NULL)
tapData_Nashv_11 <- st_set_geometry(tapData.sf_Nashv_11, NULL)
tapData_Oahu_2 <- st_set_geometry(tapData.sf_Oahu_2, NULL)
tapData_PHX_26 <- st_set_geometry(tapData.sf_PHX_26, NULL)
tapData_Port_23 <- st_set_geometry(tapData.sf_Port_23, NULL)
tapData_SanPete_3 <- st_set_geometry(tapData.sf_SanPete_3, NULL)
tapData_SC_17 <- st_set_geometry(tapData.sf_SC_17, NULL)
tapData_SD_27 <- st_set_geometry(tapData.sf_SD_27, NULL)
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
tapData_SM_5 <- st_set_geometry(tapData.sf_SM_5, NULL)
tapData_Woo_18 <- st_set_geometry(tapData.sf_Woo_18, NULL)

# Calculating K-means -----------------------------------------------------

# takes a couple minutes, take a break and re-hydrate. 

#tapData_ABQ_9
km_ABQ_9 <- tapData_ABQ_9 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_ABQ_9$km_cluster <- km_ABQ_9$cluster
tapData_ABQ_9$km_cluster <- factor(tapData_ABQ_9$km_cluster)

#tapData_Ann_19
km_Ann_19 <- tapData_Ann_19 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Ann_19$km_cluster <- km_Ann_19$cluster
tapData_Ann_19$km_cluster <- factor(tapData_Ann_19$km_cluster)

#tapData_Ath_8
km_Ath_8 <- tapData_Ath_8 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Ath_8$km_cluster <- km_Ath_8$cluster
tapData_Ath_8$km_cluster <- factor(tapData_Ath_8$km_cluster)

#tapData_Atl_7
km_Atl_7 <- tapData_Atl_7 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Atl_7$km_cluster <- km_Atl_7$cluster
tapData_Atl_7$km_cluster <- factor(tapData_Atl_7$km_cluster)

#tapData_Bell_22
km_Bell_22 <- tapData_Bell_22 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Bell_22$km_cluster <- km_Bell_22$cluster
tapData_Bell_22$km_cluster <- factor(tapData_Bell_22$km_cluster)

#tapData_Cedar_12
km_Cedar_12 <- tapData_Cedar_12 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Cedar_12$km_cluster <- km_Cedar_12$cluster
tapData_Cedar_12$km_cluster <- factor(tapData_Cedar_12$km_cluster)

#tapData_ColoSp_13
km_ColoSp_13 <- tapData_ColoSp_13 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_ColoSp_13$km_cluster <- km_ColoSp_13$cluster
tapData_ColoSp_13$km_cluster <- factor(tapData_ColoSp_13$km_cluster)

#tapData_Denv_15
km_Denv_15 <- tapData_Denv_15 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Denv_15$km_cluster <- km_Denv_15$cluster
tapData_Denv_15$km_cluster <- factor(tapData_Denv_15$km_cluster)

#tapData_DF_6
km_DF_6 <- tapData_DF_6 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_DF_6$km_cluster <- km_DF_6$cluster
tapData_DF_6$km_cluster <- factor(tapData_DF_6$km_cluster)

#tapData_Flag_10
km_Flag_10 <- tapData_Flag_10 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Flag_10$km_cluster <- km_Flag_10$cluster
tapData_Flag_10$km_cluster <- factor(tapData_Flag_10$km_cluster)

#tapData_Gaines_4
km_Gaines_4 <- tapData_Gaines_4 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Gaines_4$km_cluster <- km_Gaines_4$cluster
tapData_Gaines_4$km_cluster <- factor(tapData_Gaines_4$km_cluster)

#tapData_Haw_24
km_Haw_24 <- tapData_Haw_24 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Haw_24$km_cluster <- km_Haw_24$cluster
tapData_Haw_24$km_cluster <- factor(tapData_Haw_24$km_cluster)

#tapData_LA_28
km_LA_28 <- tapData_LA_28 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_LA_28$km_cluster <- km_LA_28$cluster
tapData_LA_28$km_cluster <- factor(tapData_LA_28$km_cluster)

#tapData_LaCro_20
km_LaCro_20 <- tapData_LaCro_20 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_LaCro_20$km_cluster <- km_LaCro_20$cluster
tapData_LaCro_20$km_cluster <- factor(tapData_LaCro_20$km_cluster)

#tapData_Law_14
km_Law_14 <- tapData_Law_14 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Law_14$km_cluster <- km_Law_14$cluster
tapData_Law_14$km_cluster <- factor(tapData_Law_14$km_cluster)

#tapData_MBS_16
km_MBS_16 <- tapData_MBS_16 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_MBS_16$km_cluster <- km_MBS_16$cluster
tapData_MBS_16$km_cluster <- factor(tapData_MBS_16$km_cluster)

#tapData_MPLS_21
km_MPLS_21 <- tapData_MPLS_21 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_MPLS_21$km_cluster <- km_MPLS_21$cluster
tapData_MPLS_21$km_cluster <- factor(tapData_MPLS_21$km_cluster)

#tapData_Nashv_11
km_Nashv_11 <- tapData_Nashv_11 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Nashv_11$km_cluster <- km_Nashv_11$cluster
tapData_Nashv_11$km_cluster <- factor(tapData_Nashv_11$km_cluster)

#tapData_Oahu_2
km_Oahu_2 <- tapData_Oahu_2 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Oahu_2$km_cluster <- km_Oahu_2$cluster
tapData_Oahu_2$km_cluster <- factor(tapData_Oahu_2$km_cluster)

#tapData_PHX_26
km_PHX_26 <- tapData_PHX_26 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_PHX_26$km_cluster <- km_PHX_26$cluster
tapData_PHX_26$km_cluster <- factor(tapData_PHX_26$km_cluster)

#tapData_Port_23
km_Port_23 <- tapData_Port_23 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Port_23$km_cluster <- km_Port_23$cluster
tapData_Port_23$km_cluster <- factor(tapData_Port_23$km_cluster)

#tapData_SanPete_3
km_SanPete_3 <- tapData_SanPete_3 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SanPete_3$km_cluster <- km_SanPete_3$cluster
tapData_SanPete_3$km_cluster <- factor(tapData_SanPete_3$km_cluster)

#tapData_SC_17
km_SC_17 <- tapData_SC_17 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SC_17$km_cluster <- km_SC_17$cluster
tapData_SC_17$km_cluster <- factor(tapData_SC_17$km_cluster)

#tapData_SD_27
km_SD_27 <- tapData_SD_27 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SD_27$km_cluster <- km_SD_27$cluster
tapData_SD_27$km_cluster <- factor(tapData_SD_27$km_cluster)

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

#tapData_SM_5
km_SM_5 <- tapData_SM_5 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_SM_5$km_cluster <- km_SM_5$cluster
tapData_SM_5$km_cluster <- factor(tapData_SM_5$km_cluster)

#tapData_Woo_18
km_Woo_18 <- tapData_Woo_18 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Woo_18$km_cluster <- km_Woo_18$cluster
tapData_Woo_18$km_cluster <- factor(tapData_Woo_18$km_cluster)

# Modality ----------------------------------------------------------------

#Let's count the number of clusters and report modality in a reproducible table. 
kmeans <- list(tapData_ABQ_9, tapData_Ann_19, tapData_Ath_8,  
               tapData_Atl_7,  tapData_Bell_22, tapData_Cedar_12, 
               tapData_ColoSp_13, tapData_Denv_15, tapData_DF_6,
               tapData_Flag_10, tapData_Gaines_4, tapData_Haw_24,
               tapData_LA_28, tapData_LaCro_20, tapData_Law_14, 
               tapData_MBS_16, tapData_MPLS_21, tapData_Nashv_11, 
               tapData_Oahu_2, tapData_PHX_26, tapData_Port_23, 
               tapData_SanPete_3, tapData_SC_17,  tapData_SD_27, 
               tapData_SD_27, tapData_SF_25.1, tapData_SF_25.2, 
               tapData_SF_25.3, tapData_SF_25.4, tapData_SF_25.5,
               tapData_SF_25.6, tapData_SF_25.7, tapData_SLC_1.01, 
               tapData_SLC_1.10, tapData_SLC_1.11, tapData_SLC_1.02, 
               tapData_SLC_1.03, tapData_SLC_1.04, tapData_SLC_1.05, 
               tapData_SLC_1.06, tapData_SLC_1.07, tapData_SLC_1.08, 
               tapData_SLC_1.09,  tapData_SM_5,   tapData_Woo_18
                )

modality <- data.frame(matrix(ncol = 0, nrow = 48))
for (i in kmeans) {
  Clusters = n_distinct(i$km_cluster)
  Cluster_ID = unique(c(as.character(i$Cluster_ID)))
  modality = rbind(modality, data.frame(Clusters, Cluster_ID))
}

tapData <- left_join(tapData, modality) %>% 
  group_by(Cluster_Location) %>% 
  mutate(Modality = ifelse(mean(Clusters) <= 1, "Uni", "Multi"))

datasummary <- tapData %>%
  group_by(Cluster_Location) %>%
  summarize(across(c(d18O, d2H, d_ex), list(
    min = min, 
    max = max, 
    mean = mean, 
    sd = sd, 
    median = median
  )))

datasummary2 <- tapData %>%
  group_by(Cluster_Location, Modality) %>%
  select(Lat, Long) %>% 
  summarize(n = n(), 
            lat = mean(Lat), 
            lon = mean(Long), 
            )

datasummary3 <- tapData %>% 
  group_by(Cluster_Location) %>% 
  summarize(
    IDR_O = abs(diff(quantile(.data$d18O, c(0.1, 0.9), names = F))), 
    IDR_d_ex = abs(diff(quantile(.data$d_ex, c(0.1, 0.9), names = F))), 
    IDR_H = abs(diff(quantile(.data$d2H, c(0.1, 0.9), names = F))),
  )

datasummary <- datasummary %>% 
  left_join(datasummary2) %>% 
  left_join(datasummary3) %>% 
  mutate_at(2:13, round, 2)

write.csv(datasummary, "data/datasummary.csv")

# Scatter Plots of K-means Clusters ---------------------------------------

################BIPLOTS with clusters#################

#tapData_Ann_19
# Calculate the hulls for each group
hull_km_Ann_19 <- tapData_Ann_19 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_Ann_19 <- ggplot(data = tapData_Ann_19, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_Ann_19, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="Ann_19") + #tag = 'A'
  theme_bw(base_size = 16)

# Update the plot with a fill group, and overlay the new hulls
Biplot_Ann_19 <- Biplot_Ann_19 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_Ann_19, alpha = 0.5)


#tapData_Atl_7
hull_km_Atl_7 <- tapData_Atl_7 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_ATL <- ggplot(data = tapData_Atl_7, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_Atl_7, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="Atl_7") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_ATL <- Biplot_ATL + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_Atl_7, alpha = 0.5)

#tapData_Bell_22
hull_km_Bell_22 <- tapData_Bell_22 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_Bell_22 <- ggplot(data = tapData_Bell_22, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_Bell_22, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="Bell_22") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_Bell_22 <- Biplot_Bell_22 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_Bell_22, alpha = 0.5)

#tapData_ColoSp_13
hull_km_ColoSp_13 <- tapData_ColoSp_13 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_ColoSp_13 <- ggplot(data = tapData_ColoSp_13, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_ColoSp_13, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="ColoSp_13") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_ColoSp_13 <- Biplot_ColoSp_13 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_ColoSp_13, alpha = 0.5)

#tapData_DF_6
hull_km_DF_6 <- tapData_DF_6 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_DF_6 <- ggplot(data = tapData_DF_6, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_DF_6, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="DF_6") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_DF_6 <- Biplot_DF_6 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_DF_6, alpha = 0.5)

#tapData_Haw_24
hull_km_Haw_24 <- tapData_Haw_24 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_Haw_24 <- ggplot(data = tapData_Haw_24, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_Haw_24, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="Haw_24") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_Haw_24 <- Biplot_Haw_24 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_Haw_24, alpha = 0.5)


#tapData_LA_28
hull_km_LA_28 <- tapData_LA_28 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_LA_28 <- ggplot(data = tapData_LA_28, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_LA_28, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="LA_28.1") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_LA_28 <- Biplot_LA_28 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_LA_28, alpha = 0.5)

#tapData_Law_14
hull_km_Law_14 <- tapData_Law_14 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_Law_14 <- ggplot(data = tapData_Law_14, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_Law_14, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="Law_14") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_Law_14 <- Biplot_Law_14 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_Law_14, alpha = 0.5)

#tapData_PHO_26
hull_km_PHX_26 <- tapData_PHX_26 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_PHX_26 <- ggplot(data = tapData_PHX_26, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_PHX_26, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="PHX_26") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_PHX_26 <- Biplot_PHX_26 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_PHX_26, alpha = 0.5)

#tapData_Port_23
hull_km_Port_23 <- tapData_Port_23 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_Port_23 <- ggplot(data = tapData_Port_23, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_Port_23, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="Port_23") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_Port_23 <- Biplot_Port_23 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_Port_23, alpha = 0.5)

#tapData_SanPete_3
hull_km_SanPete_3 <- tapData_SanPete_3 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_SanPete_3 <- ggplot(data = tapData_SanPete_3, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_SanPete_3, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="SanPete_3") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_SanPete_3 <- Biplot_SanPete_3 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_SanPete_3, alpha = 0.5)

#tapData_SD_27
hull_km_SD_27 <- tapData_SD_27 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_SD_27 <- ggplot(data = tapData_SD_27, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_SD_27, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="SD_27.1") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_SD_27 <- Biplot_SD_27 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_SD_27, alpha = 0.5)

#tapData_SF_25.1
hull_km_SF_25.1 <- tapData_SF_25.1 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_SF_25.1 <- ggplot(data = tapData_SF_25.1, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_SF_25.1, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="SF_25.1") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_SF_25.1 <- Biplot_SF_25.1 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_SF_25.1, alpha = 0.5)


#tapData_SF_25.2
hull_km_SF_25.2 <- tapData_SF_25.2 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_SF_25.2 <- ggplot(data = tapData_SF_25.2, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_SF_25.2, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="SF_25.2") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_SF_25.2 <- Biplot_SF_25.2 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_SF_25.2, alpha = 0.5)

#tapData_SF_25.4
hull_km_SF_25.4 <- tapData_SF_25.4 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_SF_25.4 <- ggplot(data = tapData_SF_25.4, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_SF_25.4, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="SF_25.4") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_SF_25.4 <- Biplot_SF_25.4 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_SF_25.4, alpha = 0.5)

#tapData_SF_25.5
hull_km_SF_25.5 <- tapData_SF_25.5 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_SF_25.5 <- ggplot(data = tapData_SF_25.5, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_SF_25.5, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="SF_25.5") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_SF_25.5 <- Biplot_SF_25.5 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_SF_25.5, alpha = 0.5)

#tapData_SM_5
hull_km_SM_5 <- tapData_SM_5 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_SM_5 <- ggplot(data = tapData_SM_5, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_SM_5, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="SM_5") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_SM_5 <- Biplot_SM_5 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_SM_5, alpha = 0.5)

#tapData_SLC_1.11
hull_km_SLC_1.11 <- tapData_SLC_1.11 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_SLC_1.11 <- ggplot(data = tapData_SLC_1.11, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_SLC_1.11, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    parse = TRUE) +
  labs(title="SLC_1.11") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_SLC_1.11 <- Biplot_SLC_1.11 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_SLC_1.11, alpha = 0.5)
