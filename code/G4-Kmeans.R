####De-convertion from spatial objects (sf) to non-spatial objects -Kmeans doesnt like sf-#######
tapData_1_ABQ_9 <- st_set_geometry(tapData.sf_ABQ_9, NULL)
tapData_1_Ann_19 <- st_set_geometry(tapData.sf_Ann_19, NULL)
tapData_1_Ath_8 <- st_set_geometry(tapData.sf_Ath_8, NULL)
tapData_1_Atl_7 <- st_set_geometry(tapData.sf_Atl_7, NULL)
tapData_1_Bell_22 <- st_set_geometry(tapData.sf_Bell_22, NULL)
tapData_1_Cedar_12 <- st_set_geometry(tapData.sf_Cedar_12, NULL)
tapData_1_ColoSp_13 <- st_set_geometry(tapData.sf_ColoSp_13, NULL)
tapData_1_Denv_15 <- st_set_geometry(tapData.sf_Denv_15, NULL)
tapData_1_DF_6 <- st_set_geometry(tapData.sf_DF_6, NULL)
tapData_1_Flag_10 <- st_set_geometry(tapData.sf_Flag_10, NULL)
tapData_1_Gaines_4 <- st_set_geometry(tapData.sf_Gaines_4, NULL)
tapData_1_Haw_24 <- st_set_geometry(tapData.sf_Haw_24, NULL)
tapData_1_LA_28.1 <- st_set_geometry(tapData.sf_LA_28.1, NULL)
tapData_1_LA_28.2 <- st_set_geometry(tapData.sf_LA_28.2, NULL)
tapData_1_LA_28.3 <- st_set_geometry(tapData.sf_LA_28.3, NULL)
tapData_1_LaCro_20 <- st_set_geometry(tapData.sf_LaCro_20, NULL)
tapData_1_Law_14 <- st_set_geometry(tapData.sf_Law_14, NULL)
tapData_1_MBS_16 <- st_set_geometry(tapData.sf_Morristown_16, NULL)
tapData_1_MPLS_21 <- st_set_geometry(tapData.sf_MPLS_21, NULL)
tapData_1_Nashv_11 <- st_set_geometry(tapData.sf_Nashv_11, NULL)
tapData_1_Oahu_2 <- st_set_geometry(tapData.sf_Oahu_2, NULL)
tapData_1_PHO_26.1 <- st_set_geometry(tapData.sf_PHO_26.1, NULL)
tapData_1_PHO_26.2 <- st_set_geometry(tapData.sf_PHO_26.2, NULL)
tapData_1_Port_23 <- st_set_geometry(tapData.sf_Port_23, NULL)
tapData_1_SanPete_3 <- st_set_geometry(tapData.sf_SanPete_3, NULL)
tapData_1_SC_17 <- st_set_geometry(tapData.sf_SC_17, NULL)
tapData_1_SD_27.1 <- st_set_geometry(tapData.sf_SD_27.1, NULL)
tapData_1_SD_27.2 <- st_set_geometry(tapData.sf_SD_27.2, NULL)
tapData_1_SF_25.1 <- st_set_geometry(tapData.sf_SF_25.1, NULL)
tapData_1_SF_25.2 <- st_set_geometry(tapData.sf_SF_25.2, NULL)
tapData_1_SF_25.3 <- st_set_geometry(tapData.sf_SF_25.3, NULL)
tapData_1_SF_25.4 <- st_set_geometry(tapData.sf_SF_25.4, NULL)
tapData_1_SF_25.5 <- st_set_geometry(tapData.sf_SF_25.5, NULL)
tapData_1_SF_25.6 <- st_set_geometry(tapData.sf_SF_25.6, NULL)
tapData_1_SF_25.7 <- st_set_geometry(tapData.sf_SF_25.7, NULL)
tapData_1_SLC_1.1 <- st_set_geometry(tapData.sf_SLC_1.1, NULL)
tapData_1_SLC_1.2 <- st_set_geometry(tapData.sf_SLC_1.2, NULL)
tapData_1_SLC_1.3 <- st_set_geometry(tapData.sf_SLC_1.3, NULL)
tapData_1_SLC_1.4 <- st_set_geometry(tapData.sf_SLC_1.4, NULL)
tapData_1_SLC_1.5 <- st_set_geometry(tapData.sf_SLC_1.5, NULL)
tapData_1_SLC_1.6 <- st_set_geometry(tapData.sf_SLC_1.6, NULL)
tapData_1_SLC_1.7 <- st_set_geometry(tapData.sf_SLC_1.7, NULL)
tapData_1_SLC_1.8 <- st_set_geometry(tapData.sf_SLC_1.8, NULL)
tapData_1_SLC_1.9 <- st_set_geometry(tapData.sf_SLC_1.9, NULL)
tapData_1_SLC_1.10 <- st_set_geometry(tapData.sf_SLC_1.10, NULL)
tapData_1_SLC_1.11 <- st_set_geometry(tapData.sf_SLC_1.11, NULL)
tapData_1_SM_5 <- st_set_geometry(tapData.sf_SM_5, NULL)
tapData_1_Woo_18 <- st_set_geometry(tapData.sf_Woo_18, NULL)


####K-MEANS#######

#tapData_1_ABQ_9
km_ABQ_9 <- tapData_1_ABQ_9 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_ABQ_9$km_cluster <- km_ABQ_9$cluster
tapData_1_ABQ_9$km_cluster <- factor(tapData_1_ABQ_9$km_cluster)
levels(tapData_1_ABQ_9$km_cluster)

#tapData_1_Ann_19
km_Ann_19 <- tapData_1_Ann_19 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Ann_19$km_cluster <- km_Ann_19$cluster
tapData_1_Ann_19$km_cluster <- factor(tapData_1_Ann_19$km_cluster)
levels(tapData_1_Ann_19$km_cluster)

#tapData_1_Ath_8
km_Ath_8 <- tapData_1_Ath_8 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Ath_8$km_cluster <- km_Ath_8$cluster
tapData_1_Ath_8$km_cluster <- factor(tapData_1_Ath_8$km_cluster)
levels(tapData_1_Ath_8$km_cluster)

#tapData_1_Atl_7
km_Atl_7 <- tapData_1_Atl_7 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Atl_7$km_cluster <- km_Atl_7$cluster
tapData_1_Atl_7$km_cluster <- factor(tapData_1_Atl_7$km_cluster)
levels(tapData_1_Atl_7$km_cluster)

#tapData_1_Bell_22
km_Bell_22 <- tapData_1_Bell_22 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Bell_22$km_cluster <- km_Bell_22$cluster
tapData_1_Bell_22$km_cluster <- factor(tapData_1_Bell_22$km_cluster)
levels(tapData_1_Bell_22$km_cluster)

#tapData_1_Cedar_12
km_Cedar_12 <- tapData_1_Cedar_12 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Cedar_12$km_cluster <- km_Cedar_12$cluster
tapData_1_Cedar_12$km_cluster <- factor(tapData_1_Cedar_12$km_cluster)
levels(tapData_1_Cedar_12$km_cluster)

#tapData_1_ColoSp_13
km_ColoSp_13 <- tapData_1_ColoSp_13 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_ColoSp_13$km_cluster <- km_ColoSp_13$cluster
tapData_1_ColoSp_13$km_cluster <- factor(tapData_1_ColoSp_13$km_cluster)
levels(tapData_1_ColoSp_13$km_cluster)

#tapData_1_Denv_15
km_Denv_15 <- tapData_1_Denv_15 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Denv_15$km_cluster <- km_Denv_15$cluster
tapData_1_Denv_15$km_cluster <- factor(tapData_1_Denv_15$km_cluster)
levels(tapData_1_Denv_15$km_cluster)

#tapData_1_DF_6
km_DF_6 <- tapData_1_DF_6 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_DF_6$km_cluster <- km_DF_6$cluster
tapData_1_DF_6$km_cluster <- factor(tapData_1_DF_6$km_cluster)
levels(tapData_1_DF_6$km_cluster)

#tapData_1_Flag_10
km_Flag_10 <- tapData_1_Flag_10 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Flag_10$km_cluster <- km_Flag_10$cluster
tapData_1_Flag_10$km_cluster <- factor(tapData_1_Flag_10$km_cluster)
levels(tapData_1_Flag_10$km_cluster)

#tapData_1_Gaines_4
km_Gaines_4 <- tapData_1_Gaines_4 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Gaines_4$km_cluster <- km_Gaines_4$cluster
tapData_1_Gaines_4$km_cluster <- factor(tapData_1_Gaines_4$km_cluster)
levels(tapData_1_Gaines_4$km_cluster)

#tapData_1_Haw_24
km_Haw_24 <- tapData_1_Haw_24 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Haw_24$km_cluster <- km_Haw_24$cluster
tapData_1_Haw_24$km_cluster <- factor(tapData_1_Haw_24$km_cluster)
levels(tapData_1_Haw_24$km_cluster)

#tapData_1_LA_28.1
km_LA_28.1 <- tapData_1_LA_28.1 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_LA_28.1$km_cluster <- km_LA_28.1$cluster
tapData_1_LA_28.1$km_cluster <- factor(tapData_1_LA_28.1$km_cluster)
levels(tapData_1_LA_28.1$km_cluster)

#tapData_1_LA_28.2
km_LA_28.2 <- tapData_1_LA_28.2 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_LA_28.2$km_cluster <- km_LA_28.2$cluster
tapData_1_LA_28.2$km_cluster <- factor(tapData_1_LA_28.2$km_cluster)
levels(tapData_1_LA_28.2$km_cluster)

#tapData_1_LA_28.3
km_LA_28.3 <- tapData_1_LA_28.3 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_LA_28.3$km_cluster <- km_LA_28.3$cluster
tapData_1_LA_28.3$km_cluster <- factor(tapData_1_LA_28.3$km_cluster)
levels(tapData_1_LA_28.3$km_cluster)

#tapData_1_LaCro_20
km_LaCro_20 <- tapData_1_LaCro_20 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_LaCro_20$km_cluster <- km_LaCro_20$cluster
tapData_1_LaCro_20$km_cluster <- factor(tapData_1_LaCro_20$km_cluster)
levels(tapData_1_LaCro_20$km_cluster)

#tapData_1_Law_14
km_Law_14 <- tapData_1_Law_14 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Law_14$km_cluster <- km_Law_14$cluster
tapData_1_Law_14$km_cluster <- factor(tapData_1_Law_14$km_cluster)
levels(tapData_1_Law_14$km_cluster)

#tapData_1_MBS_16
km_MBS_16 <- tapData_1_MBS_16 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_MBS_16$km_cluster <- km_MBS_16$cluster
tapData_1_MBS_16$km_cluster <- factor(tapData_1_MBS_16$km_cluster)
levels(tapData_1_MBS_16$km_cluster)

#tapData_1_MPLS_21
km_MPLS_21 <- tapData_1_MPLS_21 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_MPLS_21$km_cluster <- km_MPLS_21$cluster
tapData_1_MPLS_21$km_cluster <- factor(tapData_1_MPLS_21$km_cluster)
levels(tapData_1_MPLS_21$km_cluster)

#tapData_1_Nashv_11
km_Nashv_11 <- tapData_1_Nashv_11 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Nashv_11$km_cluster <- km_Nashv_11$cluster
tapData_1_Nashv_11$km_cluster <- factor(tapData_1_Nashv_11$km_cluster)
levels(tapData_1_Nashv_11$km_cluster)

#tapData_1_Oahu_2
km_Oahu_2 <- tapData_1_Oahu_2 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Oahu_2$km_cluster <- km_Oahu_2$cluster
tapData_1_Oahu_2$km_cluster <- factor(tapData_1_Oahu_2$km_cluster)
levels(tapData_1_Oahu_2$km_cluster)

#tapData_1_PHO_26.1
km_PHO_26.1 <- tapData_1_PHO_26.1 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_PHO_26.1$km_cluster <- km_PHO_26.1$cluster
tapData_1_PHO_26.1$km_cluster <- factor(tapData_1_PHO_26.1$km_cluster)
levels(tapData_1_PHO_26.1$km_cluster)

#tapData_1_PHO_26.2
km_PHO_26.2 <- tapData_1_PHO_26.2 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_PHO_26.2$km_cluster <- km_PHO_26.2$cluster
tapData_1_PHO_26.2$km_cluster <- factor(tapData_1_PHO_26.2$km_cluster)
levels(tapData_1_PHO_26.2$km_cluster)

#tapData_1_Port_23
km_Port_23 <- tapData_1_Port_23 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Port_23$km_cluster <- km_Port_23$cluster
tapData_1_Port_23$km_cluster <- factor(tapData_1_Port_23$km_cluster)
levels(tapData_1_Port_23$km_cluster)

#tapData_1_SanPete_3
km_SanPete_3 <- tapData_1_SanPete_3 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SanPete_3$km_cluster <- km_SanPete_3$cluster
tapData_1_SanPete_3$km_cluster <- factor(tapData_1_SanPete_3$km_cluster)
levels(tapData_1_SanPete_3$km_cluster)

#tapData_1_SC_17
km_SC_17 <- tapData_1_SC_17 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SC_17$km_cluster <- km_SC_17$cluster
tapData_1_SC_17$km_cluster <- factor(tapData_1_SC_17$km_cluster)
levels(tapData_1_SC_17$km_cluster)

#tapData_1_SD_27.1
km_SD_27.1 <- tapData_1_SD_27.1 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SD_27.1$km_cluster <- km_SD_27.1$cluster
tapData_1_SD_27.1$km_cluster <- factor(tapData_1_SD_27.1$km_cluster)
levels(tapData_1_SD_27.1$km_cluster)

#tapData_1_SD_27.2
km_SD_27.2 <- tapData_1_SD_27.2 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SD_27.2$km_cluster <- km_SD_27.2$cluster
tapData_1_SD_27.2$km_cluster <- factor(tapData_1_SD_27.2$km_cluster)
levels(tapData_1_SD_27.2$km_cluster)

#tapData_1_SF_25.1
km_SF_25.1 <- tapData_1_SF_25.1 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SF_25.1$km_cluster <- km_SF_25.1$cluster
tapData_1_SF_25.1$km_cluster <- factor(tapData_1_SF_25.1$km_cluster)
levels(tapData_1_SF_25.1$km_cluster)

#tapData_1_SF_25.2
km_SF_25.2 <- tapData_1_SF_25.2 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SF_25.2$km_cluster <- km_SF_25.2$cluster
tapData_1_SF_25.2$km_cluster <- factor(tapData_1_SF_25.2$km_cluster)
levels(tapData_1_SF_25.2$km_cluster)

#tapData_1_SF_25.3
km_SF_25.3 <- tapData_1_SF_25.3 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SF_25.3$km_cluster <- km_SF_25.3$cluster
tapData_1_SF_25.3$km_cluster <- factor(tapData_1_SF_25.3$km_cluster)
levels(tapData_1_SF_25.3$km_cluster)

#tapData_1_SF_25.4
km_SF_25.4 <- tapData_1_SF_25.4 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SF_25.4$km_cluster <- km_SF_25.4$cluster
tapData_1_SF_25.4$km_cluster <- factor(tapData_1_SF_25.4$km_cluster)
levels(tapData_1_SF_25.4$km_cluster)

#tapData_1_SF_25.5
km_SF_25.5 <- tapData_1_SF_25.5 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SF_25.5$km_cluster <- km_SF_25.5$cluster
tapData_1_SF_25.5$km_cluster <- factor(tapData_1_SF_25.5$km_cluster)
levels(tapData_1_SF_25.5$km_cluster)

#tapData_1_SF_25.6
km_SF_25.6 <- tapData_1_SF_25.6 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SF_25.6$km_cluster <- km_SF_25.6$cluster
tapData_1_SF_25.6$km_cluster <- factor(tapData_1_SF_25.6$km_cluster)
levels(tapData_1_SF_25.6$km_cluster)

#tapData_1_SF_25.7
km_SF_25.7 <- tapData_1_SF_25.7 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SF_25.7$km_cluster <- km_SF_25.7$cluster
tapData_1_SF_25.7$km_cluster <- factor(tapData_1_SF_25.7$km_cluster)
levels(tapData_1_SF_25.7$km_cluster)

#tapData_1_SLC_1.1
km_SLC_1.1 <- tapData_1_SLC_1.1 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SLC_1.1$km_cluster <- km_SLC_1.1$cluster
tapData_1_SLC_1.1$km_cluster <- factor(tapData_1_SLC_1.1$km_cluster)
levels(tapData_1_SLC_1.1$km_cluster)

#tapData_1_SLC_1.2
km_SLC_1.2 <- tapData_1_SLC_1.2 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SLC_1.2$km_cluster <- km_SLC_1.2$cluster
tapData_1_SLC_1.2$km_cluster <- factor(tapData_1_SLC_1.2$km_cluster)
levels(tapData_1_SLC_1.2$km_cluster)

#tapData_1_SLC_1.3
km_SLC_1.3 <- tapData_1_SLC_1.3 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SLC_1.3$km_cluster <- km_SLC_1.3$cluster
tapData_1_SLC_1.3$km_cluster <- factor(tapData_1_SLC_1.3$km_cluster)
levels(tapData_1_SLC_1.3$km_cluster)

#tapData_1_SLC_1.4
km_SLC_1.4 <- tapData_1_SLC_1.4 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SLC_1.4$km_cluster <- km_SLC_1.4$cluster
tapData_1_SLC_1.4$km_cluster <- factor(tapData_1_SLC_1.4$km_cluster)
levels(tapData_1_SLC_1.4$km_cluster)

#tapData_1_SLC_1.5
km_SLC_1.5 <- tapData_1_SLC_1.5 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SLC_1.5$km_cluster <- km_SLC_1.5$cluster
tapData_1_SLC_1.5$km_cluster <- factor(tapData_1_SLC_1.5$km_cluster)
levels(tapData_1_SLC_1.5$km_cluster)

#tapData_1_SLC_1.6
km_SLC_1.6 <- tapData_1_SLC_1.6 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SLC_1.6$km_cluster <- km_SLC_1.6$cluster
tapData_1_SLC_1.6$km_cluster <- factor(tapData_1_SLC_1.6$km_cluster)
levels(tapData_1_SLC_1.6$km_cluster)

#tapData_1_SLC_1.7
km_SLC_1.7 <- tapData_1_SLC_1.7 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SLC_1.7$km_cluster <- km_SLC_1.7$cluster
tapData_1_SLC_1.7$km_cluster <- factor(tapData_1_SLC_1.7$km_cluster)
levels(tapData_1_SLC_1.7$km_cluster)

#tapData_1_SLC_1.8
km_SLC_1.8 <- tapData_1_SLC_1.8 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SLC_1.8$km_cluster <- km_SLC_1.8$cluster
tapData_1_SLC_1.8$km_cluster <- factor(tapData_1_SLC_1.8$km_cluster)
levels(tapData_1_SLC_1.8$km_cluster)

#tapData_1_SLC_1.9
km_SLC_1.9 <- tapData_1_SLC_1.9 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SLC_1.9$km_cluster <- km_SLC_1.9$cluster
tapData_1_SLC_1.9$km_cluster <- factor(tapData_1_SLC_1.9$km_cluster)
levels(tapData_1_SLC_1.9$km_cluster)

#tapData_1_SLC_1.10
km_SLC_1.10 <- tapData_1_SLC_1.10 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SLC_1.10$km_cluster <- km_SLC_1.10$cluster
tapData_1_SLC_1.10$km_cluster <- factor(tapData_1_SLC_1.10$km_cluster)
levels(tapData_1_SLC_1.10$km_cluster)

#tapData_1_SLC_1.11
km_SLC_1.11 <- tapData_1_SLC_1.11 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SLC_1.11$km_cluster <- km_SLC_1.11$cluster
tapData_1_SLC_1.11$km_cluster <- factor(tapData_1_SLC_1.11$km_cluster)
levels(tapData_1_SLC_1.11$km_cluster)

#tapData_1_SM_5
km_SM_5 <- tapData_1_SM_5 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_SM_5$km_cluster <- km_SM_5$cluster
tapData_1_SM_5$km_cluster <- factor(tapData_1_SM_5$km_cluster)
levels(tapData_1_SM_5$km_cluster)

#tapData_1_Woo_18
km_Woo_18 <- tapData_1_Woo_18 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_1_Woo_18$km_cluster <- km_Woo_18$cluster
tapData_1_Woo_18$km_cluster <- factor(tapData_1_Woo_18$km_cluster)
levels(tapData_1_Woo_18$km_cluster)


# Modality ----------------------------------------------------------------

#Let's count the number of clusters and report modality in a reproducible table. 
kmeans <- list(tapData_1_ABQ_9, 
tapData_1_Ann_19, tapData_1_Ath_8,  tapData_1_Atl_7,  tapData_1_Bell_22       
tapData_1_Cedar_12       tapData_1_ColoSp_13      tapData_1_Denv_15        tapData_1_DF_6,  
tapData_1_Flag_10        tapData_1_Gaines_4       tapData_1_Haw_24, tapData_1_LA_28.1       
tapData_1_LA_28.2        tapData_1_LA_28.3        tapData_1_LaCro_20       tapData_1_Law_14        
tapData_1_MBS_16, tapData_1_MPLS_21        tapData_1_Nashv_11       tapData_1_Oahu_2        
tapData_1_PHO_26.1       tapData_1_PHO_26.2       tapData_1_Port_23        tapData_1_SanPete_3     
tapData_1_SC_17,  tapData_1_SD_27.1        tapData_1_SD_27.2        tapData_1_SF_25.1       
tapData_1_SF_25.2        tapData_1_SF_25.3        tapData_1_SF_25.4        tapData_1_SF_25.5       
tapData_1_SF_25.6        tapData_1_SF_25.7        tapData_1_SLC_1.1        tapData_1_SLC_1.10      
tapData_1_SLC_1.11       tapData_1_SLC_1.2        tapData_1_SLC_1.3        tapData_1_SLC_1.4       
tapData_1_SLC_1.5        tapData_1_SLC_1.6        tapData_1_SLC_1.7        tapData_1_SLC_1.8       
tapData_1_SLC_1.9        tapData_1_SM_5,   tapData_1_Woo_18  

modality <- lapply(kmeans, n_distinct(x$cluster))
modality <- lapply(kmeans, function(x) {x$clusters <- n_distinct(x$cluster)})

modality <- data.frame(matrix(ncol = 3, nrow = 48))
for (i in kmeans2) {
out <- n_distinct(i$km_cluster)
print(out)
}

n_distinct(km_ABQ_9$cluster)
