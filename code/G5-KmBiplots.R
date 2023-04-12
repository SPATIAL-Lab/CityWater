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


#tapData_LA_28.1
hull_km_LA_28.1 <- tapData_LA_28.1 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_LA_28.1 <- ggplot(data = tapData_LA_28.1, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_LA_28.1, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="LA_28.1") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_LA_28.1 <- Biplot_LA_28.1 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_LA_28.1, alpha = 0.5)


#tapData_LA_28.2
hull_km_LA_28.2 <- tapData_LA_28.2 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_LA_28.2 <- ggplot(data = tapData_LA_28.2, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_LA_28.2, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="LA_28.2") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_LA_28.2 <- Biplot_LA_28.2 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_LA_28.2, alpha = 0.5)

#tapData_LA_28.3
hull_km_LA_28.3 <- tapData_LA_28.3 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_LA_28.3 <- ggplot(data = tapData_LA_28.3, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_LA_28.3, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="LA_28.3") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_LA_28.3 <- Biplot_LA_28.3 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_LA_28.3, alpha = 0.5)

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

#tapData_PHO_26.1
hull_km_PHO_26.1 <- tapData_PHO_26.1 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_PHO_26.1 <- ggplot(data = tapData_PHO_26.1, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_PHO_26.1, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="PHO_26.1") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_PHO_26.1 <- Biplot_PHO_26.1 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_PHO_26.1, alpha = 0.5)


#tapData_PHO_26.2
hull_km_PHO_26.2 <- tapData_PHO_26.2 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

Biplot_PHO_26.2 <- ggplot(data = tapData_PHO_26.2, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 5, alpha = 0.5) +
  geom_smooth(data = tapData_PHO_26.2, aes(d18O, d2H, colour = "black"), colour = "grey", method = "lm",size = 1, se = F) + 
  stat_poly_eq( 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="PHO_26.2") + #tag = 'A'
  theme_bw(base_size = 16)

Biplot_PHO_26.2 <- Biplot_PHO_26.2 + aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_PHO_26.2, alpha = 0.5)


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

