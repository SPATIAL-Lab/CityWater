# Here we'll explore the four vignette cities only. Have to run 1 before this
library(ggpubr)


# Setup -------------------------------------------------------------------
tapData_Atl_7 <- st_set_geometry(tapData.sf_Atl_7, NULL)
km_Atl_7 <- tapData_Atl_7 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Atl_7$km_cluster <- km_Atl_7$cluster
tapData_Atl_7$km_cluster <- factor(tapData_Atl_7$km_cluster)

tapData_Law_14 <- st_set_geometry(tapData.sf_Law_14, NULL)
km_Law_14 <- tapData_Law_14 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Law_14$km_cluster <- km_Law_14$cluster
tapData_Law_14$km_cluster <- factor(tapData_Law_14$km_cluster)

tapData_MPLS_21 <- st_set_geometry(tapData.sf_MPLS_21, NULL)
km_MPLS_21 <- tapData_MPLS_21 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_MPLS_21$km_cluster <- km_MPLS_21$cluster
tapData_MPLS_21$km_cluster <- factor(tapData_MPLS_21$km_cluster)

tapData_Denv_15 <- st_set_geometry(tapData.sf_Denv_15, NULL)
km_Denv_15 <- tapData_Denv_15 %>%
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
tapData_Denv_15$km_cluster <- km_Denv_15$cluster
tapData_Denv_15$km_cluster <- factor(tapData_Denv_15$km_cluster)

# Atlanta, which has small ranges and is multimodal

hull_km_Atl_7 <- tapData_Atl_7 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

p1 <- ggplot(data = tapData_Atl_7, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 3, alpha = 0.5) +
  aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_Atl_7, alpha = 0.5) + 
  labs(
    x = expression(paste(delta^18, "O", " (\u2030, VSMOW)")),
    y = expression(paste(delta^2, "H", " (\u2030, VSMOW)"))
  ) + 
  scale_fill_manual(values = c("#003f5c", "#d2042d")) + 
  theme_classic() + 
  theme(legend.position = 'none', 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_text(size = 16))

# Lawrence, which has wider d2H ranges and is multimodal

hull_km_Law_14 <- tapData_Law_14 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

p2 <- ggplot(data = tapData_Law_14, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 3, alpha = 0.5) +
  aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_Law_14, alpha = 0.5) + 
  labs(
    x = expression(paste(delta^18, "O", " (\u2030, VSMOW)")),
    y = expression(paste(delta^2, "H", " (\u2030, VSMOW)"))
  ) + 
  scale_fill_manual(values = c("#003f5c", "#d2042d")) + 
  theme_classic() + 
  theme(legend.position = 'none', 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_text(size = 16))

# Minneapolis, with small range and unimodal

hull_km_MPLS_21 <- tapData_MPLS_21 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

p3 <- ggplot(data = tapData_MPLS_21, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 3, alpha = 0.5) +
  aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_MPLS_21, alpha = 0.5) + 
  labs(
    x = expression(paste(delta^18, "O", " (\u2030, VSMOW)")),
    y = expression(paste(delta^2, "H", " (\u2030, VSMOW)"))
  ) + 
  scale_fill_manual(values = c("#003f5c", "#d2042d")) + 
  theme_classic() + 
  theme(legend.position = 'none', 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_text(size = 16))

# Denver, with large range and unimodal

hull_km_Denv_15 <- tapData_Denv_15 %>%
  group_by(km_cluster) %>%
  slice(chull(d18O, d2H))

p4 <- ggplot(data = tapData_Denv_15, aes(x = d18O, y = d2H)) + 
  geom_point(aes(fill = km_cluster), shape = 21, size = 3, alpha = 0.5) +
  aes(fill = factor(km_cluster)) + 
  geom_polygon(data = hull_km_Denv_15, alpha = 0.5) + 
  labs(
    x = expression(paste(delta^18, "O", " (\u2030, VSMOW)")),
    y = expression(paste(delta^2, "H", " (\u2030, VSMOW)"))
  ) + 
  scale_fill_manual(values = c("#003f5c", "#d2042d")) + 
  theme_classic() + 
  theme(legend.position = 'none', 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_text(size = 16))

# Maps --------------------------------------------------------------------

# Atlanta municipal water treatment centers include: 
#Chattahoochee Water Treatment Plant (33.822, -84.4527)
#Hemphill Water Treatment Plant (33.7893, -84.4084)
#North Area Water Treatment Plant (34.0210, -84.2254??? hard to find this one on Google)
atlantaWaterSources <- data.frame("name" = c("Chattahoochee", "Hemphill", "North Area"), 
                                   "lat" = c(33.822, 33.7893, 34.0210), 
                                   "lon" = c(-84.4527, -84.4084, -84.2254))
atlantaWaterSources <- st_as_sf(atlantaWaterSources, coords = c("lon", "lat"), 
                                 crs = 4326)

tapData.sf_Atl_7$km_cluster <- tapData_Atl_7$km_cluster
Atlmap <- mapview(tapData.sf_Atl_7, 
        zcol= 'km_cluster',
      #  color = "gray",          # outline color
        alpha.regions = 0.8,     # fill transparency
        alpha = 0.5, 
        col.regions = c("#003f5c", "#d2042d"), 
        layer.name = "Cluster")

#ARGHHHH BROKEN
mapviewOptions(fgb = FALSE)
mapshot(Atlmap, url = paste0(getwd(), "/ATL.html"),
        file = paste0(getwd(), "/ATL.png"))

#Lawrence municipal water treatment centers include: 
# Kaw River Water Treatment Plant (38.9808, -95.2408)
# Clinton Reservoir Water Treatment Plant (38.9519, -95.2985)
lawrenceWaterSources <- data.frame("name" = c("Kaw River Water Treatment Plant", "Clinton Reservoir Water Treatment Plant"), 
                                   "lat" = c(38.9808, 38.9519), 
                                   "lon" = c(-95.2408, -95.2985))
lawrenceWaterSources <- st_as_sf(lawrenceWaterSources, coords = c("lon", "lat"), 
                         crs = 4326)

tapData.sf_Law_14$km_cluster <- tapData_Law_14$km_cluster
lawrence <- mapview(tapData.sf_Law_14, 
        zcol= 'km_cluster',
        color = "gray",          # outline color
        alpha.regions = 0.8,     # fill transparency
        alpha = 0.5, 
        col.regions = c("#003f5c", "#d2042d"), 
        layer.name = "Cluster") + 
mapview(lawrenceWaterSources, 
        pch = c(17),
        layer.name = "Municipal Water Source")

mapshot(lawrence, url = paste0(getwd(), "/figures/lawrence.html"))

# Minneapolis
# Note: ~18 miles from Brooklyn Park (North) to Bloomington (South)
tapData.sf_MPLS_21$km_cluster <- tapData_MPLS_21$km_cluster
mapview(tapData.sf_MPLS_21, 
        zcol= 'km_cluster',
        color = "gray",          # outline color
        alpha.regions = 0.8,     # fill transparency
        alpha = 0.5, 
        col.regions = c("#003f5c", "#d2042d"))

# Denver
# Note: ~30 miles at the widest distance between points (Boulder to Centennial)
tapData.sf_Denv_15$km_cluster <- tapData_Denv_15$km_cluster
mapview(tapData.sf_Denv_15, 
        zcol= 'd18O',
        color = "gray",          # outline color
        alpha.regions = 0.8,     # fill transparency
        alpha = 0.5, 
        col.regions = c("#003f5c", "#d2042d")) + 
mapview(tapData.sf_Denv_15, 
          zcol= 'd2H',
          color = "gray",          # outline color
          alpha.regions = 0.8,     # fill transparency
          alpha = 0.5, 
          col.regions = c("#003f5c", "#d2042d"))


# Putting It All Together -------------------------------------------------

ggarrange(p1, p2, p3, p4, 
 #         labels = c('Atlanta', 'Lawrence', 'Minneapolis', 'Denver'), 
          ncol = 2, nrow = 2)

# Let's try making better inset maps --------------------------------------

cities <- read_sf("data/cb_2018_us_ua10_500k.shp")

tapData.sf_Atl_7$km_cluster <- tapData_Atl_7$km_cluster

ggplot() + 
  geom_sf(data = subset(cities, NAME10 == "Atlanta, GA")) + 
  geom_sf(data = tapData.sf_Atl_7, aes(fill = km_cluster), color = "black", size = 4, shape = 21) +
  geom_sf(data = atlantaWaterSources, fill = 'lightblue', color = "black", size = 4, shape = 21) +
  scale_fill_manual(values = c("#003f5c", "#d2042d")) +
  theme_void() +
  theme(legend.position = "none")

ggsave('figures/ATLmap.pdf')

tapData.sf_Denv_15$km_cluster <- tapData_Denv_15$km_cluster
ggplot() + 
  geom_sf(data = subset(cities, NAME10 == "Denver--Aurora, CO")) + 
  geom_sf(data = tapData.sf_Denv_15, aes(fill = km_cluster), color = "black", size = 4, shape = 21) +
  scale_fill_manual(values = c("#003f5c", "#d2042d")) +
  theme_void() +
  theme(legend.position = "none")
ggsave('figures/DENmap.pdf')

tapData.sf_MPLS_21$km_cluster <- tapData_MPLS_21$km_cluster
ggplot() + 
  geom_sf(data = subset(cities, NAME10 == "Minneapolis--St. Paul, MN--WI")) + 
  geom_sf(data = tapData.sf_MPLS_21, aes(fill = km_cluster), color = "black", size = 4, shape = 21) +
  scale_fill_manual(values = c("#003f5c", "#d2042d")) +
  theme_void() +
  theme(legend.position = "none")
ggsave('figures/MSPmap.pdf')

tapData.sf_Law_14$km_cluster <- tapData_Law_14$km_cluster
ggplot() + 
  geom_sf(data = subset(cities, NAME10 == "Lawrence, KS")) + 
  geom_sf(data = lawrenceWaterSources, fill = 'lightblue', color = "black", size = 4, shape = 21) +
  geom_sf(data = tapData.sf_Law_14, aes(fill = km_cluster), color = "black", size = 4, shape = 21) +
  scale_fill_manual(values = c("#003f5c", "#d2042d")) +
  theme_void() +
  theme(legend.position = "none")
ggsave('figures/LAWmap.pdf')
