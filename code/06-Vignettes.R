# Here we'll explore the four vignette cities only. clustering.csv generated from 02
library(ggpubr); library(terra);library(readr); 
library(forcats); library(dplyr); library(tidyterra)

# Setup -------------------------------------------------------------------
vign <- read_csv('data/clustering.csv', 
  col_types = cols(cluster_ID = col_character(), 
                   cluster_number = col_character())) %>% 
  subset(cluster_location == "Atlanta" | cluster_location == "Lawrence" | 
           cluster_location == "Minneapolis" | cluster_location == "Denver")

vectv <- vect(vign, geom = c('lon', 'lat'), crs = 'epsg:4326')

cities <- vect("maps/cb_2018_us_ua10_500k.shp")

# Mapping -----------------------------------------------------------------

ggplot() + 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Atlanta, GA")) + 
  geom_sf(data = terra::subset(vectv, vectv$cluster_location == "Atlanta"), 
          aes(fill = cluster_number), size = 4, shape = 21) + 
  scale_fill_manual(values = c("#003f5c", "#d2042d")) +
  theme_void() +
  theme(legend.position = "none")
ggsave('figures/ATLmap.pdf')

ggplot() + 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Denver--Aurora, CO")) + 
  geom_sf(data = terra::subset(vectv, vectv$cluster_location == "Denver"), 
          aes(fill = cluster_number), size = 4, shape = 21) + 
  scale_fill_manual(values = c("#003f5c", "#d2042d")) +
  theme_void() +
  theme(legend.position = "none")
ggsave('figures/DENmap.pdf')

ggplot() + 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Minneapolis--St. Paul, MN--WI")) + 
  geom_sf(data = terra::subset(vectv, vectv$cluster_location == "Minneapolis"), 
          aes(fill = cluster_number), size = 4, shape = 21) +   scale_fill_manual(values = c("#003f5c", "#d2042d")) +
  theme_void() +
  theme(legend.position = "none")
ggsave('figures/MSPmap.pdf')

ggplot() + 
  geom_sf(data = terra::subset(cities, cities$NAME10 == "Lawrence, KS")) + 
  geom_sf(data = terra::subset(vectv, vectv$cluster_location == "Lawrence"), 
          aes(fill = cluster_number), size = 4, shape = 21) +   scale_fill_manual(values = c("#003f5c", "#d2042d")) +
  scale_fill_manual(values = c("#003f5c", "#d2042d")) +
  theme_void() +
  theme(legend.position = "none")
ggsave('figures/LAWmap.pdf')