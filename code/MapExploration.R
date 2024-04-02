# mapping different city outlines, inclusing CSAs (Combined Statistical Areas)
library(terra); library(tidyterra); library(ggplot2)

csa = vect("maps/cb_2018_us_csa_500k.shp")
city2018 = vect("maps/cb_2018_us_ua10_500k.shp")
city2018 = crop(city2018, csa)

datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(2:4, 6:13, 16, 17)]
datasummary <- vect(datasummary, geom = c("lon", "lat"), crs = "WGS84")
datasummary = crop(datasummary, csa)
extracted <- terra::extract(csa, datasummary)

smushed <- rbind(csa, city2018)
smushed <- union(csa, city2018)
  
ggplot()+ 
  geom_spatvector(data = csa, aes(col = NAME)) + 
  theme_void() + 
  theme(legend.position = 'none')

ggplot()+ 
  geom_spatvector(data = city2018, aes(col = NAME10)) + 
  theme_void() + 
  theme(legend.position = 'none')

