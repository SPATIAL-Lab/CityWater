#Let's try creating shapefiles matching the US Census using tigris, a package which directly pulls from Census data and plays well with tidyverse 
#This assumed G0 has been run. 
library(tigris)
library(ggplot2)

davis <- block_groups("UT", "Davis")
salt_lake <- block_groups("UT", "Salt Lake", cb = T)
SLC <- rbind_tigris(davis, salt_lake)
