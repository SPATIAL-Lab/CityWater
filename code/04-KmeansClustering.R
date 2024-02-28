# Used to generate k-means cluster analyses of locations and then data summary. 
# note: timeseriesModality.csv created by 03, but in data folder
library(factoextra); library(readr); library(forcats); library(dplyr)

tapData <- read_csv("data/cityWater.csv", 
                    col_types = cols(cluster_ID = col_character()))
AA <- tapData[tapData$cluster_location_time == "Ann Arbor", ] # Ann Arbor
ABQ <- tapData[tapData$cluster_location_time == "Albuquerque", ]
ATH <- tapData[tapData$cluster_location_time == "Athens", ]
ATL <- tapData[tapData$cluster_location_time == "Atlanta", ]
BEL <- tapData[tapData$cluster_location_time == "Bellingham", ] # Bellingham
CED <- tapData[tapData$cluster_location_time == "Cedar City", ] #Cedar City
COL <- tapData[tapData$cluster_location_time == "Colorado Springs", ] # Colorado Springs
DEN <- tapData[tapData$cluster_location_time == "Denver", ] #Denver
DFW <- tapData[tapData$cluster_location_time == "Dallas Fort Worth", ]
FLG <- tapData[tapData$cluster_location_time == "Flagstaff", ]
GNV <- tapData[tapData$cluster_location_time == "Gainesville", ]
HI <- tapData[tapData$cluster_location_time == "Hawaii", ] #Hawai'i
LAW <- tapData[tapData$cluster_location_time == "Lawrence", ] #Lawrence
LAX <- tapData[tapData$cluster_location == "Los Angeles", ]
LCR <- tapData[tapData$cluster_location_time == "LaCrosse", ] # LaCrosse
MOR <- tapData[tapData$cluster_location_time == "Morristown", ]# Morristown
MSP <- tapData[tapData$cluster_location_time == "Minneapolis", ] # Minneapolis
NAS <- tapData[tapData$cluster_location_time == "Nashville", ] #Nashville
OA <- tapData[tapData$cluster_location_time == "Oahu", ]
PHX <- tapData[tapData$cluster_location == "Phoenix", ]
PTD <- tapData[tapData$cluster_location_time == "Portland", ] #Portland
SC <- tapData[tapData$cluster_location_time == "State College", ] # State College
SD <- tapData[tapData$cluster_location == "San Diego", ]
SM <- tapData[tapData$cluster_location_time == "San Marcos", ]
SP <- tapData[tapData$cluster_location_time == "St Petersburg", ]
WOO <- tapData[tapData$cluster_location_time == "Wooster", ] # Wooster
YNG <- tapData[tapData$cluster_location == "Youngstown", ]

# Calculating K-means -----------------------------------------------------

# takes a couple minutes, take a break and re-hydrate. 
kmeans <- list(AA, ABQ, ATH, ATL, BEL, CED, COL, DEN, DFW, FLG, GNV, HI, LAW, 
               LAX, LCR, MOR, MSP, NAS, OA, PHX, PTD, SC, SD, SM, SP, WOO, YNG)

df <- data.frame(matrix(ncol = 0, nrow = 3929))
for (i in kmeans) {
 km <-  i %>% 
  select(c(17, 16)) %>% 
  eclust("kmeans", nboot = 500)
 sample_ID <- i$sample_ID
 cc <- km$cluster
 df <- rbind(df, data.frame(sample_ID, cc))
}  

tapData <- left_join(tapData, df) %>% 
  rename(cluster_number = cc)

#import SLC and SF since we don't need to do that again
slcsf <- read_csv("data/timeseriesModality.csv", 
                  col_types = cols(cluster_ID = col_character())) %>% 
  select(-c(city, modality, '...1', clusters))

tapData <- tapData %>% 
  group_by(cluster_ID) %>% 
  mutate(modality = ifelse(mean(cluster_number) == 1, "Uni", "Multi"))

tapData <- left_join(tapData, slcsf) %>% 
  mutate(modality = coalesce(modality, majority_modality)) %>% 
  select(-c(majority_modality))

write.csv(tapData, 'data/clustering.csv')
# Modality ----------------------------------------------------------------

datasummary <- tapData %>%
  group_by(cluster_location) %>%
  summarize(across(c(d18O, d2H, d_ex), list(
    min = min, 
    max = max, 
    mean = mean, 
    sd = sd, 
    median = median
  )))

datasummary2 <- tapData %>%
  group_by(cluster_location, modality) %>%
  select(lat, lon) %>% 
  summarize(n = n(), 
            lat = mean(lat), 
            lon = mean(lon), 
  )

datasummary3 <- tapData %>% 
  group_by(cluster_location) %>% 
  summarize(
    IDR_O = round(abs(diff(quantile(.data$d18O, c(0.1, 0.9), names = F))), 1), 
    IDR_d_ex = round(abs(diff(quantile(.data$d_ex, c(0.1, 0.9), names = F))), 1), 
    IDR_H = round(abs(diff(quantile(.data$d2H, c(0.1, 0.9), names = F))), 1),
  )

datasummary <- datasummary %>% 
  left_join(datasummary2) %>% 
  left_join(datasummary3) %>% 
  mutate_at(2:13, round, 2)

write.csv(datasummary, "data/datasummary.csv") # note that SF and SLC reporting for mean IDR is from 01
