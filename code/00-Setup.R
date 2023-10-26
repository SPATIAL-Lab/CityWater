# This script sets up everything for most other scripts, run completely. 
library(terra); library(dplyr); library(tidyterra); library(tidyr); library(ggplot2)
library(readr)

# Setup -------------------------------------------------------------------

tapData <- read_csv("data/cityWater.csv", 
                    col_types = cols(cluster_ID = col_character()))

tapDataVect <- vect(tapData, geom = c("lon", "lat"), crs = "epsg:4326")

# Subsetting Urban Areas and Slices ---------------------------------------

vectSLC01 <- tapDataVect[tapDataVect$cluster_location_time == "Salt Lake City_Ap-13", ]
vectSLC02 <- tapDataVect[tapDataVect$cluster_location_time == "Salt Lake City_Ag-Oct-13", ]
vectSLC03 <- tapDataVect[tapDataVect$cluster_location_time == "Salt Lake City_Feb-14", ]
vectSLC04 <- tapDataVect[tapDataVect$cluster_location_time == "Salt Lake City_Ap-May-14", ]
vectSLC05 <- tapDataVect[tapDataVect$cluster_location_time == "Salt Lake City_Ag-Sep-14", ]
vectSLC06 <- tapDataVect[tapDataVect$cluster_location_time == "Salt Lake City_Ap-May-15", ]
vectSLC07 <- tapDataVect[tapDataVect$cluster_location_time == "Salt Lake City_Sep-Oct-15", ]
vectSLC08 <- tapDataVect[tapDataVect$cluster_location_time == "Salt Lake City_Ap-16", ]
vectSLC09 <- tapDataVect[tapDataVect$cluster_location_time == "Salt Lake City_Sep-16", ]
vectSLC10 <- tapDataVect[tapDataVect$cluster_location_time == "Salt Lake City_Mar-May-17", ]
vectSLC11 <- tapDataVect[tapDataVect$cluster_location_time == "Salt Lake City_Oct-17", ]
vectSF25.1 <- tapDataVect[tapDataVect$cluster_location_time == "San Francisco_Dic-13", ]
vectSF25.2 <- tapDataVect[tapDataVect$cluster_location_time == "San Francisco_Mar-Ap-14", ]
vectSF25.3 <- tapDataVect[tapDataVect$cluster_location_time == "San Francisco_Jun-14", ]
vectSF25.4 <- tapDataVect[tapDataVect$cluster_location_time == "San Francisco_Nov-14", ]
vectSF25.5 <- tapDataVect[tapDataVect$cluster_location_time == "San Francisco_Dic-14", ]
vectSF25.6 <- tapDataVect[tapDataVect$cluster_location_time == "San Francisco_Mar-15", ]
vectSF25.7 <- tapDataVect[tapDataVect$cluster_location_time == "San Francisco_Jul-15", ]
vectPHX <- tapDataVect[tapDataVect$cluster_location == "Phoenix", ]
vectSD <- tapDataVect[tapDataVect$cluster_location == "San Diego", ]
vectLAX <- tapDataVect[tapDataVect$cluster_location == "Los Angeles", ]
vectOA <- tapDataVect[tapDataVect$cluster_location_time == "Oahu", ]
vectSP <- tapDataVect[tapDataVect$cluster_location_time == "St Petersburg", ]
vectGNV <- tapDataVect[tapDataVect$cluster_location_time == "Gainesville", ]
vectSM <- tapDataVect[tapDataVect$cluster_location_time == "San Marcos", ]
vectDFW <- tapDataVect[tapDataVect$cluster_location_time == "Dallas Fort Worth", ]
vectATL <- tapDataVect[tapDataVect$cluster_location_time == "Atlanta", ]
vectATH <- tapDataVect[tapDataVect$cluster_location_time == "Athens", ]
vectABQ <- tapDataVect[tapDataVect$cluster_location_time == "Albuquerque", ]
vectFLG <- tapDataVect[tapDataVect$cluster_location_time == "Flagstaff", ]
vectNAS <- tapDataVect[tapDataVect$cluster_location_time == "Nashville", ] #Nashville
vectCED <- tapDataVect[tapDataVect$cluster_location_time == "Cedar City", ] #Cedar City
vectCOL <- tapDataVect[tapDataVect$cluster_location_time == "Colorado Springs", ] # Colorado Springs
vectLAW <- tapDataVect[tapDataVect$cluster_location_time == "Lawrence", ] #Lawrence
vectDEN <- tapDataVect[tapDataVect$cluster_location_time == "Denver", ] #Denver
vectMOR <- tapDataVect[tapDataVect$cluster_location_time == "Morristown", ]# Morristown
vectSC <- tapDataVect[tapDataVect$cluster_location_time == "State College", ] # State College
vectWOO <- tapDataVect[tapDataVect$cluster_location_time == "Wooster", ] # Wooster
vectAA <- tapDataVect[tapDataVect$cluster_location_time == "Ann Arbor", ] # Ann Arbor
vectLCR <- tapDataVect[tapDataVect$cluster_location_time == "LaCrosse", ] # LaCrosse
vectMSP <- tapDataVect[tapDataVect$cluster_location_time == "Minneapolis", ] # Minneapolis
vectBEL <- tapDataVect[tapDataVect$cluster_location_time == "Bellingham", ] # Bellingham
vectPTD <- tapDataVect[tapDataVect$cluster_location_time == "Portland", ] #Portland
vectHI <- tapDataVect[tapDataVect$cluster_location_time == "Hawaii", ] #Hawai'i