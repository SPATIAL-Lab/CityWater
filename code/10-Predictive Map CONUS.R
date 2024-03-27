# Predictive map of contiguous US. Note: you'll need a Census API  --------

library(censusapi);library(tigris, options(tigris_use_cache = TRUE)); library(viridis); 
library(ggplot2); library(dplyr); library(terra); library(readxl); 
library(usmap)

datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(2:4, 6:13, 16, 17)]
multivariate <- read.csv("data/multivariate.csv")
multilevel <- left_join(multivariate, datasummary, by = 'cluster_location') %>% 
  rename('idr' = 'IDR_O')

model <- multilevel %>% 
  select(idr, streamflow, lon, ruggedness, total_area)

model$streamflow = log(model$streamflow)

# streamflow
streamflow <- rast("maps/streamflow_mean.tif")
streamflow = log(streamflow)

# ruggedness
ruggedness <- rast("maps/elev_diff.tif")

# Longitude
lon = streamflow
lon = project(lon, "WGS84")
lc = crds(lon, na.rm = FALSE)
values(lon) = lc[, 1]
lon = project(lon, streamflow)
lon = mask(lon, streamflow)

# City area
total_area = vect("maps/cb_2018_us_ua10_500k.shp")
total_area$total_area = expanse(total_area, unit = "km")
total_area = project(total_area, streamflow)
total_area = crop(total_area, streamflow)
total_area = rasterize(total_area, streamflow, field = "total_area")
cval = values(total_area)[ ,1]
cval[is.na(cval)] = 2.5
values(total_area) = cval
total_area = mask(total_area, streamflow)

# Stack rasters
s <- c(streamflow, lon, ruggedness, total_area)
names(s) <- c("streamflow", "lon", "ruggedness", "total_area")

# Fit the model
best_model <- lm((idr) ~ streamflow + lon + ruggedness + total_area,
                 data = model)

# Predict
O_pred <- max(predict(s, best_model), min(model$idr))^2

# Plot
st <- vect("maps/cb_2018_us_state_5m.shp")
st <-  project(st, "ESRI:102003")
st <- crop(st, O_pred)
usa = aggregate(st)

# plot outcome
png(filename = "figures/Fig5.png", width = 7, height = 4.8, units = 'in', 
    res = 600)
plot(O_pred, col = viridis(100), mar = c(2, 2, 2, 6), 
     axes = FALSE, box = FALSE)
plot(st, col= NA, border = 'light grey', add = TRUE)
plot(usa, col = NA, add = TRUE, lw = 2)
mtext(expression("Tap water "*delta^{18}*"O IDR"), side = 4, line = 3.5)
dev.off()

# fraction of USA w/ IDR < 1 or 2 per mil
sum(values(O_pred) < 1, na.rm = TRUE) / sum(!is.na(values(O_pred)))
sum(values(O_pred) < 2, na.rm = TRUE) / sum(!is.na(values(O_pred)))
