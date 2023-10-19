# Let's see what multi-level regression has to say about oxygen and d_excess values
# We're changing from SD to IDR. 
library(tidyr); library(dplyr); library(leaps); library(ggpubr); library(car)                        

tapData <- read.csv("data/cityWater.csv") 
tapData <- subset(tapData, Cluster_Location != "Oahu" & Cluster_Location != "Hawaii")
datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(1, 3:5, 7:14, 17, 18)]
multivariate <- read.csv("data/multivariate_test.csv")
multilevel <- left_join(multivariate, datasummary, by = 'Cluster_Location') %>% 
  rename('sd' = 'd18O_sd', 
         'idr' = 'IDR_O')

# now we want to examine the following variables: streamflow, precip, elevation_range, 
# total_area, popdensity, medincome, surfacewater_total, perc_water and water_use

# Modelling IDR--------------------------------------------------------------

model <- multilevel %>% 
  dplyr::select(idr, total_area, streamflow, precip, 
         lat, popdensity, medincome, water_use, ruggedness)

all_model <- lm(sqrt(idr) ~ .,
                 data = model)

summary(all_model)
# Choosing best predictors ------------------------------------------------
Best_Subset <- regsubsets(sqrt(idr) ~ .,
                          data = model,
                          nbest = 1,      # 1 best model for each number of predictors
                          nvmax = NULL,    # NULL for no limit on number of variables
                          force.in = NULL,
                          force.out = NULL,
                          method = "exhaustive")
summary_best_subset <- summary(Best_Subset)
as.data.frame(cbind(summary_best_subset$outmat, "bic" = round(summary_best_subset$bic, 2),
                    "adjr2" = round(summary_best_subset$adjr2, 2)))

summary_best_subset$which[which.min(summary_best_subset$bic),]
# okay, leaps suggests we drop precip, lat, popdensity, and water_use

best_model <- lm(sqrt(idr) ~ total_area + streamflow + medincome,
                 data = model)

summary(best_model)

#checking variance inflation factors for these independent variables 
vif(best_model) 
# So for popdensity especially there is moderate correlation between it and other factors, but not so much that we need to re-assess our model.

# Model comparison just to check it out
res.sum <- summary(Best_Subset)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)
plot(res.sum$adjr2)
plot(res.sum$bic)
plot(density(best_model$residuals))