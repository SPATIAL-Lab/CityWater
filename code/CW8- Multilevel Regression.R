# Let's see what multi-level regression has to say about oxygen and d_excess values
# We're changing from idr to IDR. 

library(tidyverse); library(leaps); library(ggpubr); library(car)                        

tapData <- read.csv("data/cityWater.csv") 
tapData <- subset(tapData, Cluster_Location != "Oahu" & Cluster_Location != "Hawaii")
datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(1, 3:5, 7:14, 17, 18)]
multivariate <- read.csv("data/multivariate.csv")
multilevel <- left_join(multivariate, datasummary, by = 'Cluster_Location') %>% 
  rename('sd' = 'd18O_sd', 
         'idr' = 'IDR_O')

# now we want to examine the following variables: streamflow, precip, elevation_range, 
# perc_water, total_area, popdensity, medincome
# Summarized Data ---------------------------------------------------------

# looking at d18O IDR and relationship to different variables

p1 <- ggplot(data = multilevel, aes(x = streamflow, y = idr)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 7.5, aes(label = ..rr.label..)) +
  geom_point() + 
  labs(
    x = "Streamflow",
    y = "Interdecile Range"
  ) +
  theme_classic()

p2 <- ggplot(data = multilevel, aes(x = precip, y = idr)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 7.5, aes(label = ..rr.label..)) +
  geom_point() +    
  labs(
    x = "Precipitation (mean)",
    y = ""
  ) +
  theme_classic()

p3 <- ggplot(data = multilevel, aes(x = elevation_range, y = idr)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 7.5, aes(label = ..rr.label..)) +
  geom_point() +       labs(
    x = "Elevation Range (m)",
    y = ""
  ) +                                
  theme_classic()

p4 <- ggplot(data = multilevel, aes(x = total_area, y = idr)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 7.5, aes(label = ..rr.label..)) +
  geom_point() + 
  labs(
    x = "Total area (km2)",
    y = ""
  ) +
  theme_classic()

p5 <- ggplot(data = multilevel, aes(x = popdensity, y = idr)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 7.5, aes(label = ..rr.label..)) +
  geom_point() +  
  labs(
    x = "Population Density",
    y = ""
  ) +
  theme_classic()

p6 <- ggplot(data = multilevel, aes(x = medincome, y = idr)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 7.5, aes(label = ..rr.label..)) +
  geom_point() +
  labs(
    x = "Median Income",
    y = "Interdecile Range"
  ) +
  theme_classic()

p7 <- ggplot(data = multilevel, aes(x = water_use, y = idr)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 7.5, aes(label = ..rr.label..)) +
  geom_point() +
  labs(
    x = "Water Use",
    y = "Interdecile Range"
  ) +
  theme_classic()

p8 <- ggplot(data = multilevel, aes(x = perc_water, y = idr)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 7.5, aes(label = ..rr.label..)) +
  geom_point() +
  labs(
    x = "Percent Water",
    y = "Interdecile Range"
  ) +
  theme_classic()

idrPlot <- ggarrange(p1, p2, p3, p4, p5, p6, p7,p8)
idrPlot

# Modelling IDR--------------------------------------------------------------

model <- multilevel %>% 
  select(idr, total_area, perc_water, elevation_range, streamflow, precip, water_use, 
         lat, popdensity, medincome)


shapiro.test(model$idr) #not normally distributed
shapiro.test(sqrt(model$idr))

all_model <- lm(idr ~ total_area + elevation_range + perc_water +
                   streamflow + precip + lat + popdensity + medincome,
                 data = model)

summary(all_model)
# Choosing best predictors ------------------------------------------------
Best_Subset <- regsubsets(sqrt(idr) ~ total_area + elevation_range + perc_water +
                          streamflow + precip + lat + popdensity + medincome,
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
# okay, leaps suggests we drop elevation_range, precip, and lat 

best_model <- lm(sqrt(idr) ~ total_area + water_use + 
                streamflow + popdensity + medincome,
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
