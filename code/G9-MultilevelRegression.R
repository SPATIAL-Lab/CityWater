# Let's see what multi-level regression has to say about oxygen and d_excess values
library(tidyverse); library(leaps)

tapData <- read.csv("data/cityWater.csv") 
tapData <- subset(tapData, Cluster_Location != "Oahu" & Cluster_Location != "Hawaii")
datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(1, 3:14, 17, 18)]
multivariate <- read.csv("data/multivariate.csv")
#Okay first what do we want from tapData

df <- group_by(tapData, Cluster_Location) %>% 
  summarize(variance = var(rep(d18O)),
            max = max(d18O), 
            min = min(d18O))
df$range <- df$max - df$min

multilevel <- left_join(multivariate, df, by = "Cluster_Location")
multilevel <- left_join(multilevel, datasummary, by = 'Cluster_Location')

#huge variation in values ALAND and AWATER, so doing a log transformation to normalize the data. 
multilevel$landlog <- log(multilevel$total_area)
multilevel$waterlog <- log(multilevel$total_water)

# Summarized Data ---------------------------------------------------------

# looking at d18O variance and relationship to different variables

p1 <- ggplot(data = multilevel, aes(x = streamflow, y = variance)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() +                                     
  theme_classic()

p2 <- ggplot(data = multilevel, aes(x = precip, y = variance)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() +                                     
  theme_classic()

p3 <- ggplot(data = multilevel, aes(x = elevation_range, y = variance)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() +                                     
  theme_classic()

p4 <- ggplot(data = multilevel, aes(x = total_land, y = variance)) +
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() +                                     
  theme_classic()

p5 <- ggplot(data = multilevel, aes(x = total_water, y = variance)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() +                                     
  theme_classic()

p6 <- ggplot(data = multilevel, aes(x = total_area, y = variance)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() +                                     
  theme_classic()

p7 <- ggplot(data = multilevel, aes(x = popdensity, y = variance)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() +                                     
  theme_classic()

p8 <- ggplot(data = multilevel, aes(x = medincome, y = variance)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() +                                     
  theme_classic()

variancePlot <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8)
variancePlot

# now let's compared d18O range to different variables

p9 <- ggplot(data = multilevel, aes(x = streamflow, y = range)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() +  
  labs(
    x = "Streamflow (total)",
    y = "Range"
  ) +
  theme_classic()


p10 <- ggplot(data = multilevel, aes(x = precip, y = range)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() + 
  labs(
    x = "Precipitation (mean)",
    y = ""
  ) +
  theme_classic()

p11 <- ggplot(data = multilevel, aes(x = elevation_range, y = range)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() +  
  labs(
    x = "Elevation (range)",
    y = ""
  ) +
  theme_classic()

p12 <- ggplot(data = multilevel, aes(x = total_land, y = range)) +
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() + 
  labs(
    x = "Total Land (sq m)",
    y = "Range"
  ) +
  theme_classic()

p13 <- ggplot(data = multilevel, aes(x = total_water, y = range)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() +
  labs(
    x = "Total water (sq m)",
    y = ""
  ) +
  theme_classic()

p14 <- ggplot(data = multilevel, aes(x = total_area, y = range)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() +
  labs(
    x = "Total area (sq m)",
    y = ""
  ) +
  theme_classic()

p15 <- ggplot(data = multilevel, aes(x = popdensity, y = range)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() + 
  labs(
    x = "Population Density",
    y = "Range"
  ) +
  theme_classic()

p16 <- ggplot(data = multilevel, aes(x = medincome, y = range)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 12, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 10, aes(label = ..rr.label..)) +
  geom_point() + 
  labs(
    x = "Median Income",
    y = ""
  ) +
  theme_classic()

rangePlot <- ggarrange(p9, p10, p11, p12, p13, p14, p15, p16)
rangePlot

ggplot(data = df, aes(x = range, y = variance, label = Cluster_Location)) + 
  geom_point() + 
 # geom_text(nudge_y = -0.5) + 
  theme_classic()
multilevel <- subset(multilevel, Cluster_Location != "San Francisco")


# Modelling ---------------------------------------------------------------

model <- left_join(tapData, multilevel, by = "Cluster_Location")

model <- model %>% 
  select(d18O, d_ex, landlog, waterlog, elevation_range, streamflow, precip, 
         Lat, popdensity, medincome, Elevation)

shapiro.test(model$d18O)
shapiro.test(model$d_ex)
# neither of our dependent variables are normally distributed. 

lm1 <- lm(model,formula = d18O ~.)
summary(lm1)


cor.test(model$elevation_range, model$landlog)
cor(model[, c('elevation_range', "streamflow", "precip", "landlog", "waterlog", "popdensity", "medincome")])

summary(manova(cbind(d_ex, d18O) ~ elevation_range + streamflow + precip + waterlog + medincome,
               data = model))

M1 <- lm(cbind(d_ex, d18O) ~ .,
         data = model)
summary(M1)

# To absolutely no one's surprise, the major factors in d_ex values are: elevation, streamflow, precipitation, 
# and amount of water. The median income and population density is a slight surprise, but that's probably just because it's correlated with 
# water in the area. 

ggplot(data = model2, aes(x = medincome, y = d_ex)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 20, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 18, aes(label = ..rr.label..)) +
  geom_point() + 
  labs(
    x = "Median Income",
    y = "d-excess"
  ) +
  theme_classic()

ggplot(data = model2, aes(x = popdensity, y = d_ex)) + 
  stat_smooth(method = "lm", formula= y~x) +
  stat_regline_equation(label.y = 20, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 18, aes(label = ..rr.label..)) +
  geom_point() + 
  labs(
    x = "Population Density",
    y = "d-excess"
  ) +
  theme_classic()

cor(model[, c('lat', 'elevation_range', "streamflow", "precip", "landlog", "waterlog", "popdensity", "medincome")])


# Choosing best predictors ------------------------------------------------
Best_Subset <- regsubsets(d18O ~ landlog + waterlog + elevation_range + streamflow + 
                            precip + Lat + popdensity + medincome,
                          data = model,
                          nbest = 1,      # 1 best model for each number of predictors
                          nvmax = NULL,    # NULL for no limit on number of variables
                          force.in = NULL,
                          force.out = NULL,
                          method = "exhaustive")
summary_best_subset <- summary(Best_Subset)
as.data.frame(summary_best_subset$outmat)

summary_best_subset$which[which.max(summary_best_subset$adjr2),]

best.model <- lm(d18O ~ landlog + waterlog + elevation_range + streamflow + 
                   precip + Lat + popdensity + medincome, data = model)
summary(best.model)
# okay, leaps loves all the predictor variables I threw at it. 
Best_Subset <- regsubsets(d_ex ~ landlog + waterlog + elevation_range + streamflow + 
                            precip + Lat + popdensity + medincome,
                          data = model,
                          nbest = 1,      # 1 best model for each number of predictors
                          nvmax = NULL,    # NULL for no limit on number of variables
                          force.in = NULL,
                          force.out = NULL,
                          method = "exhaustive")
summary_best_subset <- summary(Best_Subset)
as.data.frame(summary_best_subset$outmat)

summary_best_subset$which[which.max(summary_best_subset$adjr2),]

best.model <- lm(d_ex ~ landlog + elevation_range + streamflow + 
                   precip + Lat + popdensity, data = model)
summary(best.model)
#compare to all predictors, there's only a difference of 0.0004 in R2
summary(lm(d_ex ~ landlog + waterlog + elevation_range + streamflow + 
             precip + Lat + popdensity + medincome, data = model))
# Code Graveyard, Ignore --------------------------------------------------

#fixed effects modelling. currently broken

M2 <- plm(d18O ~ elevation + streamflow + precip + landlog + waterlog + medincome +
            popdensity,
                    data = multilevel,
                    index = Cluster_Location, 
                    model = "within")

require(car)
summary(M2)
summary(Anova(M2))
residM1 <- resid(M1)
#produce residual vs. fitted plot
plot(fitted(M1), residM1)

#add a horizontal line at 0 
abline(0,0)
#create Q-Q plot for residuals
qqnorm(residM1)

#add a straight diagonal line to the plot
qqline(residM1) 

#Create density plot of residuals
plot(density(residM1))


summary(M2)
summary(Anova(M2))
residM2 <- resid(M2)
#produce residual vs. fitted plot
plot(fitted(M2), residM2)
#add a horizontal line at 0 
abline(0,0)
#create Q-Q plot for residuals
qqnorm(residM2)

#add a straight diagonal line to the plot
qqline(residM2) 

#Create density plot of residuals
plot(density(residM2))

M3 <- update(M1, . ~ . - medincome - popdensity)
anova(M1, M3)
# Okay so removing median income and pop density gives us a significantly different model
# suggesting that these are pretty important factors actually


# stepwise to help deal with the linearly dependant columns? 
# Fit the full model 
full.model <- lm(cbind(d_ex, d18O) ~ elevation_range + streamflow + precip + waterlog + medincome + popdensity,
                 data = model)
library(MASS)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)


ggplot() + 
  geom_point(data = tapData, aes(x = d18O, y = d2H, color = Elevation)) + 
  theme_classic()



