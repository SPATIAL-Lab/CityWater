# Let's see what multi-level regression has to say about oxygen and d_excess values
# Run G8 before this
library(plm);library(caret);library(leaps);library(MASS);library(HLMdiag)

tapData <- read.csv("data/cityWater.csv")
tapData <- subset(tapData, Cluster_Location != "Oahu" & Cluster_Location != "Hawaii")
#Okay first what do we want from tapData

df <- group_by(tapData, Cluster_Location) %>% 
  summarize(var(rep(d18O)))
multilevel <- left_join(multivariate, df, by = "Cluster_Location")
multilevel$variance <- multilevel$'var(rep(d18O))'

#huge variation in values ALAND and AWATER, so doing a log transformation to normalize the data. 
multilevel$arealog <- log(multilevel$total_area)
multilevel$waterlog <- log(multilevel$total_water)


##################
# Summarized Data
##################
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

ggarrange(p1, p2, p3, p4, p5, p6, p7, p8)




###############
# All tap data
###############
multilevel <- left_join(tapData, multivariate, by = "Cluster_Location")
multilevel <- subset(multilevel, !is.na(precip))
#huge variation in values ALAND and AWATER, so doing a log transformation to normalize the data. 
multilevel$landlog <- log(multilevel$total_area)
multilevel$waterlog <- log(multilevel$total_water)
multilevel <- multilevel %>% 
  select(d18O, d_ex, landlog, waterlog, elevation_range, streamflow, precip, 
         popdensity, medincome)

multilevel <- subset(multilevel, !is.na(precip))
#huge variation in values ALAND and AWATER, so doing a log transformation to normalize the data. 
multilevel$landlog <- log(multilevel$total_area)
multilevel$waterlog <- log(multilevel$total_water)
multilevel <- multilevel %>% 
  select(d18O, d_ex, landlog, waterlog, elevation_range, streamflow, precip, 
         popdensity, medincome)

shapiro.test(multilevel$d18O)
shapiro.test(multilevel$d_ex)
# neither of our dependent variables are normally distributed. 
lm1 <- lm(multilevel,formula = cbind(d18O, d_ex) ~.)
summary(lm1)





cor.test(multilevel$elevation_range, multilevel$landlog)
cor(multilevel[, c('elevation_range', "streamflow", "precip", "landlog", "waterlog", "popdensity")])

summary(manova(cbind(d_ex, d18O) ~ elevation + streamflow + precip + waterlog + medincome,
               data = multilevel))

multilevel2 <- subset(multilevel, Cluster_Location != "Salt Lake City")

shapiro.test(multilevel2$d18O)
shapiro.test(multilevel2$d_ex)

M1 <- lm(cbind(d_ex, d18O) ~ elevation + streamflow + precip + waterlog + medincome,
         data = multilevel)

#Is Salt Lake City simply too many samples from one area
M2 <- lm(cbind(d_ex, d18O) ~ elevation + streamflow + precip + waterlog + medincome,
           popdensity, data = multilevel2)

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
# suggestin that these are pretty important factors actually


# stepwise to help deal with the linearly dependant columns? 
# Fit the full model 
full.model <- lm(d18O ~ elevation + streamflow + precip + landlog + waterlog + medincome +
                   popdensity, data = multilevel)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)



### God help us, let's plot the locations and each of these variables
elevation <- ggplot(data = multilevel, 
                    aes(x = d18O, y = elevation)) + 
  geom_point()+ 
  theme_classic()



