# Let's see what multi-level regression has to say about oxygen and d_excess values
# Run G8 before this
library(plm);library(caret);library(leaps);library(MASS);library(HLMdiag)

tapData <- read.csv("data/cityWater.csv", na.strings = "NA")
tapData <- subset(tapData, Cluster_Location != "Oahu" & Cluster_Location != "Hawaii")
#Okay first what do we want from tapData

df <- group_by(tapData, Cluster_Location) %>% 
  summarize(var(rep(d18O)))
multilevel <- left_join()

#huge variation in values ALAND and AWATER, so doing a log transformation to normalize the data. 
multilevel$landlog <- log(multilevel$total_area)
multilevel$waterlog <- log(multilevel$total_water)

multilevel <- subset(multilevel, !is.na(precip))
shapiro.test(multilevel$d18O)
shapiro.test(multilevel$d_ex)
cor.test(multilevel$popdensity, multilevel$landlog)
cor(multilevel[, c('elevation', "streamflow", "precip", "landlog", "waterlog", "popdensity")])

summary(manova(cbind(d_ex, d18O) ~ elevation + streamflow + precip + waterlog + medincome,
               data = multilevel2))

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
summary(M1)
summary(Anova(M1))
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



