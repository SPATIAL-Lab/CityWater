# Let's see what multi-level regression has to say about oxygen and hydrogen values
library(plm)
library(caret)
library(leaps)
library(MASS)
#okay going through the covariates data I do have a question: what the heck is "Hawaii" as a city?

# Let's create d-excess for tapData
regressData$d_ex <- (regressData$d2H - 8 * regressData$d18O)

#Okay first what do we want from tapData
regressData <- regressData %>% 
  rename(NAME = County, City = Cluster_Location) 


multilevel <- left_join(regressData, covariates, by = "NAME")
#huge variation in values ALAND and AWATER, so doing a log transformation to normalize the data. 
multilevel$ALANDlog <- log(multilevel$ALAND)
multilevel$AWATERlog <- log(multilevel$AWATER)



M1 <- lm(cbind(d_ex, d18O) ~ Elevation_mabsl + ALANDlog + AWATERlog + MEDINCOME + POPDENSITY_SQKM, data = multilevel)
M2 <- lm(d18O ~ Elevation_mabsl + ALAND + AWATER + MEDINCOME + POPDENSITY_SQKM,data = multilevel)

#fixed effects modelling 
M3 <- plm(d18O ~ Elevation_mabsl + ALANDlog + AWATERlog + MEDINCOME + POPDENSITY_SQKM, 
                    data = multilevel,
                    index = c("City"), 
                    model = "within")



require(car)
summary(M3)
summary(Anova(M1))



ggplot(data = multilevel, aes(x = d_ex, y = d18O, color = MEDINCOME)) +
  geom_point()

ggplot(data = multilevel, aes(x = d2H, y = MEDINCOME, color = NAME)) +
  geom_point() + 
  theme(
    legend.position = "NONE"
  )

# stepwise to help deal with the linearly dependant columns? 
# Fit the full model 
full.model <- lm(d18O ~ Elevation_mabsl + ALAND + AWATER +
                   MEDINCOME + POPDENSITY_SQKM, data = multilevel)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)




