# Let's see what multi-level regression has to say about oxygen and hydrogen values
library(plm)
#okay going through the covariates data I do have a question: what the heck is "Hawaii" as a city?

#Okay first what do we want from tapData
multilevel <- tapData %>% 
  select(d2H, d18O, County, Year, Season, Month, Cluster_ID, Elevation_mabsl, Cluster_Location) %>% 
  rename(NAME = County, City = Cluster_Location) %>% 
  subset(City != "NC")

multilevel <- left_join(multilevel, covariates, by = "NAME")

M1 <- lm(cbind(d2H, d18O) ~ Elevation_mabsl + ALAND + AWATER + MEDINCOME + POPDENSITY_SQKM, data = multilevel)
M2 <- lm(d2H ~ Elevation_mabsl + ALAND + AWATER + MEDINCOME + POPDENSITY_SQKM,data = multilevel)

M3 <- plm(d2H ~ Elevation_mabsl + ALAND + AWATER + MEDINCOME + POPDENSITY_SQKM, 
                    data = multilevel,
                    index = c("City"), 
                    model = "within")
#okay trying to handle fixed effects is clearly a work in progress...

require(car)
summary(M1)
summary(Anova(M1))
# We're getting an error when we try to run a linear model with d2H and d18O as covariables. That's likely because
# they're simply too strongly correlated
vcov(M1)

ggplot(data = multilevel, aes(x = d2H, y = d18O, color = MEDINCOME)) +
  geom_point()

ggplot(data = multilevel, aes(x = d2H, y = MEDINCOME, color = NAME)) +
  geom_point() + 
  theme(
    legend.position = "NONE"
  )
