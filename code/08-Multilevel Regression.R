library(tidyr); library(dplyr); library(leaps); library(ggpubr); library(car)                      

tapData <- read.csv("data/cityWater.csv") 
tapData <- subset(tapData, cluster_location != "Oahu" & cluster_location != "Hawaii")
datasummary <- read.csv("data/datasummary.csv")
datasummary <- datasummary[,-c(1, 3:5, 7:14, 17, 18)]
multivariate <- read.csv("data/multivariate.csv")
multilevel <- left_join(multivariate, datasummary, by = 'cluster_location') %>% 
  rename('sd' = 'd18O_sd', 
         'idr' = 'IDR_O')

# Modelling IDR--------------------------------------------------------------

model <- multilevel %>% 
  dplyr::select(idr, streamflow, precip, 
         lat, lon, medincome, water_use, ruggedness, popdensity)

model$water_use = multilevel$water_use / multilevel$pop * 1e6 * 3.78
model$popdensity = log(model$popdensity)

vn = c("Streamflow (units)", "Precipitation (mm)", "Latitude", "Longitude",
       "Median income (USD)", "Water use (liters/person/day)", "Ruggedness (units)",
       "Population density (ln[people/sq km])")

png("figures/Fig5.png", width = 8.2, height = 9.2, units = "in", res = 600)

layout(matrix(1:9, nrow = 3, byrow = TRUE), widths = c(lcm(3.0 * 2.54), lcm(2.5 * 2.54), lcm(2.5 * 2.54)),
       heights = rep(lcm(3 * 2.54), 3))

for(i in 2:ncol(model)){
  if(i %in% c(2, 5, 8)){
    par(mai = c(0.6, 0.6, 0.1, 0.1))
  }else{
    par(mai = c(0.6, 0.1, 0.1, 0.1))
  }
  
  if(i == 5){
    plot(model[, i], sqrt(model$idr), xlab = vn[i-1], 
         ylab = expression("Sqrt("*delta^{18}*"O IDR)"),
         pch = 21, bg = "light grey", cex = 1.75)
  }else if(i %in% c(2, 8)){
    plot(model[, i], sqrt(model$idr), xlab = vn[i-1], ylab = "",
         pch = 21, bg = "light grey", cex = 1.75)
  }else{
    plot(model[, i], sqrt(model$idr), xlab = vn[i-1], axes = FALSE, 
         ylab = "", pch = 21, bg = "light grey", cex = 1.75)
    axis(1)
    axis(2, labels = FALSE)
    box()
  }
  l = lm(sqrt(model$idr) ~ model[, i])
  abline(l)
  s = signif(summary(l)$coefficients[2], 2)
  r2 = round(summary(l)$adj.r.squared, 2)
  p = signif(summary(l)$coefficients[2, 4], 1)
  pt = paste("Slope:", s, "\nAdj R2:", r2, "\np:", p)
  text(min(model[, i]) + diff(range(model[, i])) * 0.8, 2.6, pt)
}
dev.off()

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

# compromise model, 3 covariates:
best_model <- lm(sqrt(idr) ~ streamflow + lon + medincome,
                 data = model)

summary(best_model)

# Checking variance inflation factors for these independent variables we've got highly correlated variables 
vif(best_model) 

# Compare 4-covariate model 
alt_model <- lm(sqrt(idr) ~ streamflow + lon + medincome + water_use,
                 data = model)

summary(alt_model)
vif(alt_model) 
# Also not bad, but minimal gain in predictive power for the added variable