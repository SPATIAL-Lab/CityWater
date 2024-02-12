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
  select(idr, lat, lon, ruggedness, streamflow, precip, 
                popdensity, medincome, water_use)

model$water_use = multilevel$water_use / multilevel$pop * 1e6 * 3.78

model$streamflow = log(model$streamflow)
model$water_use = log(model$water_use)
model$popdensity = log(model$popdensity)

vn = c(expression("Latitude"), expression("Longitude"), expression("Ruggedness (m)"), 
       expression("Streamflow (ln[km"^2*"/year])"), expression("Precipitation (mm)"),
       expression("Population density (ln[people/km"^2*"])"), 
       expression("Median income (USD)"), expression("Water use (ln[L/person/day])"))


# Figure 4 ----------------------------------------------------------------

png("figures/Fig4.png", width = 8.2, height = 9.2, units = "in", res = 600)

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
  r2 = round(summary(l)$r.squared, 2)
  p = signif(summary(l)$coefficients[2, 4], 2)
  text(min(model[, i]) + diff(range(model[, i])) * 0.8, 2.6, bquote("Slope: "*.(s)),
       adj = c(0.5, 0))
  text(min(model[, i]) + diff(range(model[, i])) * 0.8, 2.45, bquote("R"^2*": "*.(r2)),
       adj = c(0.5, 0))
  text(min(model[, i]) + diff(range(model[, i])) * 0.8, 2.25, bquote("p: "*.(p)),
       adj = c(0.5, 0))
}
dev.off()


# All Model ---------------------------------------------------------------

all_model <- lm(sqrt(idr) ~ .,
                 data = model)

summary(all_model)
vif(all_model)
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

# best model, 3 covariates:
best_model <- lm(sqrt(idr) ~ streamflow + lon + medincome,
                 data = model)

summary(best_model)
plot(density(best_model$residuals))
shapiro.test(best_model$residuals)
plot(best_model)

# Checking variance inflation factors for these independent variables we've got highly correlated variables 
vif(best_model) 
