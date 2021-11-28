# Load packages and data
library(dplyr)
library(ggplot2)
library(kableExtra)
library(ggpubr)
library(car)
library(olsrr)
load(file = "foresthealth.rda")

# Basic pre-processing
data <- dat
summary(data)
data$tree_sp_eu <- factor(data$tree_sp_eu)
data$id <- factor(data$id)

# Create data set for Beech
data_beech <- filter(data, tree_sp_eu == "Rbu") # makes dataset for beech trees
data_beech <- subset(data_beech, select = -c(H_spec, id, x_utm, y_utm, s_veg, prec_y, spei_12_oct, tree_sp_eu))
data_beech <- na.omit(data_beech) # removing all the NA values

# Model selection process
full_mod <- lm(nbv_ratio ~ ., data = data_beech)
aic_mod <- step(full_mod)
bic_mod <- step(full_mod, k = log(nrow(data_beech)))
anova(aic_mod, bic_mod)

# Check/add other variables of interest
bic_add_spei24 <- update(bic_mod, . ~ . + spei_24_oct, data = data_beech)
anova(bic_mod, bic_add_spei24) # p-value of 0.009143 < 1%

spei24_add_Es <- update(bic_add_spei24, . ~ . + Es, data = data_beech)
anova(add_spei24, spei24_add_Es) # p-value of 0.01519 > 1%

# Check for collinearity using VIF
ols_vif_tol(bic_add_spei24) # VIF of alt_m & tmean_y >6
cor(data_beech$alt_m, data_beech$tmean_y) # correlation of -0.8259334

rm_alt_m <- update(bic_add_spei24, . ~ . - alt_m, data = data_beech)
ols_vif_tol(rm_alt_m) # all VIFs <5
anova(rm_alt_m, bic_add_spei24) # p-value of 0.002198 < 1%

rm_tmean_y <- update(bic_add_spei24, . ~ . - tmean_y, data = data_beech)
ols_vif_tol(rm_tmean_y) # all VIFs <5
anova(rm_tmean_y, bic_add_spei24) # p-value of 0.0006639 < 1%

# Final model
model_final <- rm_alt_m

# Check model assumptions
par(mfrow = c(2,2))
plot(model_final)








