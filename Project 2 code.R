# Loading in and organising the data
library(dplyr)

data <- dat
summary(data)
data$tree_sp_eu <- factor(data$tree_sp_eu)
data$id <- factor(data$id)


#Omitting unnecessary data
data_omit <- data[-c(20)]    #removing the Es column
data_omit <- data_omit[-c(1)] #removes id
data_omit <- data_omit[-c(28)] #removing the s_veg column
data_omit <- na.omit(data_omit) #removing all the NA values
summary(data_omit)
data_omit_beech <- filter(data_omit, tree_sp_eu == "Rbu") 
data_omit_beech <- data_omit_beech[-c(4)]
data_omit_spruce <- filter(data_omit, tree_sp_eu == "Gfi")
data_omit_spruce <- data_omit_spruce[-c(4)]


# Analysis of data for Beech trees
mod1 <- lm(nbv_ratio ~ ., data = data_omit_beech) #Model with all variables
summary(mod1)

drop_mod1 <- drop1(mod1, test = "F")        #Backward selection using drop
step_mod1 <- step(mod1)                     #Backward selection using step

par(mfrow = c(2,2))
plot(step_mod1)    #plots the graphs to check model assumptions, Q-Q plots are odd at the end tail, residuals has a weird shape
#there are two points that have a leverage of 1, could be outliers (1590, 2422)
#Could imply that this model does not fit very well

mod2 <- lm(nbv_ratio ~ . -globrad_y_lag1 -prec_y_lag1 -et0_y_lag1, data = data_omit_beech)
summary(mod2)
step_mod2 <- step(mod2)

mod3 <- lm(nbv_ratio ~ . -spei_24_oct, data = data_omit_beech)
summary(mod3)
step_mod3 <- step(mod3)

mod4 <- lm(nbv_ratio ~ . -spei_24_oct -globrad_y_lag1 -prec_y_lag1 -et0_y_lag1, data = data_omit_beech)
summary(mod4)
step_mod4 <- step(mod4)

mod5 <- lm(nbv_ratio ~ . -spei_12_oct -globrad_y -prec_y -et0_y, data = data_omit_beech)
summary(mod5)
step_mod5 <- step(mod5)

mod6 <- lm(nbv_ratio ~ . -spei_12_oct -globrad_y_lag1 -prec_y_lag1 -et0_y_lag1, data = data_omit_beech)
summary(mod6)
step_mod6 <- step(mod6)

mod7 <- lm(nbv_ratio ~ . -spei_12_oct, data = data_omit_beech)
summary(mod7)
step_mod7 <- step(mod7)

#Step mod 1 and 3 have the best fit by far.



lm_site <- lm(nbv_ratio ~ geol_no + soil_no + soil_ty_no + humus_no + water_no + nutri_no + slope_dir + skel_no + depth_no + alt_m + slope_deg, data = data_omit_beech)
summary(lm_site)  #Model using only site variables












