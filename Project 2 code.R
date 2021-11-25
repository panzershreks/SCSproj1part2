# Loading in and organising the data
library(dplyr)

data <- dat
summary(data)
data$tree_sp_eu <- factor(data$tree_sp_eu)
data$id <- factor(data$id)


#Omitting unnecessary data
data_omit <- data[-c(20)]    #removing the Es column
data_omit <- data_omit[-c(26:28)] #removes the radiation, precipitation and evapotranspiration of previous year
data_omit <- data_omit[-c(29)] #removes the SPEI for the 24-month period
#data_omit <- data_omit[-c(28)] #removes the SPEI for the 12-month period
data_omit <- data_omit[-c(1)] #removes id
data_omit <- na.omit(data_omit)
summary(data_omit)
data_omit_beech <- filter(data_omit, tree_sp_eu == "Rbu")
data_omit_beech <- data_omit_beech[-c(4)]
data_omit_spruce <- filter(data_omit, tree_sp_eu == "Gfi")
data_omit_spruce <- data_omit_beech[-c(4)]


# Analysis of data for Beech trees
mod1 <- lm(nbv_ratio ~ ., data = data_omit_beech) #Model with all variables
summary(mod1)

drop_mod1 <- drop1(mod1, test = "F")        #Backward selection using drop
step_mod1 <- step(mod1)                     #Backward selection using step

lm_site <- lm(nbv_ratio ~ geol_no + soil_no + soil_ty_no + humus_no + water_no + nutri_no + slope_dir + skel_no + depth_no + alt_m + slope_deg, data = data_omit_beech)
summary(lm_site)

par(mfrow = c(2,2))
plot(step_mod1)    #plots the graphs to check model assumptions, Q-Q plots are odd at the end tail, residuals has a weird shape
#there are two points that have a leverage of 1, could be outliers (1590, 2422)




